
# script_6_Ranking_analysis


# Load Necessary Packages ------------------------------------------------------

library(tidyverse)
library(flextable)
library(officer)
library(xtable)
library(here)
library(readxl)


#set working directory
Sys.setenv(LANG = "en")
options(scipen = 999)

stopifnot(file.exists(here::here("Qual_data","Raw",
                                 "PeopleSun User Research_sentiment analysis.xlsx")))

out_dir <- here::here("Output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)



# Load data ---------------------------------------------------------------

upv_items <- read_excel(here::here("Qual_data","Raw",
                                   "PeopleSun User Research_sentiment analysis.xlsx"),
                        guess_max = 20000)

# The Dataverse file is at extract level (several text extracts per paragraph).
# The ranking works at paragraph level, so reduce to one row per paragraph.
upv_items <- upv_items %>%
  distinct(`Paragraph ID`, .keep_all = TRUE)

message("Rows after reducing to paragraph level: ", nrow(upv_items))


upv_items <- upv_items %>%
  mutate(
    Gender = case_when(
      tolower(as.character(Gender)) %in% c("female", "woman", "women") ~ "Women",
      tolower(as.character(Gender)) %in% c("male", "man", "men")       ~ "Men",
      TRUE ~ as.character(Gender)
    ),
    `Group Type` = case_when(
      tolower(as.character(`Group Type`)) %in% c("female", "woman", "women") ~ "Women",
      tolower(as.character(`Group Type`)) %in% c("male", "man", "men")       ~ "Men",
      tolower(as.character(`Group Type`)) %in% c("mixed", "mix")             ~ "Mixed",
      TRUE ~ as.character(`Group Type`)
    )
  )

upv_items$`Paragraph Number` <- as.integer(upv_items$`Paragraph Number`)


# Prepare data ---------------------------------------------------

upv_items$item_worth <- NA_real_


for (i in 1:length(upv_items$item_worth))
{if(upv_items$`Paragraph Number`[i] == 0) upv_items$item_worth[i] <- 1
if(upv_items$`Paragraph Number`[i] == 1) upv_items$item_worth[i] <- 0.8
if(upv_items$`Paragraph Number`[i] == 2) upv_items$item_worth[i] <- 0.6
if(upv_items$`Paragraph Number`[i] == 3) upv_items$item_worth[i] <- 0.4
if(upv_items$`Paragraph Number`[i] == 4) upv_items$item_worth[i] <- 0.2
if(upv_items$`Paragraph Number`[i] == 5) upv_items$item_worth[i] <- 1
if(upv_items$`Paragraph Number`[i] == 6) upv_items$item_worth[i] <- 0.8
if(upv_items$`Paragraph Number`[i] == 7) upv_items$item_worth[i] <- 0.6
if(upv_items$`Paragraph Number`[i] == 8) upv_items$item_worth[i] <- 0.4
if(upv_items$`Paragraph Number`[i] == 9) upv_items$item_worth[i] <- 0.2
}




# Gendered individual item ranks ---------------------------------------------------

upv_indiv <- filter(upv_items, `Survey Type` == "Individual")



aggregated_items <- aggregate(
  item_worth ~ `Item Name` + Gender,
  data = upv_indiv,
  FUN  = function(x) sum(x, na.rm = TRUE)
)


sorted_items <- aggregated_items[order(-aggregated_items$item_worth, 
                                       aggregated_items$Gender), ]



wide_data <- reshape(sorted_items, 
                     timevar = "Gender", 
                     idvar = "Item Name", 
                     direction = "wide")

if (!"item_worth.Women" %in% names(wide_data)) wide_data$item_worth.Women <- 0
if (!"item_worth.Men"   %in% names(wide_data)) wide_data$item_worth.Men   <- 0

wide_data_sorted_by_female <- wide_data[order(-wide_data$item_worth.Women), ]

#calculate ranks
wide_data_sorted_by_female <- wide_data_sorted_by_female %>%
  mutate(
    Rank.Women = ifelse(item_worth.Women > 0,
                         dense_rank(desc(item_worth.Women)),
                         NA_integer_),
    Rank.Men   = ifelse(item_worth.Men   > 0,
                         dense_rank(desc(item_worth.Men)),
                         NA_integer_)
  ) %>%
  
  select(`Item Name`,
         item_worth.Women, Rank.Women,
         item_worth.Men,   Rank.Men)

# table
ft <- flextable(wide_data_sorted_by_female) %>%
  set_header_labels(
    `Item Name`       = "Item name",
    item_worth.Women = "Item worth (W)",
    Rank.Women       = "Rank (W)",
    item_worth.Men   = "Item worth (M)",
    Rank.Men         = "Rank (M)"
  ) |>
  colformat_int(j = c("Rank.Women","Rank.Men"), na_str = "NA") |>
  autofit()

print(ft)

# into word
doc <- read_docx() |> body_add_flextable(ft)
print(doc, target = file.path(out_dir, "individual_item_ranks.docx"))

#latex


df_tex <- wide_data_sorted_by_female


df_tex$Rank.Women[is.na(df_tex$Rank.Women)] <- ""
df_tex$Rank.Men  [is.na(df_tex$Rank.Men)]   <- ""
df_tex$item_worth.Women[is.na(df_tex$item_worth.Women)] <- NA
df_tex$item_worth.Men  [is.na(df_tex$item_worth.Men)]   <- NA


df_out <- data.frame(
  Item = df_tex$`Item Name`,
  W_worth = ifelse(is.na(df_tex$item_worth.Women), "", sprintf("%.2f", df_tex$item_worth.Women)),
  W_rank  = as.character(df_tex$Rank.Women),
  M_worth = ifelse(is.na(df_tex$item_worth.Men),   "", sprintf("%.2f", df_tex$item_worth.Men)),
  M_rank  = as.character(df_tex$Rank.Men),
  stringsAsFactors = FALSE
)

# xtable
xt <- xtable(df_out, align = c("l","l","r","r","r","r"))

body_lines <- capture.output(print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,
  only.contents = TRUE
))

# longtable
header_lines <- c(
  "\\begin{longtable}{lrrrr}",
  "\\caption{Gendered individual item ranks}\\label{sup:tab:genranks_indiv}\\\\",
  "\\hline",
  "\\textbf{Item name} & \\multicolumn{2}{c}{\\textbf{Women}} & \\multicolumn{2}{c}{\\textbf{Men}} \\\\",
  "& \\textbf{Item worth} & \\textbf{Rank} & \\textbf{Item worth} & \\textbf{Rank} \\\\",
  "\\hline",
  "\\endfirsthead",
  "\\hline",
  "\\textbf{Item name} & \\multicolumn{2}{c}{\\textbf{Women}} & \\multicolumn{2}{c}{\\textbf{Men}} \\\\",
  "& \\textbf{Item worth} & \\textbf{Rank} & \\textbf{Item worth} & \\textbf{Rank} \\\\",
  "\\hline",
  "\\endhead",
  "\\endfoot",
  "\\hline",
  "\\endlastfoot"
)

out_lines <- c(header_lines, body_lines, "\\end{longtable}")
writeLines(out_lines, file.path(out_dir, "gendered_indiv_ranks.tex"))



# Graphic gendered individual item ranks ---------------------------------------------------


# Select relevant appliances
devices_keep <- c("Mobile Phone","Fan","TV","Iron","Fridge",
                  "Light Bulb","Blender","Radio","Computer",
                  "Torch","Sewing Machine")

# All Items in Long-Form + global rank
long_all <- wide_data_sorted_by_female %>% 
  pivot_longer(starts_with("item_worth."),
               names_to  = "Gender",           
               values_to = "Worth") %>% 
  mutate(Gender = str_remove(Gender, "item_worth\\.")) %>% 
  group_by(Gender) %>% 
  arrange(desc(Worth), .by_group = TRUE) %>% 
  mutate(Rank_global = ifelse(is.na(Worth) | Worth == 0, NA_integer_, dplyr::dense_rank(dplyr::desc(Worth)))) %>%   #dense rank
  ungroup()

# selected appliances + rank by female worth
plot_data <- long_all %>% 
  filter(`Item Name` %in% devices_keep) %>% 
  mutate(
    Item_F = factor(`Item Name`,
                    levels = long_all %>% 
                      filter(Gender == "Women",
                             `Item Name` %in% devices_keep) %>% 
                      arrange(desc(Worth)) %>% 
                      pull(`Item Name`)),
    Gender = factor(Gender, levels = c("Women","Men"))
  )

dodge <- position_dodge2(width = 0.8, reverse = TRUE) 

# plot
indiv_item_plot <- ggplot(plot_data,
                          aes(x = Item_F, y = Worth, fill = Gender)) +
  geom_col(position = dodge, width = 0.7) +
  geom_text(aes(label  = paste0("#", Rank_global),
                colour = Gender),
            position  = dodge,
            vjust     = -0.6,
            size      = 3.5,
            show.legend = FALSE) +
  # colours + legend
  scale_fill_manual(values  = c(Women = "#55C667FF", Men = "#481567"),
                    labels  = c(Women = "Women",    Men = "Men")) +
  scale_colour_manual(values = c(Women = "#55C667FF", Men = "#481567"),
                      labels = c(Women = "Women",    Men = "Men")) +
  scale_x_discrete(limits = rev(levels(plot_data$Item_F))) +
  coord_flip() +
  labs(y = "Item worth", x = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(indiv_item_plot)

ggsave(file.path(out_dir, "indiv_item_plot.jpg"), 
       plot = indiv_item_plot, width = 8, height = 6, dpi = 300)


# Group item ranks --------------------------------------------------------

# filter for group
upv_group <- upv_items[ upv_items$`Survey Type` == "Group", ]

# caluclate item worth per group
aggregated_items_group <- aggregate(item_worth ~ `Item Name` + `Group Type`,
                                    data = upv_group,
                                    FUN   = sum, na.rm = TRUE)

# sort for worth
aggregated_items_group <- aggregated_items_group[
  order(-aggregated_items_group$item_worth,
        aggregated_items_group$`Group Type`), ]

# wide: Women / Men / Mixed
wide_data_group <- reshape(aggregated_items_group,
                           timevar   = "Group Type",
                           idvar     = "Item Name",
                           direction = "wide")

# dense rank
for(col in c("item_worth.Women", "item_worth.Men", "item_worth.Mixed")){
  if(! col %in% names(wide_data_group)) wide_data_group[[col]] <- 0
}

# calculate dense rank: equal worth -> equal rank
dense_rank0 <- function(x){
  ifelse(x == 0, NA_integer_,
         match(x, sort(unique(x), decreasing = TRUE)))
}

wide_data_group$Rank.Women <- dense_rank0(wide_data_group$item_worth.Women)
wide_data_group$Rank.Men   <- dense_rank0(wide_data_group$item_worth.Men)
wide_data_group$Rank.Mixed  <- dense_rank0(wide_data_group$item_worth.Mixed)

# add NAs for 0
wide_data_group$Rank.Women[is.na(wide_data_group$Rank.Women)] <- ""
wide_data_group$Rank.Men  [is.na(wide_data_group$Rank.Men)]   <- ""
wide_data_group$Rank.Mixed [is.na(wide_data_group$Rank.Mixed)]  <- ""

# sort for female worth
wide_data_group <- wide_data_group[ order(-wide_data_group$item_worth.Women), ]



# select column names
wide_out <- wide_data_group[ ,
                             c("Item Name",
                               "item_worth.Women","Rank.Women",
                               "item_worth.Men","Rank.Men",
                               "item_worth.Mixed","Rank.Mixed") ]

# flextable
ft_group <- flextable(wide_out)
ft_group <- set_header_labels(ft_group,
                              `Item Name`         = "Item name",
                              item_worth.Women   = "Item worth (Women)",
                              Rank.Women         = "Rank (Women)",
                              item_worth.Men     = "Item worth (Men)",
                              Rank.Men           = "Rank (Men)",
                              item_worth.Mixed    = "Item worth (Mixed)",
                              Rank.Mixed          = "Rank (Mixed)" )
ft_group <- autofit(ft_group)

print(ft_group)    

# word
read_docx() |>
  body_add_flextable(ft_group) |>
  print(target = file.path(out_dir, "group_item_ranks.docx"))


# LaTeX export 


df_tex <- wide_out


df_tex$Rank.Women[is.na(df_tex$Rank.Women)] <- ""
df_tex$Rank.Men  [is.na(df_tex$Rank.Men)]   <- ""
df_tex$Rank.Mixed [is.na(df_tex$Rank.Mixed)]  <- ""

xt <- xtable(df_tex, align = c("l","l","r","r","r","r","r","r"))

body_lines <- capture.output(print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,
  only.contents = TRUE
))

# Header
header_lines <- c(
  "\\begin{longtable}{lrrrrrr}",
  "\\caption{Women, men and mixed group item ranks}\\label{sup:tab:genranks_group}\\\\",
  "\\hline",
  "\\textbf{Item name} & \\multicolumn{2}{c}{\\textbf{Women}} & \\multicolumn{2}{c}{\\textbf{Men}} & \\multicolumn{2}{c}{\\textbf{Mixed}} \\\\",
  "& \\textbf{Item worth} & \\textbf{Rank} & \\textbf{Item worth} & \\textbf{Rank} & \\textbf{Item worth} & \\textbf{Rank} \\\\",
  "\\hline",
  "\\endfirsthead",
  "\\hline",
  "\\textbf{Item name} & \\multicolumn{2}{c}{\\textbf{Women}} & \\multicolumn{2}{c}{\\textbf{Men}} & \\multicolumn{2}{c}{\\textbf{Mixed}} \\\\",
  "& \\textbf{Item worth} & \\textbf{Rank} & \\textbf{Item worth} & \\textbf{Rank} & \\textbf{Item worth} & \\textbf{Rank} \\\\",
  "\\hline",
  "\\endhead",
  "\\endfoot",
  "\\hline",
  "\\endlastfoot"
)

out_lines <- c(header_lines, body_lines, "\\end{longtable}")

writeLines(out_lines, file.path(out_dir, "gendered_group_ranks.tex"))



# Graphic group item ranks --------------------------------------------------------

# long format + global dense ranking per group
long_all_grp <- wide_data_group %>%                           # WIDE → LONG
  pivot_longer(starts_with("item_worth."),
               names_to  = "GroupType",                       # Female / Male / Mixed
               values_to = "Worth") %>% 
  mutate(GroupType = str_remove(GroupType, "item_worth\\.")) 

plot_data_grp <- long_all_grp %>% 
  group_by(GroupType) %>% 
  mutate(
    Rank_global = ifelse(
      Worth == 0,                                   
      NA_integer_,
      dplyr::dense_rank(dplyr::desc(Worth)) 
    )
  ) %>% 
  ungroup() %>% 
  filter(`Item Name` %in% devices_keep) %>% 
  mutate(
    Item_F = factor(`Item Name`,
                    levels = long_all_grp %>% 
                      filter(GroupType == "Women",
                             `Item Name` %in% devices_keep) %>% 
                      arrange(desc(Worth)) %>% 
                      pull(`Item Name`)),
    GroupType = factor(GroupType, levels = c("Women","Men","Mixed"))
  )


# colours
col_set <- c(Women = "#55C667FF", 
             Men   = "#481567",   
             Mixed  = "#FDE725")   

# Plot
group_item_plot <- ggplot(plot_data_grp,
                          aes(x = Item_F, y = Worth, fill = GroupType)) +
  geom_col(position = dodge, width = 0.7) +
  geom_text(aes(label  = paste0("#", Rank_global),
                colour = GroupType),
            position  = dodge,
            vjust     = -0.6,
            size      = 3.5,
            show.legend = FALSE) +
  scale_fill_manual(values  = col_set,
                    labels  = c(Women = "Women",
                                Men   = "Men",
                                Mixed  = "Mixed")) +
  scale_colour_manual(values = col_set,
                      labels = c(Women = "Women",
                                 Men   = "Men",
                                 Mixed  = "Mixed")) +
  scale_x_discrete(limits = rev(levels(plot_data_grp$Item_F))) + 
  coord_flip() +
  labs(y = "Item worth",
       x = NULL,
       fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(group_item_plot)

ggsave(file.path(out_dir, "group_item_plot.jpg"), 
       plot = group_item_plot, width = 8, height = 6, dpi = 300)


