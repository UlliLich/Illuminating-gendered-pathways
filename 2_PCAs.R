

# script_2_PCAs


# Load Necessary Packages ------------------------------------------------------

library(tidyverse)
library(haven)
library(survey)
library(ggridges)
library(flextable)
library(officer)
library(xtable)    
library(here)
library(tibble)
library(stringr)



Sys.setenv(LANG = "en")

options(scipen = 999)

stopifnot(
  file.exists(here::here("Quant_data", "New", "hh_clean.rds")),
  file.exists(here::here("Quant_data", "New", "Regression_1.rds"))
)

out_dir <- here::here("Output")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("Quant_data","Stata"), recursive = TRUE, showWarnings = FALSE)

hh           <- readRDS(here::here("Quant_data","New","hh_clean.rds"))
Regression_1 <- readRDS(here::here("Quant_data","New","Regression_1.rds"))




# function latex

fix_pct_latex <- function(x) {
  x <- as.character(x)
  
  # Change to \%
  x <- gsub("\\\\\\\\%", "\\\\%", x)
  
  # Unescaped % -> \% 
  x <- gsub("(?<!\\\\)%", "\\\\%", x, perl = TRUE)
  
  x
}

# Women decision PCA ---------------------------------------------------------

emp_data <- hh %>%
  transmute(
    HHID = hhid,
    weighting = natweight,
    Gender_HeadHousehold = q108,
    gen_self = q105,
    rel_head = q104,
    gen_head = dplyr::case_when(
      !is.na(Gender_HeadHousehold) ~ Gender_HeadHousehold - 1,  # recode 2/1 to 1(female)/0(male) at the same time 
      is.na(Gender_HeadHousehold) & rel_head == 1 ~ gen_self - 1,
      TRUE ~ NA_real_
    ),
    
    high_edu_female = q209,
    
    dec_expensive = #(1 = she can participate/0 = she can not participate)
      ifelse(
        is.na(q105) |
          (is.na(q219__1) & is.na(q219__2) & is.na(q219__3) & is.na(q219__4) &
             is.na(q219__5) & is.na(q219__6) & is.na(q219__96) & is.na(q219__98)),
        NA_real_,
        ifelse(
          dplyr::coalesce(q219__98, 0) == 1,  # 98 => always NA
          NA_real_,
          ifelse(
            dplyr::coalesce(q219__96, 0) == 1 &  # 96 => NA only if "only_other"
              (dplyr::coalesce(q219__1, 0) + dplyr::coalesce(q219__2, 0) +
                 dplyr::coalesce(q219__3, 0) + dplyr::coalesce(q219__4, 0) +
                 dplyr::coalesce(q219__5, 0) + dplyr::coalesce(q219__6, 0)) == 0,
            NA_real_,
            ifelse(
              (q105 == 2 & dplyr::coalesce(q219__1, 0) == 1) |
                (q105 == 1 & dplyr::coalesce(q219__2, 0) == 1),
              1, 0
            )
          )
        )
      ),
    
    # q222: 1/2 = Never/Rarely -> 1 ; 4/5 = Usually/Always -> 0 ; else NA
    dec_clothes = ifelse(q222 == 1 | q222 == 2, 1,
                         ifelse(q222 == 4 | q222 == 5, 0, NA_real_)),
    
    # q223: 1/2 = Never/Rarely -> 1 ; 4/5 = Usually/Always -> 0 ; else NA
    dec_market  = ifelse(q223 == 1 | q223 == 2, 1,
                         ifelse(q223 == 4 | q223 == 5, 0, NA_real_)),
    
    dec_dur = #(1 = she can participate/0 = she can not participate)
      ifelse(
        is.na(q105) |
          (is.na(q401__1) & is.na(q401__2) & is.na(q401__3) & is.na(q401__4) &
             is.na(q401__5) & is.na(q401__6) & is.na(q401__96) & is.na(q401__98)),
        NA_real_,
        ifelse(
          dplyr::coalesce(q401__98, 0) == 1,  # 98 => always NA
          NA_real_,
          ifelse(
            dplyr::coalesce(q401__96, 0) == 1 &
              (dplyr::coalesce(q401__1, 0) + dplyr::coalesce(q401__2, 0) +
                 dplyr::coalesce(q401__3, 0) + dplyr::coalesce(q401__4, 0) +
                 dplyr::coalesce(q401__5, 0) + dplyr::coalesce(q401__6, 0)) == 0,
            NA_real_,
            ifelse(
              (q105 == 2 & dplyr::coalesce(q401__1, 0) == 1) |
                (q105 == 1 & dplyr::coalesce(q401__2, 0) == 1),
              1, 0
            )
          )
        )
      ),
    
    dec_stove = #(1 = she can participate/0 = she can not participate)
      ifelse(
        is.na(q105) |
          (is.na(q503__1) & is.na(q503__2) & is.na(q503__3) & is.na(q503__4) &
             is.na(q503__5) & is.na(q503__6) & is.na(q503__96) & is.na(q503__98)),
        NA_real_,
        ifelse(
          dplyr::coalesce(q503__98, 0) == 1,  # 98 => always NA
          NA_real_,
          ifelse(
            dplyr::coalesce(q503__96, 0) == 1 &
              (dplyr::coalesce(q503__1, 0) + dplyr::coalesce(q503__2, 0) +
                 dplyr::coalesce(q503__3, 0) + dplyr::coalesce(q503__4, 0) +
                 dplyr::coalesce(q503__5, 0) + dplyr::coalesce(q503__6, 0)) == 0,
            NA_real_,
            ifelse(
              (q105 == 2 & dplyr::coalesce(q503__1, 0) == 1) |
                (q105 == 1 & dplyr::coalesce(q503__2, 0) == 1),
              1, 0
            )
          )
        )
      )
  )


emp_data <- emp_data %>%
  mutate(female_edu_category = case_when(
    high_edu_female %in% c(1, 2, 24, 25, 0) ~ 0, # None
    high_edu_female >= 3 & high_edu_female <= 8 ~ 1, # Primary
    high_edu_female >= 9 & high_edu_female <= 11 ~ 2, # Secondary
    high_edu_female >= 12 & high_edu_female <= 16 ~ 3, # Higher Secondary
    high_edu_female >= 17 & high_edu_female <= 23 ~ 4, # Post-Secondary
    high_edu_female == 26 ~ 1, # Adult Education treated as Primary
    high_edu_female == 96 ~ NA_real_, # Other assumed NA
    TRUE ~ NA_real_
  ))



# Removing unwanted columns
emp_PCA_1 <- emp_data %>% select(-gen_self, -rel_head, -Gender_HeadHousehold, -high_edu_female)

# Converting specific columns to numeric using mutate and across

emp_PCA_1 <- emp_PCA_1 %>%
  mutate(across(c(weighting, gen_head, dec_expensive, dec_clothes, dec_market, dec_dur, dec_stove, female_edu_category), as.numeric))



#PCA -> Perform PCA in Stata as Stata is more suited to support using weights in mixed correlations

str(emp_PCA_1)

names(emp_PCA_1)[names(emp_PCA_1) == "female_edu_category"] <- "fem_edu_cat"

write_dta(emp_PCA_1, here::here("Quant_data","Stata","emp_PCA_1.dta"))

stata_files <- c("pca_fe_dec.dta",
                 "pca_eigenvalues_onefactor.dta",
                 "polychoricpca_decision.txt")

if (!all(file.exists(here::here("Quant_data","Stata", stata_files)))) {
  stop("Stata output missing. Please run PCAs.do first (see README).")
}


var_empPCA <- read_dta(here::here("Quant_data","Stata","pca_fe_dec.dta")) %>%
  select(HHID, emp) %>%
  rename(women_decision_index = emp)

#------ Renamed to women_decision_index-------

names(emp_PCA_1)[names(emp_PCA_1) == "fem_edu_cat"] <- "female_edu_category"

emp_PCA <- left_join(emp_PCA_1, var_empPCA, by = "HHID")

stopifnot(nrow(var_empPCA) == dplyr::n_distinct(var_empPCA$HHID))
stopifnot(nrow(emp_PCA) == nrow(emp_PCA_1))

sum(is.na(emp_PCA$women_decision_index))
summary(emp_PCA$women_decision_index)


data_decision <- emp_PCA %>%
  select(HHID, gen_head, women_decision_index)

# Density plot of PCA by gender of household head ---------------------------------------------------------


emp_PCA_labeled <- emp_PCA %>%
  mutate(
    hh_type = case_when(
      gen_head == 1 ~ "Women",
      gen_head == 0 ~ "Men",
      TRUE ~ NA_character_
    ),
    hh_type_code = case_when(
      gen_head == 1 ~ 1, #give nmeric code to order plots in spefific row
      gen_head == 0 ~ 2,
      TRUE ~ NA_real_
    )
  )

# Add "All" category with numeric code
emp_PCA_combined <- bind_rows(
  emp_PCA_labeled %>% mutate(hh_type = "All", hh_type_code = 3),
  emp_PCA_labeled
)

my_cols <- c(
  "Women" = "#55C667FF",   
  "Men"   = "#481567",
  "All"    = "#FDE725"
)

# Plotting
decision_making_plot <- ggplot(emp_PCA_combined, aes(x = women_decision_index, y = reorder(hh_type, hh_type_code), fill = hh_type, color = hh_type, weight = weighting)) +
  geom_density_ridges(alpha = 0.5, size = 1) +
  scale_fill_manual(values = my_cols, name = NULL) +   # <-- neu
  scale_color_manual(values = my_cols, guide = "none") +   # <-- neu
  labs(x = "Decision-making index", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_blank(),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(margin = margin(b = 15)))

print(decision_making_plot)


ggsave(file.path(out_dir, "Decision_plot.png"), plot = decision_making_plot, width = 8, height = 6, dpi = 300, device = "png")



# (WEIGHTED) Women’s decision-making power index: descriptive statistics of included variables ---------------------------------------------------------
# only the percentages are weighted

# Make sure emp_PCA$weighting is numeric
design_emp <- svydesign(ids = ~1, data = emp_PCA, weights = ~weighting)


#A function for a specific category
get_weighted_n_pct <- function(var, value_of_interest) {
  # Unweighted count
  unweighted_n <- sum(emp_PCA[[var]] == value_of_interest, na.rm = TRUE)
  
  # Weighted proportion
  # Force logical -> numeric so svymean() returns 1 value (the proportion of TRUE)
  fmla <- as.formula(paste0("~as.numeric(", var, " == ", value_of_interest, ")"))
  m <- svymean(fmla, design_emp, na.rm = TRUE)
  
  wtd_pct <- coef(m) * 100
  paste0(unweighted_n, " (", round(wtd_pct, 1), "%)")
}


#A function for “missingness”
get_weighted_n_pct_missing <- function(var) {
  # Unweighted count of missing
  unweighted_n <- sum(is.na(emp_PCA[[var]]))
  
  # Weighted proportion missing
  # is.na(var) is logical -> convert to numeric, e.g. 1 if missing, 0 otherwise
  fmla <- as.formula(paste0("~as.numeric(is.na(", var, "))"))
  m <- svymean(fmla, design_emp, na.rm = FALSE) 
  # na.rm=FALSE so that missingness is actually counted as a "category"
  
  wtd_pct <- coef(m) * 100
  paste0(unweighted_n, " (", round(wtd_pct, 1), "%)")
}


#Gender subcategories
gender_subcategories <- data.frame(
  Variable = c(
    "Gender of household head", 
    "  Men", 
    "  Women", 
    "  Missing"
  ),
  Specification = c(
    "",  # heading
    get_weighted_n_pct("gen_head", 0),      # male
    get_weighted_n_pct("gen_head", 1),      # female
    get_weighted_n_pct_missing("gen_head")  # missing
  )
)

#Education subcategories
education_subcategories <- data.frame(
  Variable = c(
    "Highest level of education women household member", 
    "  None", 
    "  Primary", 
    "  Secondary", 
    "  Higher Secondary", 
    "  Post-secondary",
    "  Missing"
  ),
  Specification = c(
    "",  # heading
    get_weighted_n_pct("female_edu_category", 0),
    get_weighted_n_pct("female_edu_category", 1),
    get_weighted_n_pct("female_edu_category", 2),
    get_weighted_n_pct("female_edu_category", 3),
    get_weighted_n_pct("female_edu_category", 4),
    get_weighted_n_pct_missing("female_edu_category")
  )
)

#Decision subcategory
decision_responses <- data.frame(
  Variable = c(
    "Decision about expensive items",
    "  She participates",
    "  Otherwise",
    "  Missing",
    "Decision about buying appliances",
    "  She participates",
    "  Otherwise",
    "  Missing",
    "Decision about stove and fuel",
    "  She participates",
    "  Otherwise",
    "  Missing",
    "Needs to ask for permission about buying herself clothes",
    "  Never/Rarely",
    "  Usually/Always",
    "  Missing",
    "Needs to ask for permission about buying at market",
    "  Never/Rarely",
    "  Usually/Always",
    "  Missing"
  ),
  Specification = c(
    # Decision about expensive items
    "",
    get_weighted_n_pct("dec_expensive", 1),
    get_weighted_n_pct("dec_expensive", 0),
    get_weighted_n_pct_missing("dec_expensive"),
    
    # Decision about buying appliances
    "",
    get_weighted_n_pct("dec_dur", 1),
    get_weighted_n_pct("dec_dur", 0),
    get_weighted_n_pct_missing("dec_dur"),
    
    # Decision about stove and fuel
    "",
    get_weighted_n_pct("dec_stove", 1),
    get_weighted_n_pct("dec_stove", 0),
    get_weighted_n_pct_missing("dec_stove"),
    
    # Needs to ask for permission about buying herself clothes
    "",
    get_weighted_n_pct("dec_clothes", 1),
    get_weighted_n_pct("dec_clothes", 0),
    get_weighted_n_pct_missing("dec_clothes"),
    
    # Needs to ask for permission about buying at market
    "",
    get_weighted_n_pct("dec_market", 1),
    get_weighted_n_pct("dec_market", 0),
    get_weighted_n_pct_missing("dec_market")
  )
)

#Combine, create flextable, export
combined_summary_data <- rbind(
  gender_subcategories,
  education_subcategories,
  decision_responses
)

ft_descriptives_dec <- flextable(combined_summary_data) %>%
  autofit() %>%
  theme_vanilla() %>%
  delete_part(part = "header") %>%  
  bold(i = ~ Variable %in% c(
    "Gender of household head",
    "Highest level of education women household member",
    "Decision about expensive items",
    "Decision about buying appliances",
    "Decision about stove and fuel",
    "Needs to ask for permission about buying herself clothes",
    "Needs to ask for permission about buying at market"
  ), part = "body") %>%
  padding(padding = 2, part = "all") %>%
  set_table_properties(layout = "autofit")

# Optionally remove lines for certain "response_categories":
response_categories <- c(
  "  She participates",
  "  Otherwise",
  "  Missing",
  "  None",
  "  Primary",
  "  Secondary",
  "  Higher Secondary",
  "  Post-secondary",
  "  Never/Rarely",
  "  Usually/Always",
  "  Men",
  "  Women"
)

ft_descriptives_dec <- ft_descriptives_dec %>%
  hline(i = ~ Variable %in% response_categories, 
        border = fp_border(width = 0, color = "white"))


# Print to check
print(ft_descriptives_dec)


# export to LaTex


df <- combined_summary_data

# no indent
df$Variable <- trimws(df$Variable)

# Headings --> Specification == ""
is_heading <- is.na(df$Specification) | df$Specification == ""

# Headings bold for first line
df$Variable[is_heading] <- paste0("\\textbf{", df$Variable[is_heading], "}")
df$Specification[is_heading] <- ""

# Lines
heading_idx <- which(is_heading)
group_end_idx <- c(heading_idx[-1] - 1, nrow(df))   
hline_after <- sort(unique(c(heading_idx, group_end_idx)))

# 
out <- df[, c("Variable", "Specification")]
colnames(out) <- c("", "")

out[] <- lapply(out, fix_pct_latex)

xt <- xtable(
  out,
  caption = "Women’s decision-making power index: descriptive statistics of included variables (N = 3,583; index available for n = 3,116). Counts are unweighted; percentages are survey-weighted.",
  label   = "sup:tab:decision_descriptives"
)

print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,         
  hline.after = sort(unique(c(-1, hline_after))),                 
  floating = TRUE,                          
  table.placement = "!htbp",
  caption.placement = "top",
  align = c("l", "l", "r"),                  
  file = file.path(out_dir, "Decision_descriptives_weighted.tex")
)

# Women’s decision-making power index: scoring coefficients---------------------------


parse_polychoric_scoring <- function(txt_path) {
  lines <- readLines(txt_path, warn = FALSE)
  
  # 1) "Scoring coefficients"
  i0 <- grep("Scoring coefficients", lines)
  stopifnot(length(i0) >= 1)
  i0 <- i0[1]
  
  # 2) Header
  search_window <- lines[(i0+1):min(length(lines), i0+300)]
  i_hdr_rel <- grep("Variable.*\\|.*Coeff", search_window)
  stopifnot(length(i_hdr_rel) >= 1)
  i_hdr <- i0 + i_hdr_rel[1]
  
  # 
  i_start <- i_hdr + 1
  if (i_start <= length(lines) && str_detect(lines[i_start], "-{3,}")) {
    i_start <- i_start + 1
  }
  
  #
  stop_pat <- str_c(
    "^\\s*matrix\\b",
    "^\\s*return list\\b",
    "^\\s*ereturn list\\b",
    "^\\s*Principal component analysis\\b",
    "^\\s*\\.$",
    "^\\s*end of do-file\\b",
    sep = "|"
  )
  
  tail_window <- lines[i_start:min(length(lines), i_start+1000)]
  i_stop_rel <- which(str_detect(tail_window, stop_pat))
  i_end <- if (length(i_stop_rel) >= 1) i_start + i_stop_rel[1] - 2 else min(length(lines), i_start+300)
  
  sec <- lines[i_start:i_end]
  
  # 5) Parsen:
  current_var <- NA_character_
  out <- list()
  
  for (ln in sec) {
    if (str_detect(ln, "^\\s*$")) next
    
    if (!str_detect(ln, "\\|")) {
      current_var <- str_trim(ln)
      next
    }
    
    parts <- str_split(ln, "\\|", simplify = TRUE)
    level <- str_trim(parts[1])
    
    c1 <- suppressWarnings(as.numeric(str_trim(parts[2])))
    c2 <- suppressWarnings(as.numeric(str_trim(parts[3])))
    c3 <- suppressWarnings(as.numeric(str_trim(parts[4])))
    
    if (!is.na(c1) && !is.na(current_var)) {
      out[[length(out) + 1]] <- tibble(
        variable = current_var,
        category = level,
        coeff1   = c1,
        coeff2   = c2,
        coeff3   = c3
      )
    }
  }
  
  bind_rows(out)
}

scoring_df <- parse_polychoric_scoring(here::here("Quant_data","Stata","polychoricpca_decision.txt"))
scoring_df


# LaTeX


# 1) Component 1
sc1 <- scoring_df %>%
  transmute(
    variable = str_trim(as.character(variable)),
    category = str_trim(as.character(category)),
    coeff1   = as.numeric(coeff1)
  )

# 2) Helper
block <- function(heading, var, cats, labels) {
  key_needed  <- paste(var, cats)
  key_have    <- paste(sc1$variable, sc1$category)
  coeffs      <- sc1$coeff1[match(key_needed, key_have)]
  
  tibble(
    Variable    = c(paste0("\\textbf{", heading, "}"), labels),
    Coefficient = c("", ifelse(is.na(coeffs), "", sprintf("%.6f", coeffs)))
  )
}

# 3) Build table
tab_scoring <- bind_rows(
  block("Gender of household head", "gen_head", c("0","1"), c("Men","Women")),
  
  block("Highest level of education women household member",
        if ("fem_edu_cat" %in% sc1$variable) "fem_edu_cat" else "female_edu_category",
        c("0","1","2","3","4"),
        c("None","Primary","Secondary","Higher Secondary","Post-secondary")),
  
  block("Decision about expensive items", "dec_expensive", c("0","1"), c("Otherwise","She participates")),
  block("Decision about buying appliances", "dec_dur",      c("0","1"), c("Otherwise","She participates")),
  block("Decision about stove and fuel",   "dec_stove",    c("0","1"), c("Otherwise","She participates")),
  block("Needs to ask for permission about buying herself clothes", "dec_clothes", c("0","1"), c("Usually/Always","Never/Rarely")),
  block("Needs to ask for permission about buying at market",       "dec_market",  c("0","1"), c("Usually/Always","Never/Rarely"))
)

# 4) Lines in table
is_heading    <- tab_scoring$Coefficient == ""
heading_idx   <- which(is_heading)
group_end_idx <- c(heading_idx[-1] - 1, nrow(tab_scoring))
hline_after   <- sort(unique(c(-1, heading_idx, group_end_idx)))

# 5) LaTeX export
out <- tab_scoring
colnames(out) <- c("", "")

xt <- xtable(
  out,
  caption = "Women’s decision-making power index: scoring coefficients (Component 1, polychoric PCA).",
  label   = "sup:tab:decision_scoringcoeff"
)

print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,
  hline.after = hline_after,
  floating = TRUE,
  table.placement = "!htbp",
  caption.placement = "top",
  align = c("l","l","r"),
  file = file.path(out_dir, "Decision_scoringcoeff.tex")
)




# Women’s decision-making power index: Eigenvalues of the retained factors ---------------------------------------------------------
  

#Perform PCA and calculate eigenvalues in Stata

#import data set from Stata
pca_eig    <- read_dta(here::here("Quant_data","Stata","pca_eigenvalues_onefactor.dta"))

#Build a small data frame suitable for Flextable
pca_eigs_df <- pca_eig %>%
  transmute(
    Variable = "\\textbf{Retained factor}",
    Eigenvalue = round(eigenvalue, 2),
    Proportion_of_variation_explained = round(proportion_explained, 2)
  )

#Create flextable
ft_eigen_1 <- flextable(pca_eigs_df) %>%
  theme_vanilla() %>%
  padding(padding = 5, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  border_remove() %>%
  delete_part(part = "header") %>%
  add_header_row(
    values = c("", "Eigenvalue", "Proportion of variation explained"),
    colwidths = c(1, 1, 1)
  ) %>%
  bold(j = c(2, 3), part = "header") %>%
  hline(i = 1, part = "header", border = fp_border(width = 2))

print(ft_eigen_1)

# export to LaTex

# "Retained factor" bold
pca_eigs_df$Variable <- "\\textbf{Retained factor}"

my_xtable <- xtable(
  pca_eigs_df,
  caption = "Eigenvalues PCA decision-making",
  label = "sup:tab:decision_eigen"
)

print(
  my_xtable,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,
  hline.after = c(-1, nrow(pca_eigs_df)),   # only top and bottom lines
  caption.placement = "top",
  file = file.path(out_dir, "Decision_eigenvalues.tex")
)

# Wealth PCA --------------------------------------------------------------

# Define the mapping function
#roof_category_fun <- function(roof_type) {
  #if (roof_type %in% c(1, 2, 3, 4)) {
   #return(1)  # plastic/canvas, reed/bamboo, wood and mud, and wood and thatch 
  #} else if (roof_type %in% c(5, 6)) {
    #return(2)  # asbestos, and corrugated iron sheet
 # } else if (roof_type %in% c(7, 8)) {
    #return(3)  # bricks, and stone and cement
 # } else if (roof_type == 0) {
    #return(0)  # None
 # } else if (roof_type == 96) {
   # return(NA)  # Other
 # } else {
 #   return(NA)  # In case of any other value
 # }
#}


wealth_PCA <- hh %>%
  transmute(HHID = hhid,
            weighting = natweight,
            Gender_HeadHousehold = q108,
            gen_self = q105,
            rel_head = q104,
            gen_head = dplyr::case_when(
              !is.na(Gender_HeadHousehold) ~ Gender_HeadHousehold - 1, ##we recode 2/1 to 1(female)/0(male) at the same time 
              is.na(Gender_HeadHousehold) & rel_head == 1 ~ gen_self - 1,
              TRUE ~ NA_real_),
            num_chairs = q202,
            num_bic = q207_1,
            num_motorbi = q207_2,
            num_car = q207_3,
            hh_expen = q218,
            own_home = ifelse(q224 == 1, 2, ifelse(q224 == 2, 0, ifelse(q224 == 3, 1, NA))), # 0 = Rented, 1 = Borrowed/Shared, 2 = Owned
            total_members = rowSums(cbind(q203_1, q203_2, q203_3, q203_4), na.rm = TRUE),
            total_members = dplyr::na_if(total_members, 0),
            rooms_per_member = q205 / total_members,
            chairs_per_member = num_chairs / total_members,
            #adjusted_size = ifelse(total_members > 1, 1 + (total_members - 1) * 0.7, 1),
            #hh_expen_bymemb = hh_expen / adjusted_size,
            hh_expen_bynumb = hh_expen / total_members,
            #roof_category = sapply(q206, roof_category_fun),
            log_hh_expen_bynumb = log1p(hh_expen_bynumb)
  )

# Exclude columns with low variance and unnecessary ones
#Roof categories: variance too low
wealth_PCA_1 <- wealth_PCA %>%
  select(HHID, weighting, chairs_per_member, num_bic, num_motorbi, own_home, rooms_per_member, log_hh_expen_bynumb)

# Convert own_home to numeric
wealth_PCA_1 <- wealth_PCA_1 %>%
  mutate(
    own_home = as.numeric(as.character(own_home))  # ensures 0,1,2 numeric
  )

#Test variables for use in PCA
sapply(wealth_PCA_1, function(x) sum(is.na(x)))

cor(wealth_PCA_1[, c("chairs_per_member", "num_bic", "num_motorbi", "rooms_per_member", "log_hh_expen_bynumb")],
    use = "complete.obs")

summary(wealth_PCA_1$chairs_per_member)
summary(wealth_PCA_1$num_bic)
summary(wealth_PCA_1$num_motorbi)
summary(wealth_PCA_1$rooms_per_member)
summary(wealth_PCA_1$log_hh_expen_bynumb)

str(wealth_PCA_1)

#Define survey design
design_wealth <- svydesign(
  ids = ~1,                # no cluster variable if your data is not clustered
  data = wealth_PCA_1,     # the data frame
  weights = ~weighting     # the probability/survey weight
)

#Run weighted PCA 
pca_wealth <- svyprcomp(
  ~ chairs_per_member + num_bic + num_motorbi + own_home + rooms_per_member + log_hh_expen_bynumb,
  design = design_wealth,
  center = TRUE,  # subtract mean
  scale  = TRUE   # divide by sd
)

loadings_pca <- pca_wealth$rotation
loadings_pca

summary(pca_wealth)

# Predict PC scores
scores_matrix <- predict(pca_wealth, newdata = wealth_PCA_1)

# Store the first component as 'wealth' in your data
wealth_PCA_1$wealth <- scores_matrix[, 1]

# wealth data
wealth_data <- left_join(
  wealth_PCA_1,
  select(wealth_PCA, HHID, gen_head, hh_expen_bynumb),
  by = "HHID"
)

# weighted standardising first component
w <- wealth_data$weighting
m <- weighted.mean(wealth_data$wealth, w, na.rm = TRUE)
s <- sqrt(weighted.mean((wealth_data$wealth - m)^2, w, na.rm = TRUE))


wealth_data <- wealth_data %>%
  mutate(wealth = (wealth - m) / s)

weighted.mean(wealth_data$wealth, wealth_data$weighting, na.rm=TRUE)

wealth_final <- wealth_data %>%
  select(HHID, wealth)


# Wealth density plot of PCA by gender of household head  ---------------------------------------------------------

# Add gender information and numeric code for ordering
wealth_PCA_plot <- wealth_data %>%
  mutate(
    hh_type = case_when(
      gen_head == 1 ~ "Women",
      gen_head == 0 ~ "Men",
      TRUE ~ NA_character_
    ),
    hh_type_code = case_when(
      gen_head == 1 ~ 1, # give numeric code to order plots in specific row
      gen_head == 0 ~ 2,
      TRUE ~ NA_real_
    )
  )

# Add "All" category with numeric code
wealth_PCA_combined <- bind_rows(
  wealth_PCA_plot %>% mutate(hh_type = "All", hh_type_code = 3),
  wealth_PCA_plot
)


# Plotting
wealth_plot <- ggplot(wealth_PCA_combined, aes(x = wealth, y = reorder(hh_type, hh_type_code), fill = hh_type, color = hh_type, weight = weighting)) +
  geom_density_ridges(alpha = 0.5, size = 1) +
  scale_fill_manual(values = my_cols, name = NULL) +
  scale_color_manual(values = my_cols, guide = "none") +  
  labs(x = "Wealth index", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_blank(),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(margin = margin(b = 15)))

print(wealth_plot)


ggsave(file.path(out_dir, "Wealth_gendered_plot.jpg"), plot = wealth_plot, width = 8, height = 6, dpi = 300, device = "jpeg")



# (WEIGHTED) Wealth index: descriptive statistics of included variables ---------------------------------------------------------


# If own_home has values 0 = Rented, 1 = Borrowed, 2 = Owned:
wealth_data$home_rented   <- as.numeric(wealth_data$own_home == 0)
wealth_data$home_borrowed <- as.numeric(wealth_data$own_home == 1)
wealth_data$home_owned    <- as.numeric(wealth_data$own_home == 2)


# Create the survey design with weights
design_wealth <- svydesign(ids = ~1, data = wealth_data, weights = ~weighting)

#Weighted mean (for chairs, bicycles, motorbikes, expenditures)
get_weighted_n_mean <- function(var) {
  # Unweighted count of non-missing observations
  unweighted_n <- sum(!is.na(wealth_data[[var]]))
  
  # Weighted mean using survey design
  m <- svymean(as.formula(paste0("~", var)), design_wealth, na.rm = TRUE)
  wtd_mean <- coef(m)
  
  # Format "N (mean)"
  paste0(unweighted_n, " (", round(wtd_mean, 1), ")")
}

get_weighted_n_pct_binary <- function(var) {
  # Unweighted count of households with var == 1
  unweighted_n <- sum(wealth_data[[var]] == 1, na.rm = TRUE)
  
  # Weighted proportion of var == 1
  m <- svymean(as.formula(paste0("~", var)), design_wealth, na.rm = TRUE)
  wtd_pct <- coef(m) * 100
  
  # IMPORTANT: escape % for LaTeX
  paste0(unweighted_n, " (", round(wtd_pct, 1), "%)")
}


#Calculate weighted values for each row of your table
#Numeric variables (chairs, bicycles, motorbikes, expenditures):
chairs_str      <- get_weighted_n_mean("chairs_per_member")
rooms_str      <- get_weighted_n_mean("rooms_per_member")
bicycles_str    <- get_weighted_n_mean("num_bic")
motorbikes_str  <- get_weighted_n_mean("num_motorbi")
expen_str       <- get_weighted_n_mean("hh_expen_bynumb")
log_expen_str   <- get_weighted_n_mean("log_hh_expen_bynumb")


#Home ownership (already split into 3 binary variables):
rented_str          <- get_weighted_n_pct_binary("home_rented")
borrowed_shared_str <- get_weighted_n_pct_binary("home_borrowed")
owned_str           <- get_weighted_n_pct_binary("home_owned")


wealth_summary_weighted <- data.frame(
  Variable = c(
    "Assets",
    "Bicycles", 
    "Motorbikes", 
    "Chairs per member", 
    "Rooms per member", 
    "Home ownership",
    "  Rented", 
    "  Borrowed/shared",
    "  Owned",
    "Expenditure",
    "Per member",
    "Log per member"
  ),
  Specification = c(
    "",                 # heading -> leer
    bicycles_str,
    motorbikes_str,
    chairs_str,
    rooms_str,
    "",                 # heading -> leer
    rented_str,
    borrowed_shared_str,
    owned_str,
    "",                 # heading -> leer
    expen_str,
    log_expen_str
  ),
  stringsAsFactors = FALSE
)


#Build the flextable
ft_wealth_descriptives_wtd <- flextable(wealth_summary_weighted) %>%
  autofit() %>%
  theme_vanilla() %>%
  delete_part(part = "header") %>%
  bold(i = ~ Variable %in% c(
    "Bicycles",
    "Motorbikes",
    "Chairs per member", 
    "Rooms per member",
    "Home ownership",
    "Expenditure"
  ), part = "body") %>%
  padding(padding = 2, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  border_remove() %>%
  hline(i = ~ Variable %in% c(
    "Bicycles",
    "Motorbikes",
    "Chairs per member", 
    "Rooms per member",
    "Home ownership",
    "Expenditure"
  ), border = fp_border(width = 1, color = "black")) %>%
  # "Home ownership" row is the 4th item, so add a line below that:
  hline(i = 4, border = fp_border(width = 1, color = "black"))

print(ft_wealth_descriptives_wtd)


# export to LaTeX

df <- wealth_summary_weighted

# no indent
df$Variable <- trimws(df$Variable)

# headings are rows where Specification is "" (Assets, Home ownership, Expenditure)
is_heading <- is.na(df$Specification) | df$Specification == ""

# bold headings (only first column) and blank right column for headings
df$Variable[is_heading] <- paste0("\\textbf{", df$Variable[is_heading], "}")
df$Specification[is_heading] <- ""

# line positions: top of table + after each heading row + after each group end
heading_idx <- which(is_heading)
group_end_idx <- c(heading_idx[-1] - 1, nrow(df))
hline_after <- sort(unique(c(-1, heading_idx, group_end_idx)))

out <- df[, c("Variable", "Specification")]
colnames(out) <- c("", "")

out[] <- lapply(out, fix_pct_latex)

xt <- xtable(
  out,
  caption = "Wealth index: descriptive statistics of included variables (n = 3,583). Counts are unweighted; means/proportions are survey-weighted.",
  label   = "sup:tab:wealth_descriptives"
)

print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,
  hline.after = hline_after,
  floating = TRUE,
  table.placement = "!htbp",
  caption.placement = "top",
  align = c("l", "l", "r"),
  file = file.path(out_dir, "Wealth_descriptives_weighted.tex")
)

# Wealth index: Component loadings -----------------------------------------------

l_pc1 <- pca_wealth$rotation[, 1] * pca_wealth$sdev[1]

getL <- function(nm) {
  if (!nm %in% names(l_pc1)) return(NA_real_)
  as.numeric(l_pc1[nm])
}

tab_wealth_load <- dplyr::bind_rows(
  tibble::tibble(Variable="\\textbf{Assets}", Loading=""),
  tibble::tibble(Variable="Bicycles",         Loading=sprintf("%.6f", getL("num_bic"))),
  tibble::tibble(Variable="Motorbikes",       Loading=sprintf("%.6f", getL("num_motorbi"))),
  tibble::tibble(Variable="Chairs per member",Loading=sprintf("%.6f", getL("chairs_per_member"))),
  tibble::tibble(Variable="Rooms per member", Loading=sprintf("%.6f", getL("rooms_per_member"))),
  
  tibble::tibble(Variable="\\textbf{Home ownership}", Loading=""),
  tibble::tibble(Variable="Home ownership (ordinal: 0=rented, 1=borrowed/shared, 2=owned)",
                 Loading=sprintf("%.6f", getL("own_home"))),
  
  tibble::tibble(Variable="\\textbf{Expenditure}", Loading=""),
  tibble::tibble(Variable="Log expenditure per member", Loading=sprintf("%.6f", getL("log_hh_expen_bynumb")))
)

# Lines
is_heading    <- tab_wealth_load$Loading == ""
heading_idx   <- which(is_heading)
group_end_idx <- c(heading_idx[-1] - 1, nrow(tab_wealth_load))
hline_after   <- sort(unique(c(-1, heading_idx, group_end_idx)))

out <- tab_wealth_load
colnames(out) <- c("", "")

xt <- xtable::xtable(
  out,
  caption = "Wealth index: component loadings (PC1, survey-weighted PCA).",
  label   = "sup:tab:wealth_loadings"
)

print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,
  hline.after = hline_after,
  floating = TRUE,
  table.placement = "!htbp",
  caption.placement = "top",
  align = c("l","l","r"),
  file = file.path(out_dir, "Wealth_loadings.tex")
)



# Wealth index: Eigenvalues ---------------------------------------------------------

# Extract eigenvalues from the PCA (eigenvalues = squared standard deviations)
eigenvalues_wealth <- pca_wealth$sdev^2

# Calculate the proportion of variance explained by the first component
proportion_variance_explained_wealth <- eigenvalues_wealth[1] / sum(eigenvalues_wealth)

# Prepare table for eigenvalues and variance explained
eigenvalues_df_wealth <- data.frame(
  Variable = c("Retained factor"),
  Eigenvalue = round(eigenvalues_wealth[1], 2),
  Proportion_of_variation_explained = round(proportion_variance_explained_wealth, 2)
)

print(eigenvalues_df_wealth)

# Create a flextable for eigenvalues and variance explained
ft_eigen_wealth <- flextable(eigenvalues_df_wealth) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  padding(padding = 5, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  border_remove() %>%
  delete_part(part = "header")

ft_eigen_wealth <- add_header_row(
  ft_eigen_wealth,
  values = c("", "Eigenvalue", "Proportion of variation explained"),
  colwidths = c(1, 1, 1)
) %>%
  bold(j = c(2, 3), part = "header") %>%
  hline(i = 1, part = "header", border = fp_border(width = 2))

# Print the flextable to check
print(ft_eigen_wealth)

# latex

eigenvalues_df_wealth_ltx <- eigenvalues_df_wealth
eigenvalues_df_wealth_ltx$Variable <- "\\textbf{Retained factor}"

xt <- xtable(
  eigenvalues_df_wealth_ltx,
  caption = "Eigenvalues wealth PCA",
  label = "sup:tab:wealth_eigen"
)


print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = identity,
  hline.after = c(-1, nrow(eigenvalues_df_wealth_ltx)),  # only top & bottom line
  caption.placement = "top",
  file = file.path(out_dir, "Wealth_eigen.tex")
)

# Merge datasets ---------------------------------------------------------

Regression_2 <- left_join(data_decision, wealth_final, by = c("HHID"))

regression_data <- left_join(Regression_1, Regression_2, by = c("HHID"))

saveRDS(regression_data, here::here("Quant_data","New","regression_data.rds"))



