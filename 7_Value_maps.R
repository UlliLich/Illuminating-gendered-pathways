
# script_7_Value maps

library(tidyverse)
library(readxl)
library(janitor)
library(here)



Sys.setenv(LANG = "en")
options(scipen = 999)

out_dir <- here::here("Output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# read data
raw_file <- here::here("Qual_data","Raw",
                       "PeopleSun User Research_sentiment analysis.xlsx")

stopifnot(file.exists(raw_file))

# Same source file as script 6, but used at extract level here:
# value prevalence is computed per paragraph inside topic_weights_paragraph().
df_base <- read_excel(raw_file, guess_max = 20000) %>%
  clean_names() %>%
  transmute(
    paragraph_id,
    interview_id,
    item_name   = str_trim(item_name),
    annotations = str_trim(annotations),
    gender      = str_squish(as.character(gender))
  ) %>%
  mutate(
    gender = dplyr::recode(gender,
                           "Female" = "Women",
                           "Male"   = "Men",
                           .default = gender
    )
  )

stopifnot(all(na.omit(df_base$gender) %in% c("Women","Men")))


# Value rankings (aggregated over 11 devices) -------------------------------------

# 11 devices
devices_keep <- c(
  "Mobile Phone","Fan","Iron","TV","Fridge",
  "Light Bulb","Radio","Blender","Computer",
  "Sewing Machine","Torch"
)

sep_char <- ";"

df_base <- df_base %>%
  filter(
    item_name %in% devices_keep,
    gender %in% c("Women","Men"),
    !is.na(annotations), annotations != ""
  ) %>%
  separate_rows(annotations, sep = sep_char) %>%
  mutate(annotations = str_trim(annotations)) %>%
  filter(annotations != "") %>%
  rename(annotation = annotations)

# Display labels (what you want to see in tables/plots)
device_label_map <- c(
  "Mobile Phone" = "Phone",
  "Light Bulb"   = "Bulb"
)

label_device <- function(x) dplyr::recode(x, !!!device_label_map, .default = x)

# Core: paragraph-prevalence per gender
topic_weights_paragraph <- function(d) {
  # One count per paragraph × topic (no multiple counts within the same paragraph)
  pts <- d %>% distinct(paragraph_id, annotation)
  # Denominator: total number of unique paragraphs in the current filter
  paragraph_count <- dplyr::n_distinct(pts$paragraph_id)
  # Numerator: unique paragraphs mentioning each topic
  pts %>%
    count(annotation, name = "annotation_count") %>%
    mutate(percent = 100 * annotation_count / paragraph_count) %>%
    transmute(value = annotation, percent)
}

# --- Compute weights for each gender separately
female_tbl <- df_base %>%
  filter(gender == "Women") %>%
  topic_weights_paragraph() %>%
  rename(`percent women` = percent)

male_tbl <- df_base %>%
  filter(gender == "Men") %>%
  topic_weights_paragraph() %>%
  rename(`percent men` = percent)

# --- Wide table with the exact columns your plotting code expects
gender_values_wide <- full_join(female_tbl, male_tbl, by = "value") %>%
  arrange(desc(pmax(coalesce(`percent women`, 0), coalesce(`percent men`, 0))))

# Optional: inspect the first rows for a quick check
# Men (sorted by highest percentage)
cat("\n================ MEN (11 devices) ================\n")
male_tbl %>%
  arrange(desc(`percent men`)) %>%
  print(n = 20)

# Women (sorted by highest percentage)
cat("\n================ WOMEN (11 devices) ================\n")
female_tbl %>%
  arrange(desc(`percent women`)) %>%
  print(n = 20)

# Reshape to long format for plot code
gender_data_long <- gender_values_wide %>%
  select(value, `percent women`, `percent men`) %>%
  pivot_longer(
    cols = c(`percent women`, `percent men`),
    names_to = "gender",
    values_to = "percent"
  ) %>%
  mutate(
    gender = case_when(
      gender == "percent women" ~ "Women",
      gender == "percent men"   ~ "Men"
    )
  )

#### Plot top-10 selection

rank_all <- gender_data_long %>%
  group_by(gender) %>%
  mutate(rank = dense_rank(desc(percent))) %>%
  ungroup()


rank_wide <- rank_all %>%
  select(value, gender, rank) %>%
  pivot_wider(names_from = gender, values_from = rank, values_fill = NA) %>%
  rename(rank_women = Women, rank_men = Men) %>%
  mutate(
    rank_women = ifelse(rank_women > 10, NA, rank_women),
    rank_men   = ifelse(rank_men   > 10, NA, rank_men)
  ) %>%
  filter(!is.na(rank_women) | !is.na(rank_men)) %>%
  mutate(rank_women_order = ifelse(is.na(rank_women), Inf, rank_women)) %>%
  arrange(rank_women_order, rank_men, value) %>%
  mutate(value = factor(value, levels = rev(unique(value))))  # rank 1 at top


# plot: aggregated top-ranked values (split tiles)
plot_data <- rank_wide %>%
  pivot_longer(cols = c(rank_women, rank_men),
               names_to  = "gender_src", values_to = "rank") %>%
  mutate(
    gender = ifelse(gender_src == "rank_women", "Women", "Men"),
    x_pos  = ifelse(gender == "Women", 1, 2),
    fill   = ifelse(gender == "Women", "#55C667", "#5B2BCB"),
    has_rank = !is.na(rank)
  )


split_tile_rank_plot <- ggplot(plot_data, aes(x = x_pos, y = value)) +
  geom_tile(data = filter(plot_data, has_rank),
            aes(fill = fill), width = 0.9, height = 0.9, show.legend = FALSE) +
  geom_text(data = filter(plot_data, has_rank),
            aes(label = rank), colour = "white",
            fontface = "bold", size = 5) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Women", "Men")) +
  scale_fill_identity() +
  labs(
    x = NULL, y = NULL,
    title = "Top-ranked values across all devices",
    subtitle = "(green = women, purple = men)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0, face = "bold"),
    axis.text.x = element_text(size = 13, colour = "grey20"),
    axis.text.y = element_text(size = 13, colour = "grey20"),
    panel.grid  = element_blank()
  )


print(split_tile_rank_plot)

ggsave(file.path(out_dir, "top_ranked_values_split_tile.png"),
       plot = split_tile_rank_plot, width = 7, height = 8, dpi = 300, bg = "white")




# Gendered value rankings (by device) -------------------------------------


# Helper: compute Women/Men paragraph-prevalence for ONE device, return LONG rows
device_gender_long <- function(device_name) {
  d_dev <- df_base %>% dplyr::filter(item_name == device_name)
  
  fem <- d_dev %>%
    dplyr::filter(gender == "Women") %>%
    topic_weights_paragraph() %>%
    dplyr::mutate(gender = "Women")
  
  men <- d_dev %>%
    dplyr::filter(gender == "Men") %>%
    topic_weights_paragraph() %>%
    dplyr::mutate(gender = "Men")
  
  dplyr::bind_rows(fem, men) %>%
    dplyr::mutate(device = label_device(device_name)) %>%  # labeled name
    dplyr::select(device, value, gender, percent)
}


# Build long table for ALL 11 devices
gender_devices_long <- purrr::map_df(devices_keep, device_gender_long)

# Device order and vertical separator lines (used in both heatmaps)
device_order <- sort(unique(gender_devices_long$device))
n_dev  <- length(device_order)
vlines <- if (n_dev > 1) seq(1.5, n_dev - 0.5, by = 1) else numeric(0)

# Wide per device × value with one column per gender, then stable within-device ranks
ranked_data <- gender_devices_long %>%
  tidyr::pivot_wider(
    id_cols     = c(device, value),
    names_from  = gender,            # "Women", "Men"
    values_from = percent,
    values_fill = list(percent = 0)  # missing -> 0%
  ) %>%
  dplyr::rename(women = Women, men = Men) %>%
  dplyr::group_by(device) %>%
  dplyr::mutate(
    rank_women   = dense_rank(dplyr::desc(women)),
    rank_men     = dense_rank(dplyr::desc(men)),
    rank_diff    = rank_women - rank_men,  # positive = higher rank for Men
    percent_diff = women - men           # positive = higher % for Women
  ) %>%
  dplyr::ungroup()

# no filter heatmap

base_unfiltered <- ranked_data %>%
  mutate(
    women = coalesce(women, 0),
    men   = coalesce(men,   0),
    show_women = women > 0,
    show_men   = men > 0
  ) %>%
  filter(show_women | show_men) 

# plot full heatmap
plot_data_unf <- base_unfiltered %>%
  dplyr::mutate(device = factor(device, levels = device_order)) %>%
  tidyr::pivot_longer(
    cols = c(rank_women, rank_men),
    names_to = "gender", values_to = "rank"
  ) %>%
  dplyr::mutate(
    gender = dplyr::recode(gender, rank_women = "Women", rank_men = "Men"),
    show   = dplyr::case_when(gender == "Women" ~ show_women, TRUE ~ show_men),
    x_num  = as.numeric(device),
    x_pos  = ifelse(gender == "Women", x_num - 0.25, x_num + 0.25),
    fill   = ifelse(gender == "Women", "#55C667", "#5B2BCB")
  )

full_split_heat_map <- ggplot(plot_data_unf,
                              aes(x = x_pos, y = forcats::fct_rev(factor(value)))) +
  { if (length(vlines)) geom_vline(xintercept = vlines, color = "grey85", linewidth = 0.3) } +
  geom_tile(data = dplyr::filter(plot_data_unf, show),
            aes(fill = fill), width = 0.48, height = 0.9, show.legend = FALSE) +
  geom_text(data = dplyr::filter(plot_data_unf, show),
            aes(label = rank), colour = "white", 
            fontface = "bold", size = 3.5) +
  scale_x_continuous(breaks = seq_along(device_order),
                     labels = device_order, expand = c(0, 0)) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL,
       title = "Full comparison of women's and men's value rankings (all appliances)",
       subtitle = paste0(
         "Within-gender rank (1 = highest priority for that gender); all device\u2013value ",
         "combinations, no rank-difference filter.\n",
         "Green = Women | Purple = Men. A single coloured tile means only one gender mentioned that value."
       )) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(hjust = 0, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0, size = 13),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(size = 13, colour = "grey20"),  # value labels
    axis.text.x = element_text(size = 13, colour = "grey20", angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


print(full_split_heat_map)

ggsave(file.path(out_dir, "full_split_heat_map.png"),
  plot = full_split_heat_map, width = 13, height = 15, dpi = 300, bg = "white"
)

# Parameters
#use_percent_filter <- FALSE   # set TRUE to use a % threshold, FALSE to ignore it
#threshold_each     <- 0     # only used if use_percent_filter == TRUE
min_abs_rank_diff  <- 3


# Build the filtered base from stable ranks in `ranked_data`
base <- ranked_data %>%
  mutate(
    women = coalesce(women, 0),
    men   = coalesce(men,   0),
    show_women = women > 0,
    show_men   = men > 0
  ) %>%
  filter((show_women | show_men) & abs(rank_diff) > min_abs_rank_diff)


# Reshape for ggplot
plot_data <- base %>%
  dplyr::mutate(device = factor(device, levels = device_order)) %>%
  tidyr::pivot_longer(
    cols = c(rank_women, rank_men),
    names_to = "gender", 
    values_to = "rank"
  ) %>%
  dplyr::mutate(
    gender = dplyr::recode(gender, rank_women = "Women", rank_men = "Men"),
    show   = dplyr::case_when(gender == "Women" ~ show_women, TRUE ~ show_men),
    x_num  = as.numeric(device),
    x_pos  = ifelse(gender == "Women", x_num - 0.25, x_num + 0.25),
    fill   = ifelse(gender == "Women", "#55C667", "#5B2BCB")
  )


# subtitle reflects whether a % threshold is used
subtitle_txt <- paste0(
  "Within-gender rank (1 = highest priority for that gender); ",
  "shown only where the rank difference exceeds ", min_abs_rank_diff, ".\n",
  "Green = Women | Purple = Men. A single coloured tile means only one gender mentioned that value."
)


filtered_split_heat_map <- ggplot(plot_data,
                                  aes(x = x_pos, y = forcats::fct_rev(factor(value)))) +
  { if (length(vlines)) geom_vline(xintercept = vlines, color = "grey80", linewidth = 0.3) } +
  geom_tile(data = dplyr::filter(plot_data, show),
            aes(fill = fill), width = 0.48, height = 0.9, show.legend = FALSE) +
  
  geom_text(data = dplyr::filter(plot_data, show),
            aes(label = rank), colour = "white",
            fontface = "bold", size = 5) +
  
  scale_x_continuous(breaks = seq_along(device_order),
                     labels = device_order, expand = c(0, 0)) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL,
       title = "Where women's and men's value rankings diverge most",
       subtitle = subtitle_txt) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title    = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 13),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(size = 14, colour = "grey20"),  # value labels
    axis.text.x = element_text(size = 14, colour = "grey20", angle = 45, hjust = 1),
    panel.grid = element_blank()
  )



print(filtered_split_heat_map)

ggsave(
  file.path(out_dir, paste0("filtered_split_heat_map_gap", min_abs_rank_diff, ".png")),
  plot = filtered_split_heat_map, width = 13, height = 15, dpi = 300, bg = "white"
)



# --- Tile counts (focus on 'available') ---

# Theoretical tiles: every device–value pair * 2 genders (no filters)
n_tiles_theoretical <- nrow(ranked_data) * 2L

# Available tiles: tiles that actually exist in the data (> 0%) per gender
n_tiles_available <- with(ranked_data,
                          sum(dplyr::coalesce(women, 0) > 0) + sum(dplyr::coalesce(men, 0) > 0)
)

# Shown tiles: tiles actually drawn (after your current filter + show flag)
n_tiles_shown <- plot_data %>%
  dplyr::filter(show) %>%
  nrow()

# Not shown tiles (relative to AVAILABLE)
n_tiles_not_shown <- n_tiles_available - n_tiles_shown

cat("Theoretical tiles (all pairs x 2): ", n_tiles_theoretical, "\n", sep = "")
cat("Available tiles (>0%): ", n_tiles_available, "\n", sep = "")
cat("Shown tiles: ", n_tiles_shown, "\n", sep = "")
cat("Not shown tiles (of available): ", n_tiles_not_shown, "\n", sep = "")

cat("  Women: ", sum(coalesce(ranked_data$women, 0) > 0),
    " | Men: ", sum(coalesce(ranked_data$men, 0) > 0), "\n", sep = "")




