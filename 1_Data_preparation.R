

# script_1_Data_preparation


# Load Necessary Packages ------------------------------------------------------

library(tidyverse)
library(here)


Sys.setenv(LANG = "en")

options(scipen = 999)


stopifnot(
  file.exists(here::here("Quant_data", "Raw", "peoplesun_hh_anon.csv")),
  file.exists(here::here("Quant_data", "Raw", "peoplesun_hhapps_anon.csv"))
)

message("Project root: ", here::here())


# Upload data ------------------------------------------------------------

hh   <- read.csv(here::here("Quant_data","Raw","peoplesun_hh_anon.csv"))
apps <- read.csv(here::here("Quant_data","Raw","peoplesun_hhapps_anon.csv"))

if (any(is.na(hh$eaid))) stop("Missing EAIDs detected!")

# clean identifiers (EA as character) and standardize LGA spelling
hh <- hh %>%
  mutate(
    eaid = as.character(eaid),
    lga  = stringr::str_squish(stringr::str_to_lower(as.character(lga)))
  )

# For each EA: count households per LGA (n)
ea_lga_counts_long <- hh %>%
  count(eaid, lga, name = "n") %>%
  arrange(eaid, desc(n), lga)

# For each EA: pick the modal LGA by n; flag ties (50/50 etc.)
ea_lga_map <- ea_lga_counts_long %>%
  group_by(eaid) %>%
  arrange(desc(n), lga, .by_group = TRUE) %>%   # <-- important
  summarise(
    lga_ea   = first(lga),
    top_n    = first(n),
    second_n = dplyr::nth(n, 2, default = NA_integer_),
    tie      = !is.na(second_n) & top_n == second_n,
    .groups  = "drop"
  )


# Drop only tie-EAs, then overwrite lga with the EA-consistent LGA
hh <- hh %>%
  left_join(ea_lga_map %>% select(eaid, lga_ea, tie), by = "eaid") %>%
  filter(!tie) %>%                 # removes only ambiguous EAs (e.g., 8 vs 8)
  mutate(lga = lga_ea) %>%         # keep column name "lga"
  select(-lga_ea, -tie)

# Keep apps consistent with remaining households
apps <- apps %>%
  filter(hhid %in% hh$hhid)

# Quick sanity checks
cat("Households after fix:", dplyr::n_distinct(hh$hhid), "\n")
cat("EAs after fix:", dplyr::n_distinct(hh$eaid), "\n")

# After the fix, every EA should map to exactly 1 LGA:
check_after <- hh %>%
  group_by(eaid) %>%
  summarise(n_lga = n_distinct(lga), .groups = "drop") %>%
  count(n_lga)
print(check_after)

#how many EAs were dropped due to ties?
cat("Tie-EAs dropped:", sum(ea_lga_map$tie, na.rm = TRUE), "\n")


# Save cleaned base data so all other scripts use the same sample + corrected LGA
dir.create(here::here("Quant_data","New"), showWarnings = FALSE, recursive = TRUE)
saveRDS(hh,   here::here("Quant_data","New","hh_clean.rds"))
saveRDS(apps, here::here("Quant_data","New","apps_clean.rds")) # optional but recommended


# Data preperation: Dep Variable -----------------------------------------------------------

Apps <- apps %>%
  transmute(HHID = hhid,
            apps = q403,
            apps_num = q403_1)

# Check: any household × appliance duplicates?
dupes <- Apps %>% 
  count(HHID, apps) %>% 
  filter(n > 1)
cat("Duplicate HH-appliance rows:", nrow(dupes), "\n")

apps_wide <- Apps %>%
  pivot_wider(names_from = apps, values_from = apps_num) %>%
  select(HHID, everything())


apps_mixed <- apps_wide %>%
  transmute(HHID = HHID,
            dum_computer = ifelse(is.na(`Laptop / Computer`), NA, ifelse(`Laptop / Computer` >= 1, 1, 0)),
            computer = `Laptop / Computer`,
            dum_light_bulb = ifelse(is.na(`Light bulb`), NA, ifelse(`Light bulb` >= 1, 1, 0)),
            light_bulb = `Light bulb`,
            dum_mobile_phone_charger = ifelse(is.na(`Mobile phone charger`), NA, ifelse(`Mobile phone charger` >= 1, 1, 0)),
            mobile_phone_charger = `Mobile phone charger`,
            dum_radio = ifelse(is.na(Radio), NA, ifelse(Radio >= 1, 1, 0)),
            radio = Radio,
            dum_tv = ifelse(is.na(Television), NA, ifelse(Television >= 1, 1, 0)),
            tv = Television,
            dum_fan = ifelse(is.na(Fan), NA, ifelse(Fan >= 1, 1, 0)),
            fan = Fan
  )

to_dummy01 <- function(x) {
  ifelse(is.na(x), NA, ifelse(x == 1, 1, 0))
}

oth_app_dummy <- hh %>%
  transmute(HHID = hhid,
            dum_fridge = to_dummy01(q404__2),
            dum_wash_mach = to_dummy01(q404__3),
            dum_air_con = to_dummy01(q404__4),
            dum_water_heater = to_dummy01(q404__6),
            dum_cooker = to_dummy01(q404__7),
            dum_rice_cook = to_dummy01(q404__8),
            dum_sew_mach = to_dummy01(q404__9),
            dum_mech_apps = to_dummy01(q404__10),
            dum_therm_apps = to_dummy01(q404__11),
            dum_torch = ifelse(is.na(q301__7), NA, ifelse(q301__7 == 1, 1, 0)) 
  )


all_apps <- left_join(apps_mixed, oth_app_dummy, by = c("HHID"))


# Data preperation: Indep Variables ---------------------------------------


indep_var <- hh %>%
  transmute(HHID = hhid,
            eaid = eaid,
            weighting = natweight,
            lga = lga,
            high_edu_male = q208, 
            numhh_mem_young = q203_1,
            main_grid = if_else(q302 == 1, 1, 0, missing = NA_real_),
            years_grid = q307_1,
            hours_grid = q307_3,
            main_mini_g = if_else(q302 == 2, 1, 0, missing = NA_real_),
            years_mini_g = q306_1,
            hours_mini_g = q306_3,
            years_elec = tidyr::replace_na(coalesce(years_grid, years_mini_g), 0),
            main_shs = if_else(q302 == 3, 1, 0, missing = NA_real_),
            hours_shs = q304_1,
            main_gen = if_else(q302 == 5, 1, 0, missing = NA_real_),
            hours_gen = q305_1,
            age_self = q106,
            rel_head = q104,
            Age_HeadHousehold = q109,
            age_head = case_when(
              !is.na(Age_HeadHousehold) ~ Age_HeadHousehold,
              is.na(Age_HeadHousehold) & rel_head == 1 ~ age_self),
            total_hours_elec_all = if_else(
              is.na(hours_grid) & is.na(hours_mini_g) & is.na(hours_gen) & is.na(hours_shs),
              NA_real_,
              pmin(
              coalesce(hours_grid, 0) + 
                coalesce(hours_mini_g, 0) + 
                coalesce(hours_gen, 0) + 
                coalesce(hours_shs, 0),
              24
              )
            ),
            total_hours_elec_no_shs = if_else(
              is.na(hours_grid) & is.na(hours_mini_g) & is.na(hours_gen),
              NA_real_,
              coalesce(hours_grid, 0) + 
                coalesce(hours_mini_g, 0) + 
                coalesce(hours_gen, 0)
            )
             )


missing_summary <- colSums(is.na(indep_var))
print(missing_summary)


indep_var <- indep_var %>%
  mutate(male_edu_category = case_when(
    high_edu_male %in% c(1, 2, 24, 25, 0) ~ 0, # None
    high_edu_male >= 3 & high_edu_male <= 8 ~ 1, # Primary
    high_edu_male >= 9 & high_edu_male <= 11 ~ 2, # Secondary
    high_edu_male >= 12 & high_edu_male <= 16 ~ 3, # Higher Secondary
    high_edu_male >= 17 & high_edu_male <= 23 ~ 4, # Post-Secondary
    high_edu_male == 26 ~ 1, # Adult Education treated as Primary
    high_edu_male == 96 ~ NA_real_, # Other assumed NA
    TRUE ~ NA_real_
  ))


# Merge datasets ----------------------------------------------------------


Regression_ <- left_join(all_apps, indep_var, by = c("HHID"))

Regression_1 <- Regression_ %>% select(-age_self, -rel_head, -Age_HeadHousehold, -high_edu_male)

saveRDS(Regression_1, here::here("Quant_data","New","Regression_1.rds"))

