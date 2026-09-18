
# script_3_Descriptives


# Load Necessary Packages ------------------------------------------------------

library(tidyverse)
library(survey)
library(flextable)
library(xtable)
library(here)




#set working directory

Sys.setenv(LANG = "en")

options(scipen = 999)

stopifnot(file.exists(here::here("Quant_data","New","Regression_1.rds")))

out_dir <- here::here("Output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

Regression_1 <- readRDS(here::here("Quant_data","New","Regression_1.rds"))



# Summary stats (weighted)--------------------------------------------------

df_desc <- Regression_1 %>%
  transmute(HHID = HHID,
            computer = dum_computer,
            bulb = dum_light_bulb,
            charger = dum_mobile_phone_charger,
            radio = dum_radio,
            tv = dum_tv,
            fan = dum_fan,
            fridge = dum_fridge,
            wash_mach = dum_wash_mach,
            air_con = dum_air_con,
            cooker = dum_cooker,
            rice_cooker = dum_rice_cook,
            sew_mach = dum_sew_mach,
            mech_app = dum_mech_apps,
            therm_app = dum_therm_apps,
            torch = dum_torch,
            high_edu_male = male_edu_category,
            numhh_mem_young = numhh_mem_young,
            main_grid = main_grid,
            years_elec = years_elec,
            hours_grid = hours_grid,
            main_mini_g = main_mini_g,
            hours_mini_g = hours_mini_g,  
            main_shs = main_shs,
            hours_shs = hours_shs,
            main_gen = main_gen,
            hours_gen = hours_gen,
            weighting = weighting   # Add the weighting variable here
  )

# survey design

des <- svydesign(ids = ~1, data = df_desc, weights = ~weighting)

# Helper functions (parametrised by design)
wtd_pct <- function(design, var_name) {
  m <- svymean(as.formula(paste0("~", var_name)), design, na.rm = TRUE)
  paste0(round(as.numeric(coef(m)) * 100, 1), "%")
}

wtd_mean_sd <- function(design, var_name) {
  m <- svymean(as.formula(paste0("~", var_name)), design, na.rm = TRUE)
  v <- svyvar(as.formula(paste0("~", var_name)), design, na.rm = TRUE)
  paste0(round(as.numeric(coef(m)), 1), " (", round(sqrt(as.numeric(coef(v))), 1), ")")
}

n_nonmiss <- function(df, var_name) {
  sum(!is.na(df[[var_name]]))
}

sanitize_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("(?<!\\\\)%", "\\\\%", x, perl = TRUE)
  x <- gsub("(?<!\\\\)#", "\\\\#", x, perl = TRUE)
  x <- gsub("(?<!\\\\)_", "\\\\_", x, perl = TRUE)
  x <- gsub("(?<!\\\\)&", "\\\\&", x, perl = TRUE)
  x
}



appliance_small <- c(
  "computer","bulb","charger","radio","tv","fan","fridge",
  "sew_mach","mech_app","therm_app","torch"
)

elec_sources <- c("main_grid","main_mini_g","main_shs","main_gen")

# Check: binary vars are really 0/1/NA
binary_vars <- c(appliance_small, elec_sources)
for (v in binary_vars) {
  ok <- all(na.omit(df_desc[[v]]) %in% c(0, 1))
  stopifnot(ok)
}


# compute stats


wa_small <- sapply(appliance_small, function(v) wtd_pct(des, v))
we       <- sapply(elec_sources,    function(v) wtd_pct(des, v))



years_elec_str <- wtd_mean_sd(des, "years_elec")
hours_grid_str <- wtd_mean_sd(des, "hours_grid")
hours_gen_str  <- wtd_mean_sd(des, "hours_gen")
hours_mini_str <- wtd_mean_sd(des, "hours_mini_g")
hours_shs_str  <- wtd_mean_sd(des, "hours_shs")


# --------- SMALL table (LaTeX export)------

tab_small <- data.frame(
  Variable = c(
    "Household appliances",
    "  Light bulb","  Mobile phone charger","  Fan","  TV","  Fridge",
    "  Thermal appliance","  Torch","  Radio","  Mechanical appliance",
    "  Computer","  Sewing machine",
    "Electricity sources",
    "  Main source: grid (%)",
    "  Years grid/mini grid access: mean (std. dev.)",
    "  Hours daily grid access: mean (std. dev.)",
    "  Main source: generator (%)",
    "  Hours daily generator access: mean (std. dev.)",
    "  Main source: mini grid (%)",
    "  Hours daily mini grid access: mean (std. dev.)",
    "  Main source: SHS (%)",
    "  Hours daily SHS access: mean (std. dev.)"
  ),
  Specification = c(
    "",
    wa_small["bulb"], wa_small["charger"], wa_small["fan"], wa_small["tv"], wa_small["fridge"],
    wa_small["therm_app"], wa_small["torch"], wa_small["radio"], wa_small["mech_app"],
    wa_small["computer"], wa_small["sew_mach"],
    "",
    we["main_grid"],
    years_elec_str,
    hours_grid_str,
    we["main_gen"],
    hours_gen_str,
    we["main_mini_g"],
    hours_mini_str,
    we["main_shs"],
    hours_shs_str
  ),
  stringsAsFactors = FALSE
)

df_tex <- tab_small
df_tex$Variable <- trimws(df_tex$Variable)

is_heading <- is.na(df_tex$Specification) | df_tex$Specification == ""
df_tex$Variable[is_heading] <- paste0("\\textbf{", df_tex$Variable[is_heading], "}")
df_tex$Specification[is_heading] <- ""

idx <- which(is_heading)
hline_after <- sort(unique(c(-1, idx, idx[idx > 1] - 1, nrow(df_tex))))

xt <- xtable(
  df_tex,
  caption = "Weighted summary statistics energy access and appliance ownership (n = 3,583 households)",
  label   = "tab:descriptives",
  align   = c("l", "l", "r")
)

print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = sanitize_latex,
  hline.after = hline_after,
  floating = TRUE,
  table.placement = "!htbp",
  caption.placement = "top",
  file = file.path(out_dir, "table_descriptives.tex")
)

# Additional table (not included in the paper) ---------------------------------
# Full appliance list incl. low-prevalence devices (air conditioner, washing
# machine, electric cooker, rice cooker) and number of observations per mean.
# Shown for transparency; the paper reports the reduced table above.


# FULL table (with "Other household appliances" + N rows)
appliance_full <- c(
  "computer", "bulb", "charger", "radio", "tv", "fan", "fridge",
  "wash_mach", "air_con", "cooker", "rice_cooker",
  "sew_mach", "mech_app", "therm_app", "torch"
)

wa_full  <- sapply(appliance_full,  function(v) wtd_pct(des, v))

# Ns (unweighted counts of non-missing)
n_years_elec <- n_nonmiss(df_desc, "years_elec")
n_hours_grid <- n_nonmiss(df_desc, "hours_grid")
n_hours_gen  <- n_nonmiss(df_desc, "hours_gen")
n_hours_mini <- n_nonmiss(df_desc, "hours_mini_g")
n_hours_shs  <- n_nonmiss(df_desc, "hours_shs")


# ---- FULL flextable------


tab_full <- data.frame(
  Variable = c(
    "Household appliances discussed",
    "  Light bulb","  Mobile phone charger","  Fan","  TV","  Fridge",
    "  Thermal appliance","  Torch","  Radio","  Mechanical appliance",
    "  Computer","  Sewing machine",
    "Other household appliances",
    "  Air conditioner","  Washing machine","  Electric cooker","  Rice cooker",
    "Electricity sources",
    "  Main source: grid (%)",
    "  Years grid/mini grid access: mean (std. dev.)","  # observations",
    "  Hours daily grid access: mean (std. dev.)","  # observations",
    "  Main source: generator (%)",
    "  Hours daily generator access: mean (std. dev.)","  # observations",
    "  Main source: mini grid (%)",
    "  Hours daily mini grid access: mean (std. dev.)","  # observations",
    "  Main source: SHS (%)",
    "  Hours daily SHS access: mean (std. dev.)","  # observations"
  ),
  Specification = c(
    "",
    wa_full["bulb"], wa_full["charger"], wa_full["fan"], wa_full["tv"], wa_full["fridge"],
    wa_full["therm_app"], wa_full["torch"], wa_full["radio"], wa_full["mech_app"],
    wa_full["computer"], wa_full["sew_mach"],
    "",
    wa_full["air_con"], wa_full["wash_mach"], wa_full["cooker"], wa_full["rice_cooker"],
    "",
    we["main_grid"],
    years_elec_str, n_years_elec,
    hours_grid_str, n_hours_grid,
    we["main_gen"],
    hours_gen_str,  n_hours_gen,
    we["main_mini_g"],
    hours_mini_str, n_hours_mini,
    we["main_shs"],
    hours_shs_str,  n_hours_shs
  ),
  stringsAsFactors = FALSE
)

ft_full <- flextable(tab_full) %>%
  autofit() %>%
  theme_vanilla() %>%
  delete_part(part = "header")

print(ft_full)


# Optional: export this table as well
#write.csv(tab_full, file.path(out_dir, "table_descriptives_full.csv"), row.names = FALSE)

