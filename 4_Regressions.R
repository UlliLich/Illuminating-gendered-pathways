
# script_4_Regressions
# Fits all models and saves them to Quant_data/New/all_results.rds.
# Tables and plots are produced from that file in script 5.

# Load necessary libraries
library(fixest)
library(survival)
library(marginaleffects)
library(sandwich)
library(dplyr)
library(car)
library(here)



# Set the working directory
Sys.setenv(LANG = "en")
options(scipen = 999)

stopifnot(file.exists(here::here("Quant_data", "New", "regression_data.rds")))
regression_data <- readRDS(here::here("Quant_data", "New", "regression_data.rds"))


stopifnot("lga" %in% names(regression_data))
stopifnot(sum(is.na(regression_data$lga)) == 0)
stopifnot("weighting" %in% names(regression_data))
stopifnot(sum(is.na(regression_data$weighting)) == 0)



# Multicollinearity check (VIF) ---------------------------------------------------------------------

X <- regression_data %>%
  dplyr::select(
    gen_head, total_hours_elec_all, wealth, male_edu_category,
    numhh_mem_young, age_head, years_elec
  ) %>%
  na.omit()

vif_model <- lm(gen_head ~ ., data = X) 
car::vif(vif_model)


X2 <- regression_data %>%
  dplyr::select(
    women_decision_index, total_hours_elec_all, wealth, male_edu_category,
    numhh_mem_young, age_head, years_elec
  ) %>%
  na.omit()

vif_model2 <- lm(women_decision_index ~ ., data = X2)
car::vif(vif_model2)



# Regression tables (Logit, Clogit, Poisson) + AME and APP for logit -------------------------------------------------------------


# Convert "dum_" variables to 0/1
dum_vars <- grep("^dum_", names(regression_data), value = TRUE)

for (v in dum_vars) {
  x <- suppressWarnings(as.numeric(as.character(regression_data[[v]])))
  
  if (any(!is.na(x) & !x %in% c(0,1))) {
    x <- ifelse(is.na(x), NA_integer_, ifelse(x >= 1, 1L, 0L))
  } else {
    x <- as.integer(x)
  }
  
  regression_data[[v]] <- x
}

stopifnot(all(sapply(regression_data[dum_vars], function(z) all(na.omit(z) %in% c(0L, 1L)))))



# Define devices
devices <- list("dum_mech_apps"            = c("logit", "clogit"),
                "dum_light_bulb"           = c("logit", "clogit"),
                "light_bulb"               = c("poisson"),
                "dum_computer"             = c("logit", "clogit"),
                "computer"                 = c("poisson"),
                "dum_fan"                  = c("logit", "clogit"),
                "fan"                      = c("poisson"),
                "dum_fridge"               = c("logit", "clogit"),
                "dum_therm_apps"           = c("logit", "clogit"),
                "dum_mobile_phone_charger" = c("logit", "clogit"),
                "mobile_phone_charger"     = c("poisson"),
                "dum_radio" = c("logit", "clogit"),
                "radio"     = c("poisson"),
                "dum_sew_mach"             = c("logit", "clogit"),
                "dum_torch"                = c("logit", "clogit"),
                "dum_tv"    = c("logit", "clogit"),
                "tv"        = c("poisson")
                )


# Four regression types
regression_types <- list(
  "women_decision_index_with_elec"         = "women_decision_index + total_hours_elec_all",
  "gen_head_with_elec"    = "gen_head + total_hours_elec_all",
  "women_decision_index_without_elec"      = "women_decision_index",
  "gen_head_without_elec" = "gen_head"
)


# Function to Run Models (Logit, Clogit, Poisson) With Weighting
run_models_for_device <- function(device_name, models, data, reg_type) {
  results <- list()
  
  for (model in models) {
    cat("Running", model, "model for device:", device_name, "and regression type:", reg_type, "\n")
    
    if (model == "logit") {
      
      # LOGIT with weighting
      
      model_name <- paste0(device_name, "_logit_", reg_type)
      results[[model_name]] <- try(
        feglm(
          as.formula(paste(device_name, "~", reg_type, 
                           "+ wealth + male_edu_category + numhh_mem_young + age_head + years_elec")),
          data     = data,
          family   = binomial(link = "logit"),
          cluster  = ~ lga,
          weights  = data$weighting
        ),
        silent = FALSE
      )
      
      # If model fits, compute AME and APP for logit
      if (!inherits(results[[model_name]], "try-error")) {
        ame_name <- paste0(model_name, "_AME")
        app_name <- paste0(model_name, "_APP")
        
        # Average Marginal Effect (AME)
        ame_result <- try(
          avg_slopes(
            results[[model_name]],
            variables  = if (grepl("\\bwomen_decision_index\\b", reg_type)) "women_decision_index" else "gen_head",
            conf_level = 0.95,
            vcov       = ~ lga,
            wts        = results[[model_name]]$weights
          ),
          silent = FALSE
        )
        results[[ame_name]] <- ame_result
        
        # Average Predicted Probability (APP) - weighted, matched to estimation sample
        app_result <- try({
          p <- predict(results[[model_name]], type = "response")
          w <- results[[model_name]]$weights   # weights used in the fitted model (same rows as p)
          weighted.mean(p, w = w, na.rm = TRUE)
        }, silent = FALSE)
        results[[app_name]] <- app_result
      } 
      
    } else if (model == "clogit") {
      
      # CLOGIT with weighting
      # (using method = "efron")
      # Note: We use frequency weights within clogit and then compute
      # cluster-robust standard errors post-estimation to account for it.
      
      model_name <- paste0(device_name, "_clogit_", reg_type)
      results[[model_name]] <- try({
        model_clogit <- clogit(
          as.formula(paste(device_name, "~", reg_type, 
                           "+ wealth + male_edu_category + numhh_mem_young + age_head + years_elec + strata(lga)")),
          data     = data,
          method   = "efron",    # efron or breslow
          weights  = data$weighting
        )
        model_clogit
      }, silent = FALSE)
      
      # Store the clustered variance-covariance matrix if it fits
      if (!inherits(results[[model_name]], "try-error")) {
        vcov_name <- paste0(model_name, "_vcov")
        
        
        v <- try(sandwich::vcovCL(results[[model_name]], cluster = ~ lga), silent = TRUE)
        results[[vcov_name]] <- if (inherits(v, "try-error")) NULL else v
        
      }
      
      # No AME/APP for clogit in this script, but you could add if needed.
      
    } else if (model == "poisson" && device_name %in% c("radio","tv","computer","mobile_phone_charger","fan","light_bulb")) {
      
      # POISSON with weighting
      
      model_name <- paste0(device_name, "_poisson_", reg_type)
      results[[model_name]] <- try(
        fepois(
          as.formula(paste(device_name, "~", reg_type,
                           "+ wealth + male_edu_category + numhh_mem_young + age_head + years_elec | lga")),
          data     = data,
          cluster  = ~ lga,
          weights  = data$weighting
        ),
        silent = FALSE
      )
      # No AME/APP for Poisson
    }
  }
  
  return(results)
}


# Run All Models for All Devices + Regression Types and store results
all_results <- list()
for (device in names(devices)) {
  for (reg_type in names(regression_types)) {
    cat("Running models for device:", device, "and regression type:", reg_type, "\n")
    
    if (is.null(all_results[[device]])) {
      all_results[[device]] <- list()
    }
    
    all_results[[device]][[reg_type]] <-
      run_models_for_device(
        device,
        devices[[device]],
        regression_data,   # pass your dataset
        regression_types[[reg_type]]
      )
  }
}

# ---- Save fitted models for the output script ----
saveRDS(all_results, here::here("Quant_data","New","all_results.rds"))



