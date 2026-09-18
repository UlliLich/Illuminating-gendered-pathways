

# script_5_outputs: Tables and plots from ONE single source

# Prerequisite: script_4 has been run and saved all_results.rds

# Load libraries
library(dplyr)
library(marginaleffects)
library(sandwich)
library(flextable)     
library(officer)       
library(ggplot2)
library(scales)
library(xtable)
library(here)
library(broom)

# Set the working directory
Sys.setenv(LANG = "en")
options(scipen = 999)


stopifnot(file.exists(here::here("Quant_data","New","all_results.rds")))

out_dir <- here::here("Output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Load the fitted models produced by script_4
all_results <- readRDS(here::here("Quant_data","New","all_results.rds"))


# ------- Constants (defined once, used by both tables and plots) ----
term_order <- c("women_decision_index", "gen_head", "total_hours_elec_all",
                "wealth", "male_edu_category", "numhh_mem_young",
                "age_head", "years_elec")

reg_types_order <- c("women_decision_index_with_elec", "gen_head_with_elec",
                     "women_decision_index_without_elec", "gen_head_without_elec")

device_order <- c("mech_apps", "light_bulb", "computer", "fan", "fridge",
                  "therm_apps", "mobile_phone_charger", "radio", "sew_mach",
                  "torch", "tv")

# Regression specification to summarise (without the electricity control)
rt_focus <- "women_decision_index_without_elec"

#elec_colors <- c("With Elec" = "#3FA0FF", "Without Elec" = "#F76D5E")

#elec_colors <- c("With Elec" = "#FDE725FF", "Without Elec" = "#404788FF")

elec_colors <- c("With Elec" = "#FDB130", "Without Elec" = "#D7566C")

#elec_colors <- c("With Elec" = "#55C667FF", "Without Elec" = "#481567FF")

device_plot_order <- c("tv", "torch", "sew_mach", "radio", "mobile_phone_charger",
                       "therm_apps", "fridge", "fan", "computer", "light_bulb",
                       "mech_apps"
                      )
                       

# --------- Master data frames--------
# (One tidy() call per model with the clustered vcov. Estimates and CIs are
# already exponentiated (odds ratios / IRRs). Both tables and plots read
# from these three data frames, so they can never diverge.)

build_master_df <- function(all_results, model_tag, clustered) {
  out <- list()
  for (device in names(all_results)) {
    for (rt in names(all_results[[device]])) {
      res <- all_results[[device]][[rt]]
      nms <- grep(paste0("_", model_tag, "_"), names(res), value = TRUE)
      nms <- nms[!grepl("_vcov|_AME|_APP", nms)]
      for (mn in nms) {
        m <- res[[mn]]
        if (inherits(m, "try-error") || is.null(m)) next
        # fixest models (logit/poisson) already carry the clustered vcov in vcov();
        # clogit (survival) needs the clustered vcov applied here.
        V <- if (clustered) {
          tryCatch(sandwich::vcovCL(m, cluster = ~ lga), error = function(e) vcov(m))
        } else {
          vcov(m)
        }
        td <- marginaleffects::tidy(m, conf.int = TRUE, conf.level = 0.95, vcov = V)
        td <- td %>%
          mutate(
            estimate  = exp(estimate),
            conf.low  = exp(conf.low),
            conf.high = exp(conf.high),
            device    = gsub("dum_", "", device),
            reg_type  = rt,
            model_name = mn,
            significance = case_when(
              p.value < .001 ~ "***",
              p.value < .01  ~ "**",
              p.value < .05  ~ "*",
              p.value < .1   ~ "+",
              TRUE           ~ ""
            ),
            show_star = (conf.low > 1 + 1e-6 | conf.high < 1 - 1e-6)
          )
        out[[length(out) + 1]] <- td
      }
    }
  }
  bind_rows(out)
}

master_logit  <- build_master_df(all_results, "logit",   clustered = FALSE)
master_clogit <- build_master_df(all_results, "clogit",  clustered = TRUE)
master_pois   <- build_master_df(all_results, "poisson", clustered = FALSE)



# ------- Build tables --------


# Keep only terms that are actually present, in the desired order
order_terms <- function(d) {
  present <- term_order[term_order %in% d$term]
  setNames(d$cell, d$term)[present]
}


# Number of observations actually used by a fitted model
get_nobs <- function(all_results, device, rt, mn) {
  m <- all_results[[device]][[rt]][[mn]]
  if (is.null(m)) return(NA_character_)
  # clogit/coxph: stats::nobs works
  if (inherits(m, "coxph")) {
    n <- tryCatch(stats::nobs(m), error = function(e) NA)
  } else {
    # fixest (logit/poisson): count used residuals -> robust across versions
    n <- tryCatch(length(stats::residuals(m)), error = function(e) NA)
  }
  as.character(n)
}

# One combined Logit + Clogit table for a given device and regression type
make_combo_ft <- function(master_logit, master_clogit, all_results, device_clean, rt) {
  cols <- list(); nobs_row <- c()
  for (tag in c("logit", "clogit")) {
    df <- if (tag == "logit") master_logit else master_clogit
    d <- df %>% filter(device == device_clean, reg_type == rt)
    if (nrow(d) == 0) next
    mn <- d$model_name[1]
    dev_orig <- if (paste0("dum_", device_clean) %in% names(all_results)) paste0("dum_", device_clean) else device_clean
    d <- d %>% mutate(cell = sprintf("%.3f%s\n[%.3f, %.3f]", estimate, significance, conf.low, conf.high))
    cols[[paste0(device_clean, "_", tag)]] <- setNames(d$cell, d$term)
    nobs_row <- c(nobs_row, setNames(as.character(get_nobs(all_results, dev_orig, rt, mn)),
                                     paste0(device_clean, "_", tag)))
  }
  if (length(cols) == 0) return(NULL)
  
  # union of terms across the columns, in term_order order
  all_terms <- unique(unlist(lapply(cols, names)))
  ord <- term_order[term_order %in% all_terms]
  tab <- data.frame(term = ord, stringsAsFactors = FALSE)
  for (cn in names(cols)) tab[[cn]] <- cols[[cn]][ord]
  
  # append N row with exactly as many entries as there are columns
  nobs_vals <- sapply(names(cols), function(cn) nobs_row[[cn]])
  tab[nrow(tab) + 1, ] <- c("Num.Obs.", nobs_vals)
  
  flextable(tab) %>% autofit()
}

# One Poisson table for a given device and regression type
make_pois_ft <- function(master_pois, all_results, device_clean, rt) {
  d <- master_pois %>% filter(device == device_clean, reg_type == rt)
  if (nrow(d) == 0) return(NULL)
  mn <- d$model_name[1]
  
  # Poisson models are stored under the PLAIN device name (e.g. "radio"),
  # not under "dum_radio" -> use device_clean directly for the N lookup.
  nval <- as.character(get_nobs(all_results, device_clean, rt, mn))
  
  d <- d %>% mutate(cell = sprintf("%.3f%s\n[%.3f, %.3f]", estimate, significance, conf.low, conf.high))
  vec <- order_terms(d)
  
  tab <- data.frame(term = names(vec), value = as.character(vec), stringsAsFactors = FALSE)
  tab <- rbind(tab, data.frame(term = "Num.Obs.", value = nval, stringsAsFactors = FALSE))
  names(tab) <- c("term", paste0(device_clean, "_poisson"))
  flextable(tab) %>% autofit()
}

# AME / APP block (logit only), taken unchanged from all_results
add_ame <- function(doc, res, rt) {
  ame_nm <- grep("_logit_.*_AME$", names(res), value = TRUE)
  if (length(ame_nm) != 1 || inherits(res[[ame_nm]], "try-error") || is.null(res[[ame_nm]])) return(doc)
  app_nm <- sub("_AME$", "_APP", ame_nm)
  ame_v <- round(res[[ame_nm]]$estimate[1], 3)
  app_v <- if (app_nm %in% names(res) && !inherits(res[[app_nm]], "try-error")) round(res[[app_nm]], 3) else NA
  
  # derive a clean model label, e.g. "dum_radio_logit_gen_head_AME" -> "radio_logit"
  model_lab <- sub("_AME$", "", ame_nm)
  model_lab <- gsub("dum_", "", model_lab)
  model_lab <- sub("_logit_.*", "_logit", model_lab)
  
  var_lab <- if (grepl("women_decision", rt)) "Decision-making index" else "Gender of household head"
  ame_df <- data.frame(
    Model     = model_lab,
    Statistic = c("Average Marginal Effect (AME)", "Average Predicted Probability (APP)"),
    Value     = c(ame_v, app_v),
    stringsAsFactors = FALSE
  )
  if (is.na(app_v)) ame_df <- ame_df[1, , drop = FALSE]
  
  doc %>%
    body_add_par(paste("Average Marginal Effects and Predicted Probabilities for", var_lab, "(Logit)"),
                 style = "heading 2") %>%
    body_add_flextable(flextable(ame_df) %>% autofit()) %>%
    body_add_par(" ", style = "Normal")
}

# Build the full Word document
build_doc <- function(master_logit, master_clogit, master_pois, all_results, output_file) {
  doc <- read_docx()
  for (device in names(all_results)) {
    if (!grepl("^dum_", device)) next   # skip the Poisson-only list entries to avoid duplicates
    dc <- gsub("dum_", "", device)
    for (rt in reg_types_order) {
      res <- all_results[[device]][[rt]]; if (is.null(res)) next
      
      ft <- make_combo_ft(master_logit, master_clogit, all_results, dc, rt)
      if (!is.null(ft)) {
        doc <- doc %>%
          body_add_par(paste0("Device: ", dc, " | Regression Type: ", rt,
                              " | Model Type: Logit and Clogit Combined (Odds Ratios & Confidence Intervals)"),
                       style = "heading 1") %>%
          body_add_flextable(ft) %>% body_add_par(" ", style = "Normal")
        doc <- add_ame(doc, res, rt)
      }
      
      ftp <- make_pois_ft(master_pois, all_results, dc, rt)
      if (!is.null(ftp)) {
        doc <- doc %>%
          body_add_par(paste0("Device: ", dc, " | Regression Type: ", rt,
                              " | Model Type: Poisson (Incidence Rate Ratios & Confidence Intervals)"),
                       style = "heading 1") %>%
          body_add_flextable(ftp) %>% body_add_par(" ", style = "Normal")
      }
    }
  }
  print(doc, target = output_file)
}

build_doc(master_logit, master_clogit, master_pois, all_results,
          file.path(out_dir, "Regression_results_AME_APP_clean.docx"))


# ------------- Plots ----------------------
# (The plotting functions take a master df as input and filter to the
#    variable of interest, so the plotted points/CIs are identical to the table.)


#  Logit / Clogit odds-ratio plot
plot_or_combined <- function(data, variable, title_suffix, model_label) {
  plot_data_with <- data %>%
    filter(term == variable, reg_type == paste0(variable, "_with_elec")) %>%
    mutate(elec_condition = "With Elec")
  plot_data_without <- data %>%
    filter(term == variable, reg_type == paste0(variable, "_without_elec")) %>%
    mutate(elec_condition = "Without Elec")
  if (nrow(plot_data_with) == 0 || nrow(plot_data_without) == 0) return(NULL)
  
  all_devices <- device_plot_order[device_plot_order %in%
                                     unique(c(plot_data_with$device, plot_data_without$device))]
  plot_data_with$device    <- factor(plot_data_with$device,    levels = all_devices)
  plot_data_without$device <- factor(plot_data_without$device, levels = all_devices)
  
  scale_factor <- 15
  d_with    <- plot_data_with    %>% mutate(d_num = as.numeric(device), d_base = d_num * scale_factor,
                                            y_point = d_base + 2, y_star = d_base + 4)
  d_without <- plot_data_without %>% mutate(d_num = as.numeric(device), d_base = d_num * scale_factor,
                                            y_point = d_base - 4, y_star = d_base - 8)
  
  x_min <- max(min(c(d_with$conf.low, d_without$conf.low), na.rm = TRUE) * 0.9, 0.5)
  x_max <- max(c(d_with$conf.high, d_without$conf.high), na.rm = TRUE) * 1.1
  
  ggplot() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
    geom_errorbarh(data = d_with,
                   aes(xmin = conf.low, xmax = conf.high, y = y_point, color = elec_condition), height = 0.2) +
    geom_point(data = d_with, aes(x = estimate, y = y_point, color = elec_condition), size = 3) +
    geom_text(data = d_with %>% filter(show_star & significance != ""),
              aes(x = estimate, y = y_star, label = significance, color = elec_condition),
              size = 5, show.legend = FALSE) +
    geom_errorbarh(data = d_without,
                   aes(xmin = conf.low, xmax = conf.high, y = y_point, color = elec_condition), height = 0.2) +
    geom_point(data = d_without, aes(x = estimate, y = y_point, color = elec_condition), size = 3) +
    geom_text(data = d_without %>% filter(show_star & significance != ""),
              aes(x = estimate, y = y_star, label = significance, color = elec_condition),
              size = 5, show.legend = FALSE) +
    scale_color_manual(values = elec_colors) +
    scale_x_continuous(trans = "log10", breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3),
                       labels = scales::label_number(accuracy = 0.01)) +
    scale_y_continuous(breaks = seq_along(all_devices) * scale_factor, labels = all_devices) +
    labs(title = paste(model_label, "effect of", variable, title_suffix),
         x = "Odds Ratio", y = "Device", color = "Condition") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border = element_blank(),
          axis.text.y = element_text(face = "bold", size = 10),
          plot.title = element_text(margin = margin(b = 15))) +
    coord_cartesian(xlim = c(x_min, x_max))
}

# Poisson IRR plot (same layout, different x-axis label) 
plot_irr_combined <- function(data, variable, title_suffix) {
  plot_data_with <- data %>%
    filter(term == variable, reg_type == paste0(variable, "_with_elec")) %>%
    mutate(elec_condition = "With Elec")
  plot_data_without <- data %>%
    filter(term == variable, reg_type == paste0(variable, "_without_elec")) %>%
    mutate(elec_condition = "Without Elec")
  if (nrow(plot_data_with) == 0 || nrow(plot_data_without) == 0) return(NULL)
  
  all_devices <- device_plot_order[device_plot_order %in%
                                     unique(c(plot_data_with$device, plot_data_without$device))]
  plot_data_with$device    <- factor(plot_data_with$device,    levels = all_devices)
  plot_data_without$device <- factor(plot_data_without$device, levels = all_devices)
  
  scale_factor <- 15
  d_with    <- plot_data_with    %>% mutate(d_num = as.numeric(device), d_base = d_num * scale_factor,
                                            y_point = d_base + 2, y_star = d_base + 4)
  d_without <- plot_data_without %>% mutate(d_num = as.numeric(device), d_base = d_num * scale_factor,
                                            y_point = d_base - 4, y_star = d_base - 8)
  
  x_min <- max(min(c(d_with$conf.low, d_without$conf.low), na.rm = TRUE) * 0.9, 0.5)
  x_max <- max(c(d_with$conf.high, d_without$conf.high), na.rm = TRUE) * 1.1
  
  ggplot() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
    geom_errorbarh(data = d_with,
                   aes(xmin = conf.low, xmax = conf.high, y = y_point, color = elec_condition), height = 0.2) +
    geom_point(data = d_with, aes(x = estimate, y = y_point, color = elec_condition), size = 3) +
    geom_text(data = d_with %>% filter(show_star & significance != ""),
              aes(x = estimate, y = y_star, label = significance, color = elec_condition),
              size = 5, show.legend = FALSE) +
    geom_errorbarh(data = d_without,
                   aes(xmin = conf.low, xmax = conf.high, y = y_point, color = elec_condition), height = 0.2) +
    geom_point(data = d_without, aes(x = estimate, y = y_point, color = elec_condition), size = 3) +
    geom_text(data = d_without %>% filter(show_star & significance != ""),
              aes(x = estimate, y = y_star, label = significance, color = elec_condition),
              size = 5, show.legend = FALSE) +
    scale_color_manual(values = elec_colors) +
    scale_x_continuous(trans = "log10", breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 3),
                       labels = scales::label_number(accuracy = 0.01)) +
    scale_y_continuous(breaks = seq_along(all_devices) * scale_factor, labels = all_devices) +
    labs(title = paste("Poisson effect of", variable, title_suffix),
         x = "Incidence Rate Ratio (IRR)", y = "Device", color = "Condition") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border = element_blank(),
          axis.text.y = element_text(face = "bold", size = 10),
          plot.title = element_text(margin = margin(b = 15))) +
    coord_cartesian(xlim = c(x_min, x_max))
}

# ---- Build and save the six plots
logit_emp_plot      <- plot_or_combined(master_logit,  "women_decision_index", "on device ownership", "Logit")
logit_genhead_plot  <- plot_or_combined(master_logit,  "gen_head",             "on device ownership", "Logit")
clogit_emp_plot     <- plot_or_combined(master_clogit, "women_decision_index", "on device ownership", "Clogit")
clogit_genhead_plot <- plot_or_combined(master_clogit, "gen_head",             "on device ownership", "Clogit")
pois_emp_plot       <- plot_irr_combined(master_pois,  "women_decision_index", "on device ownership")
pois_genhead_plot   <- plot_irr_combined(master_pois,  "gen_head",             "on device ownership")

ggsave(file.path(out_dir, "combined_logit_emp_plot.png"),        logit_emp_plot,      width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "combined_logit_gen_head_plot.png"),   logit_genhead_plot,  width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "combined_clogit_emp_plot.png"),       clogit_emp_plot,     width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "combined_clogit_gen_head_plot.png"),  clogit_genhead_plot, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "combined_poisson_emp_plot.png"),      pois_emp_plot,       width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "combined_poisson_gen_head_plot.png"), pois_genhead_plot,   width = 8, height = 6, dpi = 300)



# ------- Control variables: --------
# build overview table (Logit/Clogit/Poisson)
# print word table


# Variables to summarise: main predictor first, then the controls
control_vars <- c("women_decision_index",   # main predictor
                  "wealth", "male_edu_category", "numhh_mem_young",
                  "age_head", "years_elec")



# Helper: format one cell as "OR* [low, high]" (for Logit/Clogit) or
# "IRR* [low, high]" (for Poisson) — same formatting either way
fmt <- function(est, lo, hi, sig) {
  if (is.na(est)) return("")
  sprintf("%.3f%s [%.3f, %.3f]", est, sig, lo, hi)
}

# Build one overview table for a given variable: Logit, Clogit, Poisson
build_control_table <- function(var_name, rt = rt_focus) {
  rows <- lapply(device_order, function(dev) {
    lg <- master_logit  %>% filter(device == dev, reg_type == rt, term == var_name)
    cl <- master_clogit %>% filter(device == dev, reg_type == rt, term == var_name)
    po <- master_pois   %>% filter(device == dev, reg_type == rt, term == var_name)
    data.frame(
      Device  = dev,
      Logit   = if (nrow(lg) == 1) fmt(lg$estimate, lg$conf.low, lg$conf.high, lg$significance) else "",
      Clogit  = if (nrow(cl) == 1) fmt(cl$estimate, cl$conf.low, cl$conf.high, cl$significance) else "",
      Poisson = if (nrow(po) == 1) fmt(po$estimate, po$conf.low, po$conf.high, po$significance) else "—",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Build all tables into one Word document
doc_ctrl <- read_docx()

for (v in control_vars) {
  tab <- build_control_table(v)
  ft  <- flextable(tab) %>% autofit()
  
  heading <- if (v == "women_decision_index") {
    "Main predictor: women's decision-making index (Odds Ratios / Incidence Rate Ratios with 95% CI; specification without electricity control)"
  } else {
    paste0("Control variable: ", v,
           " (Odds Ratios / Incidence Rate Ratios with 95% CI; women_decision_index_without_elec)")
  }
  
  doc_ctrl <- doc_ctrl %>%
    body_add_par(heading, style = "heading 1") %>%
    body_add_flextable(ft) %>%
    body_add_par(" ", style = "Normal")
}

print(doc_ctrl, target = file.path(out_dir, "Control_variable_overview.docx"))




# ------- Selected control variables: male education and household wealth --------
# build latex table (Logit, Clgoti, Poisson)


device_labels <- c(
  radio = "Radio", tv = "TV", computer = "Computer",
  mobile_phone_charger = "Mobile phone charger", fan = "Fan",
  light_bulb = "Light bulb", fridge = "Fridge", torch = "Torch",
  sew_mach = "Sewing machine", mech_apps = "Mechanical appliances",
  therm_apps = "Thermal appliances"
)


# format one cell as "OR^{*} [low, high]" (LaTeX-friendly stars)
fmt_tex <- function(est, lo, hi, sig) {
  if (is.na(est)) return("")
  star <- ""
  if (!is.na(sig) && nzchar(sig)) {
    star <- paste0("$^{", gsub("\\*", "*", sig), "}$")
  }
  sprintf("%.3f%s [%.3f, %.3f]", est, star, lo, hi)
}

build_control_df <- function(var_name, rt = rt_focus) {
  do.call(rbind, lapply(device_order, function(dev) {
    lg <- master_logit  %>% filter(device == dev, reg_type == rt, term == var_name)
    cl <- master_clogit %>% filter(device == dev, reg_type == rt, term == var_name)
    po <- master_pois   %>% filter(device == dev, reg_type == rt, term == var_name)
    data.frame(
      Appliance = device_labels[[dev]],
      Logit   = if (nrow(lg) == 1) fmt_tex(lg$estimate, lg$conf.low, lg$conf.high, lg$significance) else "",
      Clogit  = if (nrow(cl) == 1) fmt_tex(cl$estimate, cl$conf.low, cl$conf.high, cl$significance) else "",
      Poisson = if (nrow(po) == 1) fmt_tex(po$estimate, po$conf.low, po$conf.high, po$significance) else "---",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
}

write_control_tex <- function(var_name, file, caption, label) {
  df <- build_control_df(var_name)
  colnames(df) <- c("Appliance", "Logit", "Conditional logit", "Poisson")
  
  xt <- xtable(
    df,
    caption = caption,
    label   = label,
    align   = c("l", "l", "l", "l", "l")   # one extra "l" for the Poisson column
  )
  
  print(
    xt,
    file = file,
    include.rownames = FALSE,
    sanitize.text.function     = identity,
    sanitize.colnames.function = function(x) paste0("\\textbf{", x, "}"),
    hline.after       = c(-1, 0, nrow(df)),
    floating          = TRUE,
    table.placement   = "!htbp",
    caption.placement = "top"
  )
}

write_control_tex(
  "male_edu_category",
  file    = file.path(out_dir, "control_male_edu.tex"),
  caption = "Association between highest education of a man in the household and appliance ownership across devices (odds ratios for logit/conditional logit and incidence rate ratios for Poisson, with 95\\% confidence intervals). Estimates from weighted models with LGA-clustered standard errors; specification without the total daily electricity access control. Poisson models are estimated only for appliances with available unit counts; remaining cells are marked ``---''. Coefficients of control variables are reported for transparency and are not interpreted causally. $^{+}p<.1$, $^{*}p<.05$, $^{**}p<.01$, $^{***}p<.001$.",
  label   = "sup:tab:control_male_edu"
)

write_control_tex(
  "wealth",
  file    = file.path(out_dir, "control_wealth.tex"),
  caption = "Association between the household wealth index and appliance ownership across devices (odds ratios for logit/conditional logit and incidence rate ratios for Poisson, with 95\\% confidence intervals). Estimates from weighted models with LGA-clustered standard errors; specification without the total daily electricity access control. Poisson models are estimated only for appliances with available unit counts; remaining cells are marked ``---''. Coefficients of control variables are reported for transparency and are not interpreted causally. $^{+}p<.1$, $^{*}p<.05$, $^{**}p<.01$, $^{***}p<.001$.",
  label   = "sup:tab:control_wealth"
)