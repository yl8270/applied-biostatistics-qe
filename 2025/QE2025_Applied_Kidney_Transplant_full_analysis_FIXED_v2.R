###############################################################################
# QE 2025 PhD Applied Exam Practice Script
# Kidney Transplant Dataset, ALG and Graft Survival
#
# Purpose:
#   Complete applied-analysis workflow for the 2025 Applied QE style problem.
#   This script reads Kidney_Transplant_data.csv, cleans variables, creates
#   publication-style three-line Word tables, runs survival analyses, checks
#   Cox proportional hazards assumptions, screens ALG interactions, and exports
#   figures and appendices.
#
# How to use:
#   1. Put Kidney_Transplant_data.csv in: data/raw/
#   2. Open the project folder in RStudio.
#   3. Run this script from top to bottom.
#   4. Outputs will be created in output/word, output/tables, output/figures,
#      and output/appendix.
#
# Exam safety note:
#   This is a general reusable code template for practice and preparation.
#   During the actual exam, follow the exam rules exactly. Do not use AI tools
#   to write the report or interpret your results.
###############################################################################

# 0) Setup ----
# Open the 2025 project folder before running this script.

# 0.1) User settings ----
PROJECT_TITLE <- "QE2025 Applied Practice, Kidney Transplant Graft Survival"
DATA_FILE <- file.path("data", "raw", "Kidney_Transplant_data.csv")

OUT_WORD <- file.path("output", "word")
OUT_TABLES <- file.path("output", "tables")
OUT_FIGURES <- file.path("output", "figures")
OUT_APPENDIX <- file.path("output", "appendix")
OUT_LOGS <- file.path("logs")

for (d in c(OUT_WORD, OUT_TABLES, OUT_FIGURES, OUT_APPENDIX, OUT_LOGS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Toggle package installation for your own computer. During the actual exam,
# package installation may not be allowed or may be slow, so install packages
# before the exam when possible.
INSTALL_MISSING_PACKAGES <- TRUE

# 0.2) Packages ----
required_packages <- c(
  "tidyverse", "janitor", "survival", "survminer", "gtsummary", "flextable",
  "officer", "broom", "broom.helpers", "MASS", "forcats", "scales"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (isTRUE(INSTALL_MISSING_PACKAGES)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    } else {
      stop("Package not installed: ", pkg, call. = FALSE)
    }
  }
}
invisible(lapply(required_packages, install_if_missing))

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(survival)
  library(survminer)
  library(gtsummary)
  library(flextable)
  library(officer)
  library(broom)
  library(broom.helpers)
  library(forcats)
  library(scales)
})

gtsummary::theme_gtsummary_compact()
options(gtsummary.print_engine = "flextable")

# Avoid accidental function masking. MASS is not attached because MASS::select()
# can mask dplyr::select(); we call MASS::stepAIC() explicitly below.

# 0.3) Helper functions ----

safe_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

fmt_n_pct <- function(n, denom, digits = 1) {
  ifelse(
    is.na(n) | is.na(denom) | denom == 0,
    "",
    paste0(n, " (", round(100 * n / denom, digits), "%)")
  )
}

fmt_est_ci <- function(est, lcl, ucl, digits = 2) {
  ifelse(
    is.na(est) | is.na(lcl) | is.na(ucl),
    "",
    paste0(
      formatC(est, digits = digits, format = "f"),
      " (",
      formatC(lcl, digits = digits, format = "f"),
      ", ",
      formatC(ucl, digits = digits, format = "f"),
      ")"
    )
  )
}

# Three-line table style for flextable objects.
# This removes vertical and inner grid lines, keeps top border, header-bottom
# border, and bottom border. It matches a simple publication-style table.
make_three_line <- function(ft, font_size = 9, header_font_size = 9) {
  thin <- officer::fp_border(color = "black", width = 0.75)
  thick <- officer::fp_border(color = "black", width = 1.25)

  ft %>%
    flextable::border_remove() %>%
    flextable::hline_top(part = "all", border = thick) %>%
    flextable::hline(part = "header", border = thin) %>%
    flextable::hline_bottom(part = "body", border = thick) %>%
    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::fontsize(size = font_size, part = "body") %>%
    flextable::fontsize(size = header_font_size, part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
    flextable::autofit()
}

# Apply three-line style to gtsummary tables after converting to flextable.
gtsummary_to_three_line <- function(tbl, font_size = 9) {
  tbl %>%
    gtsummary::as_flex_table() %>%
    make_three_line(font_size = font_size)
}

add_table_to_doc <- function(doc, title, ft, note = NULL) {
  doc <- officer::body_add_par(doc, title, style = "heading 2")
  doc <- flextable::body_add_flextable(doc, value = ft)
  if (!is.null(note) && nzchar(note)) {
    doc <- officer::body_add_par(doc, note, style = "Normal")
  }
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc
}

write_csv_safely <- function(x, path) {
  readr::write_csv(x, path, na = "")
  invisible(path)
}

save_survminer_plot <- function(plot_obj, filename, width = 8, height = 6, dpi = 300) {
  grDevices::png(filename, width = width, height = height, units = "in", res = dpi)
  print(plot_obj)
  grDevices::dev.off()
  invisible(filename)
}

# Extract HR table from a Cox model.
tidy_cox_hr <- function(model, model_label = NULL) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      model = model_label,
      HR_95_CI = fmt_est_ci(estimate, conf.low, conf.high, digits = 2),
      p_value = fmt_p(p.value)
    ) %>%
    dplyr::select(model, term, HR = estimate, conf.low, conf.high, HR_95_CI, p.value, p_value)
}

# Safe Cox fitting function for repeated univariate and interaction models.
safe_coxph <- function(formula, data) {
  tryCatch(
    suppressWarnings(survival::coxph(formula, data = data, x = TRUE, y = TRUE)),
    error = function(e) {
      message("Model failed: ", deparse(formula), " | ", e$message)
      return(NULL)
    }
  )
}

# 1) Import data ----

if (!file.exists(DATA_FILE)) {
  stop(
    "Data file not found. Please place Kidney_Transplant_data.csv at: ",
    DATA_FILE,
    call. = FALSE
  )
}

raw_dat <- readr::read_csv(
  DATA_FILE,
  na = c("", "NA", "N/A", "NULL", "null", ".", "-99"),
  show_col_types = FALSE
) %>%
  janitor::clean_names()

# 1.1) Required variable check ----
required_vars <- c("obs", "age", "sex", "dialy", "dbt", "ptx", "blood", "mis", "alg", "month", "fail")
missing_vars <- setdiff(required_vars, names(raw_dat))
if (length(missing_vars) > 0) {
  stop(
    "The following required variables are missing after clean_names(): ",
    paste(missing_vars, collapse = ", "),
    "\nAvailable variables are: ", paste(names(raw_dat), collapse = ", "),
    call. = FALSE
  )
}

# 2) Clean and label variables ----

analysis_dat <- raw_dat %>%
  transmute(
    subject_id = obs,
    age_years = safe_num(age),
    sex = case_when(
      safe_num(sex) == 0 ~ "Male",
      safe_num(sex) == 1 ~ "Female",
      TRUE ~ NA_character_
    ),
    dialysis_days = safe_num(dialy),
    diabetes = case_when(
      safe_num(dbt) == 0 ~ "No",
      safe_num(dbt) == 1 ~ "Yes",
      TRUE ~ NA_character_
    ),
    prior_transplants_count = safe_num(ptx),
    prior_transplant_any = case_when(
      safe_num(ptx) == 0 ~ "No",
      safe_num(ptx) > 0 ~ "Yes",
      TRUE ~ NA_character_
    ),
    blood_units = safe_num(blood),
    mismatch_score = safe_num(mis),
    alg_use = case_when(
      safe_num(alg) == 0 ~ "No ALG",
      safe_num(alg) == 1 ~ "ALG",
      TRUE ~ NA_character_
    ),
    followup_months = safe_num(month),
    graft_failure = case_when(
      safe_num(fail) == 0 ~ 0L,
      safe_num(fail) == 1 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female")),
    diabetes = factor(diabetes, levels = c("No", "Yes")),
    prior_transplant_any = factor(prior_transplant_any, levels = c("No", "Yes")),
    alg_use = factor(alg_use, levels = c("No ALG", "ALG")),
    graft_failure_label = factor(
      graft_failure,
      levels = c(0, 1),
      labels = c("Censored or functioning graft", "Graft failure")
    )
  )

# Remove records that cannot contribute to survival analysis. Keep this count
# explicit in the log because missing outcomes should be reported if present.
analysis_dat <- analysis_dat %>%
  mutate(
    invalid_survival_time = is.na(followup_months) | followup_months <= 0,
    invalid_event = is.na(graft_failure)
  )

flow_counts <- tibble::tibble(
  Step = c(
    "Raw records imported",
    "Records with nonmissing positive follow-up time",
    "Records with nonmissing graft failure indicator",
    "Records included in survival analysis"
  ),
  N = c(
    nrow(raw_dat),
    sum(!analysis_dat$invalid_survival_time),
    sum(!analysis_dat$invalid_event),
    sum(!analysis_dat$invalid_survival_time & !analysis_dat$invalid_event)
  )
)

surv_dat <- analysis_dat %>%
  filter(!invalid_survival_time, !invalid_event)

# Variable sets.
table1_vars <- c(
  "age_years", "sex", "dialysis_days", "diabetes", "prior_transplants_count",
  "blood_units", "mismatch_score"
)

continuous_vars <- c(
  "age_years", "dialysis_days", "prior_transplants_count", "blood_units", "mismatch_score"
)

categorical_vars <- c("sex", "diabetes")

model_covariates <- c(
  "alg_use", "age_years", "sex", "dialysis_days", "diabetes",
  "prior_transplants_count", "blood_units", "mismatch_score"
)

modifiers <- c(
  "age_years", "sex", "dialysis_days", "diabetes",
  "prior_transplants_count", "blood_units", "mismatch_score"
)

# Complete-case dataset for multivariable Cox modeling and model selection.
# This prevents stepAIC from failing with: "number of rows in use has changed".
model_dat <- surv_dat %>%
  dplyr::select(dplyr::all_of(c(
    "followup_months", "graft_failure", model_covariates, "prior_transplant_any"
  ))) %>%
  tidyr::drop_na(dplyr::all_of(c("followup_months", "graft_failure", model_covariates)))

# Human-readable labels. The helper functions below pass only labels that are
# actually present in a given table/model. This avoids gtsummary errors when a
# label is supplied for a variable that is not in that specific model.
label_text <- c(
  age_years = "Age at transplant, years",
  sex = "Biological sex",
  dialysis_days = "Duration of hemodialysis prior to transplant, days",
  diabetes = "Diabetes status",
  prior_transplants_count = "Number of prior transplants",
  prior_transplant_any = "Any prior transplant",
  blood_units = "Amount of blood transfusion, blood units",
  mismatch_score = "Donor mismatch score",
  alg_use = "ALG use",
  followup_months = "Follow-up time, months",
  graft_failure_label = "Graft status"
)

label_for_vars <- function(vars) {
  vars <- intersect(vars, names(label_text))
  purrr::map(vars, function(v) rlang::new_formula(rlang::sym(v), label_text[[v]], env = rlang::global_env()))
}

label_for_model <- function(model) {
  vars <- setdiff(all.vars(stats::formula(model)), c("followup_months", "graft_failure"))
  label_for_vars(vars)
}

# 3) Data quality summaries ----

missingness_table <- surv_dat %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_N") %>%
  mutate(
    Total_N = nrow(surv_dat),
    Missing_Percent = round(100 * Missing_N / Total_N, 1)
  ) %>%
  arrange(desc(Missing_N), Variable)

write_csv_safely(flow_counts, file.path(OUT_TABLES, "00_cohort_flow_counts.csv"))
write_csv_safely(missingness_table, file.path(OUT_TABLES, "00_missingness_table.csv"))

# 4) Descriptive statistics, Table 1 ----

# Table 1 by ALG use. Use median/IQR for continuous variables because clinical
# variables such as dialysis duration and transfusion units are often skewed.
table1_gts <- surv_dat %>%
  dplyr::select(dplyr::all_of(c("alg_use", table1_vars))) %>%
  tbl_summary(
    by = alg_use,
    label = label_for_vars(table1_vars),
    # Important: gtsummary may classify numeric variables with only a few unique
    # values, such as mismatch_score 0--6, as categorical by default. Force the
    # intended continuous/ordinal summaries here so Fisher's exact test is not
    # accidentally attempted for mismatch_score.
    type = list(
      dplyr::all_of(continuous_vars) ~ "continuous2",
      dplyr::all_of(categorical_vars) ~ "categorical"
    ),
    statistic = list(
      dplyr::all_of(continuous_vars) ~ c("{median} ({p25}, {p75})", "{mean} ({sd})"),
      dplyr::all_of(categorical_vars) ~ "{n} ({p}%)"
    ),
    digits = list(
      dplyr::all_of(continuous_vars) ~ 1,
      dplyr::all_of(categorical_vars) ~ c(0, 1)
    ),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(
      dplyr::all_of(continuous_vars) ~ "wilcox.test",
      dplyr::all_of(categorical_vars) ~ "fisher.test"
    ),
    # Fisher exact tests for 2-by-2 categorical variables should be fine. The
    # workspace argument makes the code more robust if a future exam has a
    # larger categorical variable in Table 1.
    test.args = list(
      dplyr::all_of(categorical_vars) ~ list(workspace = 2e7)
    ),
    pvalue_fun = fmt_p
  ) %>%
  modify_header(label ~ "Characteristic") %>%
  modify_caption("Table 1. Patient characteristics by ALG use") %>%
  bold_labels()

table1_ft <- gtsummary_to_three_line(table1_gts, font_size = 8)

# 5) Survival summaries and Kaplan-Meier figure ----

surv_obj <- survival::Surv(time = surv_dat$followup_months, event = surv_dat$graft_failure)

km_fit_alg <- survival::survfit(surv_obj ~ alg_use, data = surv_dat)

# Summary table for events and follow-up by ALG.
event_summary <- surv_dat %>%
  group_by(alg_use) %>%
  summarise(
    N = n(),
    Events = sum(graft_failure == 1, na.rm = TRUE),
    Censored = sum(graft_failure == 0, na.rm = TRUE),
    Event_Percent = round(100 * Events / N, 1),
    Followup_Median_IQR = paste0(
      round(median(followup_months, na.rm = TRUE), 1), " (",
      round(quantile(followup_months, 0.25, na.rm = TRUE), 1), ", ",
      round(quantile(followup_months, 0.75, na.rm = TRUE), 1), ")"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Event_N_Percent = fmt_n_pct(Events, N),
    Censored_N_Percent = fmt_n_pct(Censored, N)
  ) %>%
  dplyr::select(
    `ALG use` = alg_use,
    N,
    `Graft failures, n (%)` = Event_N_Percent,
    `Censored/functioning graft, n (%)` = Censored_N_Percent,
    `Follow-up, median (IQR), months` = Followup_Median_IQR
  )

event_summary_ft <- event_summary %>%
  flextable::flextable() %>%
  make_three_line(font_size = 9)

# Log-rank test by ALG.
logrank_alg <- survival::survdiff(surv_obj ~ alg_use, data = surv_dat)
logrank_p <- 1 - stats::pchisq(logrank_alg$chisq, length(logrank_alg$n) - 1)

logrank_table <- tibble::tibble(
  Test = "Log-rank test comparing graft survival by ALG use",
  Chisq = round(logrank_alg$chisq, 3),
  df = length(logrank_alg$n) - 1,
  `P value` = fmt_p(logrank_p)
)

logrank_ft <- logrank_table %>%
  flextable::flextable() %>%
  make_three_line(font_size = 9)

# Kaplan-Meier plot by ALG use.
km_plot <- survminer::ggsurvplot(
  km_fit_alg,
  data = surv_dat,
  risk.table = TRUE,
  conf.int = TRUE,
  pval = TRUE,
  pval.method = TRUE,
  xlab = "Months after kidney transplant",
  ylab = "Graft survival probability",
  legend.title = "ALG use",
  legend.labs = levels(surv_dat$alg_use),
  break.time.by = 12,
  risk.table.height = 0.25,
  ggtheme = ggplot2::theme_bw(base_size = 11)
)

km_png <- file.path(OUT_FIGURES, "Figure_1_Kaplan_Meier_by_ALG.png")
save_survminer_plot(km_plot, km_png, width = 8, height = 6, dpi = 300)

# Log-log plot for visual PH assumption by ALG.
loglog_plot <- survminer::ggsurvplot(
  km_fit_alg,
  data = surv_dat,
  fun = "cloglog",
  conf.int = FALSE,
  xlab = "log(Months after kidney transplant)",
  ylab = "log(-log(Survival probability))",
  legend.title = "ALG use",
  legend.labs = levels(surv_dat$alg_use),
  ggtheme = ggplot2::theme_bw(base_size = 11)
)
loglog_png <- file.path(OUT_APPENDIX, "Appendix_loglog_PH_by_ALG.png")
save_survminer_plot(loglog_plot, loglog_png, width = 7, height = 5, dpi = 300)

# 6) Univariate Cox models ----

univ_models <- purrr::map(
  model_covariates,
  ~ safe_coxph(as.formula(paste0("Surv(followup_months, graft_failure) ~ ", .x)), data = surv_dat)
)
names(univ_models) <- model_covariates
univ_models <- univ_models[!vapply(univ_models, is.null, logical(1))]

univ_results <- purrr::imap_dfr(univ_models, ~ tidy_cox_hr(.x, model_label = .y)) %>%
  mutate(
    Variable = case_when(
      str_detect(term, "alg_use") ~ "ALG use",
      str_detect(term, "age_years") ~ "Age at transplant, years",
      str_detect(term, "sex") ~ "Biological sex",
      str_detect(term, "dialysis_days") ~ "Duration of hemodialysis prior to transplant, days",
      str_detect(term, "diabetes") ~ "Diabetes status",
      str_detect(term, "prior_transplants_count") ~ "Number of prior transplants",
      str_detect(term, "blood_units") ~ "Amount of blood transfusion, blood units",
      str_detect(term, "mismatch_score") ~ "Donor mismatch score",
      TRUE ~ term
    ),
    `Hazard ratio (95% CI)` = HR_95_CI,
    `P value` = p_value
  ) %>%
  dplyr::select(Variable, term, `Hazard ratio (95% CI)`, `P value`)

write_csv_safely(univ_results, file.path(OUT_TABLES, "01_univariate_cox_results.csv"))

univ_ft <- univ_results %>%
  flextable::flextable() %>%
  flextable::set_header_labels(term = "Model term") %>%
  make_three_line(font_size = 8)

# 7) Multivariable Cox models ----

full_formula <- as.formula(
  "Surv(followup_months, graft_failure) ~ alg_use + age_years + sex + dialysis_days + diabetes + prior_transplants_count + blood_units + mismatch_score"
)

full_cox <- survival::coxph(full_formula, data = model_dat, x = TRUE, y = TRUE)

# Model selection with ALG forced into the model. This is included because the
# exam asks for model selection as needed, but the main exposure ALG should not
# be removed from the scientific model.
selected_cox <- tryCatch(
  MASS::stepAIC(
    full_cox,
    scope = list(
      lower = as.formula("Surv(followup_months, graft_failure) ~ alg_use"),
      upper = full_formula
    ),
    direction = "both",
    trace = FALSE
  ),
  error = function(e) {
    message("stepAIC failed. Using full multivariable Cox model. Error: ", e$message)
    full_cox
  }
)

full_cox_gts <- tbl_regression(
  full_cox,
  exponentiate = TRUE,
  label = label_for_model(full_cox),
  estimate_fun = ~ style_sigfig(.x, digits = 3),
  pvalue_fun = fmt_p
) %>%
  modify_caption("Table 3. Full multivariable Cox proportional hazards model") %>%
  bold_labels()

selected_cox_gts <- tbl_regression(
  selected_cox,
  exponentiate = TRUE,
  label = label_for_model(selected_cox),
  estimate_fun = ~ style_sigfig(.x, digits = 3),
  pvalue_fun = fmt_p
) %>%
  modify_caption("Table 4. Selected Cox proportional hazards model with ALG forced") %>%
  bold_labels()

full_cox_ft <- gtsummary_to_three_line(full_cox_gts, font_size = 8)
selected_cox_ft <- gtsummary_to_three_line(selected_cox_gts, font_size = 8)

write_csv_safely(tidy_cox_hr(full_cox, "Full Cox model"), file.path(OUT_TABLES, "02_full_cox_results.csv"))
write_csv_safely(tidy_cox_hr(selected_cox, "Selected Cox model"), file.path(OUT_TABLES, "03_selected_cox_results.csv"))

# Forest plot for selected Cox model.
forest_png <- file.path(OUT_FIGURES, "Figure_2_Selected_Cox_Forest_Plot.png")
try({
  grDevices::png(forest_png, width = 8, height = 6, units = "in", res = 300)
  print(survminer::ggforest(selected_cox, data = model_dat, fontsize = 0.8))
  grDevices::dev.off()
}, silent = TRUE)

# 8) Cox PH assumption checks ----

ph_full <- survival::cox.zph(full_cox)
ph_selected <- survival::cox.zph(selected_cox)

ph_full_table <- as.data.frame(ph_full$table) %>%
  rownames_to_column("Term") %>%
  as_tibble() %>%
  transmute(
    Model = "Full Cox model",
    Term,
    Chisq = round(chisq, 3),
    df = round(df, 0),
    `P value` = fmt_p(p)
  )

ph_selected_table <- as.data.frame(ph_selected$table) %>%
  rownames_to_column("Term") %>%
  as_tibble() %>%
  transmute(
    Model = "Selected Cox model",
    Term,
    Chisq = round(chisq, 3),
    df = round(df, 0),
    `P value` = fmt_p(p)
  )

ph_table <- bind_rows(ph_full_table, ph_selected_table)
write_csv_safely(ph_table, file.path(OUT_TABLES, "04_ph_assumption_tests.csv"))

ph_ft <- ph_table %>%
  flextable::flextable() %>%
  make_three_line(font_size = 8)

# Save Schoenfeld residual plots for selected model.
zph_png <- file.path(OUT_APPENDIX, "Appendix_selected_cox_zph_plots.png")
grDevices::png(zph_png, width = 10, height = 8, units = "in", res = 300)
plot(ph_selected)
grDevices::dev.off()

# Martingale residual plots for continuous covariates, rough functional-form check.
martingale_df <- model_dat %>%
  dplyr::mutate(martingale_resid = residuals(full_cox, type = "martingale"))

for (v in continuous_vars) {
  p <- ggplot(martingale_df, aes(x = .data[[v]], y = martingale_resid)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      x = v,
      y = "Martingale residuals",
      title = paste("Functional form check for", v)
    ) +
    theme_bw(base_size = 11)
  ggplot2::ggsave(
    filename = file.path(OUT_APPENDIX, paste0("Appendix_martingale_", v, ".png")),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}

# 9) ALG interaction screening ----

# The exam asks whether ALG effect differs by other measured covariates. This
# section fits one interaction at a time using likelihood ratio tests. In the
# report, do not overclaim. Treat these as effect modification screening tests.
fit_interaction_lrt <- function(modifier) {
  base_covariates <- setdiff(model_covariates, c("alg_use", modifier))
  reduced_formula <- as.formula(
    paste0(
      "Surv(followup_months, graft_failure) ~ alg_use + ",
      paste(c(modifier, base_covariates), collapse = " + ")
    )
  )
  interaction_formula <- as.formula(
    paste0(
      "Surv(followup_months, graft_failure) ~ alg_use * ", modifier,
      ifelse(length(base_covariates) > 0, paste0(" + ", paste(base_covariates, collapse = " + ")), "")
    )
  )

  reduced_model <- safe_coxph(reduced_formula, model_dat)
  interaction_model <- safe_coxph(interaction_formula, model_dat)

  if (is.null(reduced_model) || is.null(interaction_model)) {
    return(tibble::tibble(
      Modifier = modifier,
      LRT_Chisq = NA_real_,
      df = NA_real_,
      P_value_raw = NA_real_,
      `P value` = "Model failed"
    ))
  }

  lrt <- anova(reduced_model, interaction_model, test = "LRT")
  tibble::tibble(
    Modifier = modifier,
    LRT_Chisq = as.numeric(lrt$`Chisq`[2]),
    df = as.numeric(lrt$`Df`[2]),
    P_value_raw = as.numeric(lrt$`Pr(>|Chi|)`[2]),
    `P value` = fmt_p(P_value_raw)
  )
}

interaction_screen <- purrr::map_dfr(modifiers, fit_interaction_lrt) %>%
  arrange(P_value_raw)

write_csv_safely(interaction_screen, file.path(OUT_TABLES, "05_alg_interaction_screening.csv"))

interaction_ft <- interaction_screen %>%
  mutate(
    LRT_Chisq = round(LRT_Chisq, 3),
    df = round(df, 0)
  ) %>%
  dplyr::select(Modifier, LRT_Chisq, df, `P value`) %>%
  flextable::flextable() %>%
  flextable::set_header_labels(
    Modifier = "Potential effect modifier",
    LRT_Chisq = "LRT chi-square"
  ) %>%
  make_three_line(font_size = 8)

# Fit and export detailed tables for interactions with p < 0.10, or the top two
# smallest p-values if none meet the threshold. This keeps the output useful
# while avoiding excessive main-report clutter.
selected_modifiers_for_detail <- interaction_screen %>%
  filter(!is.na(P_value_raw)) %>%
  mutate(rank = row_number()) %>%
  filter(P_value_raw < 0.10 | rank <= 2) %>%
  pull(Modifier)

interaction_detail_tables <- list()
for (mod in selected_modifiers_for_detail) {
  base_covariates <- setdiff(model_covariates, c("alg_use", mod))
  f_int <- as.formula(
    paste0(
      "Surv(followup_months, graft_failure) ~ alg_use * ", mod,
      ifelse(length(base_covariates) > 0, paste0(" + ", paste(base_covariates, collapse = " + ")), "")
    )
  )
  m_int <- safe_coxph(f_int, model_dat)
  if (!is.null(m_int)) {
    interaction_detail_tables[[mod]] <- tbl_regression(
      m_int,
      exponentiate = TRUE,
      label = label_for_model(m_int),
      estimate_fun = ~ style_sigfig(.x, digits = 3),
      pvalue_fun = fmt_p
    ) %>%
      modify_caption(paste0("Interaction model: ALG use by ", mod)) %>%
      bold_labels()
  }
}

# 10) Sensitivity checks ----

# Sensitivity model using prior transplant as yes/no instead of count. This is
# useful because the description says prior transplant yes/no but the data
# dictionary labels PTX as a count.
sensitivity_formula <- as.formula(
  "Surv(followup_months, graft_failure) ~ alg_use + age_years + sex + dialysis_days + diabetes + prior_transplant_any + blood_units + mismatch_score"
)
sensitivity_cox <- survival::coxph(sensitivity_formula, data = model_dat, x = TRUE, y = TRUE)

sensitivity_gts <- tbl_regression(
  sensitivity_cox,
  exponentiate = TRUE,
  label = label_for_model(sensitivity_cox),
  estimate_fun = ~ style_sigfig(.x, digits = 3),
  pvalue_fun = fmt_p
) %>%
  modify_caption("Sensitivity model using any prior transplant instead of prior transplant count") %>%
  bold_labels()

sensitivity_ft <- gtsummary_to_three_line(sensitivity_gts, font_size = 8)
write_csv_safely(tidy_cox_hr(sensitivity_cox, "Sensitivity Cox model"), file.path(OUT_TABLES, "06_sensitivity_cox_prior_tx_binary.csv"))

# 11) Export Word tables and figures ----

# Main tables document. This is intended for tables and figures that can go into
# the main report. Interpretations must be written by you based on the actual
# numerical results.
main_doc <- officer::read_docx()
main_doc <- body_add_par(main_doc, PROJECT_TITLE, style = "heading 1")
main_doc <- body_add_par(main_doc, paste0("Generated: ", Sys.Date()), style = "Normal")
main_doc <- body_add_par(
  main_doc,
  "This document contains formatted three-line tables and figures generated from the R analysis script. Replace this note with your own written interpretation before submitting any report.",
  style = "Normal"
)
main_doc <- body_add_par(main_doc, "", style = "Normal")

main_doc <- add_table_to_doc(
  main_doc,
  "Table 1. Patient characteristics by ALG use",
  table1_ft,
  note = "Continuous variables are summarized as median (IQR) and mean (SD); categorical variables are summarized as n (%). P values are from Wilcoxon rank-sum tests for continuous variables and Fisher exact tests for categorical variables."
)

main_doc <- add_table_to_doc(
  main_doc,
  "Table 2. Graft failure and follow-up summary by ALG use",
  event_summary_ft,
  note = "Events are graft failures. Patients without graft failure at last follow-up are treated as censored."
)

main_doc <- add_table_to_doc(
  main_doc,
  "Log-rank test by ALG use",
  logrank_ft,
  note = "The log-rank test compares unadjusted graft survival curves by ALG use."
)

main_doc <- officer::body_add_par(main_doc, "Figure 1. Kaplan-Meier graft survival curve by ALG use", style = "heading 2")
main_doc <- officer::body_add_img(main_doc, src = km_png, width = 6.8, height = 5.1)
main_doc <- officer::body_add_par(main_doc, "", style = "Normal")

main_doc <- add_table_to_doc(
  main_doc,
  "Table 3. Univariate Cox proportional hazards models",
  univ_ft,
  note = "Each row comes from a separate univariate Cox proportional hazards model. Hazard ratios greater than 1 indicate higher hazard of graft failure."
)

main_doc <- add_table_to_doc(
  main_doc,
  "Table 4. Full multivariable Cox proportional hazards model",
  full_cox_ft,
  note = "The full model includes ALG use and all prespecified covariates. Hazard ratios greater than 1 indicate higher hazard of graft failure."
)

main_doc <- add_table_to_doc(
  main_doc,
  "Table 5. Selected Cox proportional hazards model with ALG forced",
  selected_cox_ft,
  note = "Model selection used AIC with ALG forced into the model because ALG is the main exposure of scientific interest."
)

if (file.exists(forest_png)) {
  main_doc <- officer::body_add_par(main_doc, "Figure 2. Forest plot for selected Cox model", style = "heading 2")
  main_doc <- officer::body_add_img(main_doc, src = forest_png, width = 6.8, height = 5.1)
  main_doc <- officer::body_add_par(main_doc, "", style = "Normal")
}

main_doc <- add_table_to_doc(
  main_doc,
  "Table 6. Screening tests for ALG effect modification",
  interaction_ft,
  note = "Each row compares a model with an ALG-by-modifier interaction against the corresponding model without the interaction using a likelihood ratio test. These results should be interpreted as effect modification screening."
)

main_word_path <- file.path(OUT_WORD, "QE2025_Kidney_Transplant_Main_Tables_ThreeLine.docx")
print(main_doc, target = main_word_path)

# Appendix document with diagnostics and sensitivity analyses.
appendix_doc <- officer::read_docx()
appendix_doc <- body_add_par(appendix_doc, "Appendix, Diagnostics and Sensitivity Analyses", style = "heading 1")
appendix_doc <- body_add_par(appendix_doc, paste0("Generated: ", Sys.Date()), style = "Normal")
appendix_doc <- body_add_par(appendix_doc, "", style = "Normal")

flow_ft <- flow_counts %>%
  flextable::flextable() %>%
  make_three_line(font_size = 9)
appendix_doc <- add_table_to_doc(
  appendix_doc,
  "Appendix Table A1. Cohort flow counts",
  flow_ft,
  note = "These counts document the analytic sample used for survival analysis."
)

missing_ft <- missingness_table %>%
  flextable::flextable() %>%
  make_three_line(font_size = 8)
appendix_doc <- add_table_to_doc(
  appendix_doc,
  "Appendix Table A2. Missingness summary",
  missing_ft,
  note = "Missingness is calculated after excluding records that cannot contribute to survival analysis."
)

appendix_doc <- add_table_to_doc(
  appendix_doc,
  "Appendix Table A3. Cox proportional hazards assumption tests",
  ph_ft,
  note = "Tests are based on scaled Schoenfeld residuals. A small p value suggests possible violation of the proportional hazards assumption."
)

appendix_doc <- officer::body_add_par(appendix_doc, "Appendix Figure A1. Log-log survival plot by ALG use", style = "heading 2")
appendix_doc <- officer::body_add_img(appendix_doc, src = loglog_png, width = 6.4, height = 4.6)
appendix_doc <- officer::body_add_par(appendix_doc, "", style = "Normal")

appendix_doc <- officer::body_add_par(appendix_doc, "Appendix Figure A2. Scaled Schoenfeld residual plots for selected Cox model", style = "heading 2")
appendix_doc <- officer::body_add_img(appendix_doc, src = zph_png, width = 6.8, height = 5.4)
appendix_doc <- officer::body_add_par(appendix_doc, "", style = "Normal")

appendix_doc <- add_table_to_doc(
  appendix_doc,
  "Appendix Table A4. Sensitivity model using prior transplant as binary",
  sensitivity_ft,
  note = "This sensitivity analysis uses any prior transplant instead of prior transplant count."
)

if (length(interaction_detail_tables) > 0) {
  for (nm in names(interaction_detail_tables)) {
    appendix_doc <- add_table_to_doc(
      appendix_doc,
      paste0("Appendix Table A5. Detailed interaction model for ALG by ", nm),
      gtsummary_to_three_line(interaction_detail_tables[[nm]], font_size = 7),
      note = "This model is included to support interpretation of the corresponding interaction screening result."
    )
  }
}

# Add martingale plots one by one.
for (v in continuous_vars) {
  img_path <- file.path(OUT_APPENDIX, paste0("Appendix_martingale_", v, ".png"))
  if (file.exists(img_path)) {
    appendix_doc <- officer::body_add_par(
      appendix_doc,
      paste0("Appendix Figure. Martingale residual functional-form check for ", v),
      style = "heading 2"
    )
    appendix_doc <- officer::body_add_img(appendix_doc, src = img_path, width = 6.2, height = 4.1)
    appendix_doc <- officer::body_add_par(appendix_doc, "", style = "Normal")
  }
}

appendix_word_path <- file.path(OUT_WORD, "QE2025_Kidney_Transplant_Appendix_Diagnostics_ThreeLine.docx")
print(appendix_doc, target = appendix_word_path)

# 12) Export reproducibility log ----

sink(file.path(OUT_LOGS, "session_info.txt"))
cat(PROJECT_TITLE, "\n")
cat("Generated:", as.character(Sys.time()), "\n\n")
cat("Data file:", DATA_FILE, "\n\n")
cat("R session information:\n")
print(sessionInfo())
sink()

# 13) Final console message ----

message("Analysis complete.")
message("Main Word tables: ", main_word_path)
message("Appendix Word tables and diagnostics: ", appendix_word_path)
message("CSV tables: ", OUT_TABLES)
message("Figures: ", OUT_FIGURES, " and ", OUT_APPENDIX)
message("Session log: ", file.path(OUT_LOGS, "session_info.txt"))
