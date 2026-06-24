# 0) Setup ----
# SEARCH KEYS: KEY_TABLE1_PROCEDURE, KEY_TABLE2_OUTCOMES, KEY_TABLE3_INDIVIDUAL_COMPLICATIONS
# SEARCH KEYS: KEY_BINARY_LOGISTIC_INTERACTION, KEY_COUNT_POISSON_NEGATIVE_BINOMIAL, KEY_OVERDISPERSION
# SEARCH KEYS: KEY_LENGTH_OF_STAY, KEY_MODEL_DIAGNOSTICS, KEY_THREE_LINE_WORD_OUTPUT
# SEARCH KEYS: KEY_MISSING_RECODE_NULL_MINUS99
# Project: 2024 PhD Qualifying Exam Applied Practice, NSQIP Bariatric Surgery
# Purpose: Reusable applied-exam R script for Table 1, outcome summaries,
#          binary logistic modeling, count modeling, interaction testing,
#          diagnostics, figures, and three-line Word output.
# Data: Synthetic practice data generated to mimic the 2024 exam structure.
# Note: Replace DATA_PATH with the real exam data path during practice or exam.
# Open the 2024 project folder (or its .Rproj file) before running this script.
rm(list = ls())

PROJECT_TITLE <- "QE2024 Applied Practice, NSQIP Bariatric Surgery Outcomes"
DATA_PATH <- "data/raw/NSQIP_bmi_synthetic.csv"

OUT_TABLES <- "output/tables"
OUT_FIGURES <- "output/figures"
OUT_APPENDIX <- "output/appendix"
OUT_WORD <- "output/word"
for (d in c(OUT_TABLES, OUT_FIGURES, OUT_APPENDIX, OUT_WORD)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

required_packages <- c(
  "tidyverse", "janitor", "gtsummary", "flextable", "officer",
  "broom", "MASS", "splines", "scales", "readxl"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install missing packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(tidyverse)
library(janitor)
library(gtsummary)
library(flextable)
library(officer)
library(broom)

# Helper formatting functions ----
fmt_p <- function(x) {
  gtsummary::style_pvalue(x, digits = 3)
}

fmt_n_pct <- function(n, denom, digits = 1) {
  ifelse(is.na(n) | is.na(denom) | denom == 0,
         NA_character_,
         paste0(n, " (", round(100 * n / denom, digits), "%)"))
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]
}

make_three_line <- function(ft, font_size = 8) {
  ft %>%
    flextable::theme_booktabs() %>%
    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::valign(valign = "top", part = "all") %>%
    flextable::autofit()
}

gtsummary_to_three_line <- function(tbl, font_size = 8) {
  tbl %>%
    gtsummary::as_flex_table() %>%
    make_three_line(font_size = font_size)
}

add_table_to_doc <- function(doc, title, ft, note = NULL) {
  doc <- officer::body_add_par(doc, title, style = "heading 2")
  doc <- flextable::body_add_flextable(doc, value = ft)
  if (!is.null(note)) {
    doc <- officer::body_add_par(doc, note, style = "Normal")
  }
  officer::body_add_par(doc, "", style = "Normal")
}

write_csv_safely <- function(x, path) {
  readr::write_csv(x, path, na = "")
}

save_plot <- function(plot, path, width = 7, height = 5, dpi = 300) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi)
  invisible(path)
}

# Model table helper for OR/IRR/RR-like exponentiated models.
tidy_exp_model <- function(model, model_label) {
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::mutate(
      model = model_label,
      Estimate_95_CI = paste0(round(estimate, 3), " (", round(conf.low, 3), ", ", round(conf.high, 3), ")"),
      p_value = fmt_p(p.value)
    ) %>%
    dplyr::select(model, term, estimate, conf.low, conf.high, Estimate_95_CI, p.value, p_value)
}

# 1) Import data ----
if (!file.exists(DATA_PATH)) {
  stop("DATA_PATH does not exist: ", DATA_PATH, call. = FALSE)
}

if (grepl("\\.sas7bdat$", DATA_PATH, ignore.case = TRUE)) {
  dat_raw <- haven::read_sas(DATA_PATH) %>% as_tibble()
} else if (grepl("\\.csv$", DATA_PATH, ignore.case = TRUE)) {
  dat_raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
} else if (grepl("\\.xlsx$|\\.xls$", DATA_PATH, ignore.case = TRUE)) {
  dat_raw <- readxl::read_excel(DATA_PATH) %>% as_tibble()
} else {
  stop("Unsupported file type. Use .csv, .xlsx, .xls, or .sas7bdat.", call. = FALSE)
}


# Some versions of the NSQIP teaching file may name or omit the reoperation variable.
# The synthetic practice data uses REOPERATION. If absent, create an all-missing placeholder
# so the script still runs after you decide how to map the real reoperation variable.
if (!"REOPERATION" %in% names(dat_raw)) {
  dat_raw$REOPERATION <- NA_character_
}

# 2) Clean variables and derive outcomes ----
# Exam-specific points:
# - Recode -99 and "NULL" as missing.
# - Create readable labels so readers do not need the codebook.
# - Define any surgical complication and number of complications from the individual items.

complication_vars <- c(
  "OUPNEUMO", "REINTUB", "PULEMBOL", "FAILWEAN", "OPRENAFL", "RENAINSF",
  "URNINFEC", "CDARREST", "CDMI", "OTHBLEED", "OTHDVT", "NEURODEF",
  "CNSCOMA", "CNSCVA", "OTHGRAFL", "OTHSYSEP", "OTHSESHOCK", "SUPINFEC",
  "WNDINFD", "ORGSPCSSI", "DEHIS"
)

# The exam variable list may omit some rare complication variables in a given dataset.
# Use only variables that exist, but warn if expected variables are absent.
missing_comp_vars <- setdiff(complication_vars, names(dat_raw))
if (length(missing_comp_vars) > 0) {
  message("These expected complication variables were not found and will be skipped: ", paste(missing_comp_vars, collapse = ", "))
}
complication_vars_present <- intersect(complication_vars, names(dat_raw))

clean_null <- function(x) {
  if (is.character(x)) {
    x <- trimws(x)
    x[x %in% c("", "NULL", "null", "Null", "NA", "N/A", ".")] <- NA_character_
  }
  x
}

dat <- dat_raw %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), clean_null)) %>%
  dplyr::mutate(
    dplyr::across(dplyr::where(is.numeric), ~ dplyr::na_if(.x, -99)),
    dplyr::across(dplyr::all_of(intersect(c("bmi", "ageyrs", "OPTIME", "TOTHLOS"), names(.))), as.numeric)
  ) %>%
  dplyr::mutate(
    case_id = CaseID,
    age_years = as.numeric(ageyrs),
    bmi = as.numeric(bmi),
    procedure = dplyr::case_when(
      as.character(CPT) == "43770" ~ "Gastric Banding",
      as.character(CPT) == "43644" ~ "Gastric Bypass",
      TRUE ~ NA_character_
    ),
    procedure = factor(procedure, levels = c("Gastric Banding", "Gastric Bypass")),
    sex = factor(SEX),
    race_ethnicity = factor(race_eth),
    smoking_status = factor(SMOKE, levels = c("No", "Yes")),
    asa_class = factor(ASACLAS),
    asa_severity = dplyr::case_when(
      ASACLAS %in% c("1-No Disturb", "2-Mild Disturb", "None assigned") ~ "Mild",
      ASACLAS %in% c("3-Severe Disturb", "4-Life Threat") ~ "Severe",
      TRUE ~ NA_character_
    ),
    asa_severity = factor(asa_severity, levels = c("Mild", "Severe")),
    operation_time_min = as.numeric(OPTIME),
    los_days = as.numeric(TOTHLOS),
    mortality = dplyr::case_when(
      Mortality %in% c("Yes", "Y", "1", "Death", "Dead") ~ 1L,
      Mortality %in% c("No", "N", "0", "Alive") ~ 0L,
      TRUE ~ NA_integer_
    ),
    reoperation = dplyr::case_when(
      REOPERATION %in% c("Yes", "Y", "1") ~ 1L,
      REOPERATION %in% c("No", "N", "0") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Convert individual complications to 0/1 numeric helper variables.
for (v in complication_vars_present) {
  new_name <- paste0(tolower(v), "_bin")
  dat[[new_name]] <- dplyr::case_when(
    dat[[v]] %in% c("Yes", "Y", "1") ~ 1L,
    dat[[v]] %in% c("No", "N", "0") ~ 0L,
    TRUE ~ NA_integer_
  )
}
complication_bin_vars <- paste0(tolower(complication_vars_present), "_bin")

# Derived primary and secondary outcomes.
dat <- dat %>%
  dplyr::mutate(
    number_complications = rowSums(dplyr::across(dplyr::all_of(complication_bin_vars)), na.rm = TRUE),
    # If all individual complications are missing for a subject, set derived outcome missing.
    n_comp_missing = rowSums(is.na(dplyr::across(dplyr::all_of(complication_bin_vars)))),
    surgical_complication = dplyr::case_when(
      n_comp_missing == length(complication_bin_vars) ~ NA_integer_,
      number_complications > 0 ~ 1L,
      number_complications == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    surgical_complication_label = factor(
      dplyr::if_else(surgical_complication == 1, "Any complication", "No complication", missing = NA_character_),
      levels = c("No complication", "Any complication")
    ),
    mortality_label = factor(dplyr::if_else(mortality == 1, "Yes", "No", missing = NA_character_), levels = c("No", "Yes")),
    reoperation_label = factor(dplyr::if_else(reoperation == 1, "Yes", "No", missing = NA_character_), levels = c("No", "Yes"))
  )

# Cohort flow and missingness ----
flow_counts <- tibble::tibble(
  Step = c(
    "Raw records imported",
    "Records with known procedure type",
    "Records with nonmissing surgical complication status",
    "Records with nonmissing age and BMI",
    "Records available for primary logistic complete-case model"
  ),
  N = c(
    nrow(dat_raw),
    sum(!is.na(dat$procedure)),
    sum(!is.na(dat$procedure) & !is.na(dat$surgical_complication)),
    sum(!is.na(dat$procedure) & !is.na(dat$surgical_complication) & !is.na(dat$age_years) & !is.na(dat$bmi)),
    dat %>%
      dplyr::select(procedure, surgical_complication, age_years, bmi, sex, race_ethnicity, smoking_status, asa_severity) %>%
      tidyr::drop_na() %>%
      nrow()
  )
)

missingness_table <- dat %>%
  dplyr::select(
    case_id, procedure, age_years, bmi, sex, race_ethnicity, smoking_status, asa_class,
    asa_severity, operation_time_min, surgical_complication, number_complications,
    mortality, los_days, reoperation, dplyr::all_of(complication_bin_vars)
  ) %>%
  summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) %>%
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Missing_N") %>%
  dplyr::mutate(
    Total_N = nrow(dat),
    Missing_Percent = round(100 * Missing_N / Total_N, 1)
  ) %>%
  dplyr::arrange(dplyr::desc(Missing_N), Variable)

write_csv_safely(flow_counts, file.path(OUT_TABLES, "00_cohort_flow.csv"))
write_csv_safely(missingness_table, file.path(OUT_TABLES, "00_missingness.csv"))

# 3) Descriptive tables ----
# Table 1: baseline characteristics by procedure type.
table1_vars <- c("age_years", "sex", "race_ethnicity", "bmi", "smoking_status", "asa_severity")
continuous_table1 <- c("age_years", "bmi")
categorical_table1 <- setdiff(table1_vars, continuous_table1)

table1_gts <- dat %>%
  dplyr::select(dplyr::all_of(c("procedure", table1_vars))) %>%
  tbl_summary(
    by = procedure,
    type = list(
      dplyr::all_of(continuous_table1) ~ "continuous2",
      dplyr::all_of(categorical_table1) ~ "categorical"
    ),
    statistic = list(
      dplyr::all_of(continuous_table1) ~ c("{median} ({p25}, {p75})", "{mean} ({sd})"),
      dplyr::all_of(categorical_table1) ~ "{n} ({p}%)"
    ),
    label = list(
      age_years ~ "Age, years",
      sex ~ "Sex",
      race_ethnicity ~ "Race/ethnicity",
      bmi ~ "Body mass index, kg/m^2",
      smoking_status ~ "Current smoker within one year",
      asa_severity ~ "ASA severity"
    ),
    missing = "ifany"
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      dplyr::all_of(continuous_table1) ~ "wilcox.test",
      dplyr::all_of(categorical_table1) ~ "chisq.test.no.correct"
    ),
    pvalue_fun = fmt_p
  ) %>%
  modify_caption("Table 1. Patient characteristics by procedure type") %>%
  bold_labels()

table1_ft <- gtsummary_to_three_line(table1_gts, font_size = 8)

# Table 2: outcome summaries by procedure type.
table2_vars <- c("surgical_complication_label", "number_complications", "mortality_label", "los_days", "reoperation_label")
continuous_table2 <- c("number_complications", "los_days")
categorical_table2 <- c("surgical_complication_label", "mortality_label", "reoperation_label")

table2_gts <- dat %>%
  dplyr::select(dplyr::all_of(c("procedure", table2_vars))) %>%
  tbl_summary(
    by = procedure,
    type = list(
      dplyr::all_of(continuous_table2) ~ "continuous2",
      dplyr::all_of(categorical_table2) ~ "categorical"
    ),
    statistic = list(
      dplyr::all_of(continuous_table2) ~ c("{median} ({p25}, {p75})", "{mean} ({sd})"),
      dplyr::all_of(categorical_table2) ~ "{n} ({p}%)"
    ),
    label = list(
      surgical_complication_label ~ "Any surgical complication",
      number_complications ~ "Number of surgical complications",
      mortality_label ~ "Mortality",
      los_days ~ "Length of hospital stay, days",
      reoperation_label ~ "Reoperation"
    ),
    missing = "ifany"
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      dplyr::all_of(continuous_table2) ~ "wilcox.test",
      dplyr::all_of(categorical_table2) ~ "chisq.test.no.correct"
    ),
    pvalue_fun = fmt_p
  ) %>%
  modify_caption("Table 2. Outcomes by procedure type") %>%
  bold_labels()

table2_ft <- gtsummary_to_three_line(table2_gts, font_size = 8)

# Table 3: individual complications by procedure type.
individual_comp_labels <- c(
  OUPNEUMO = "Pneumonia",
  REINTUB = "Unplanned intubation",
  PULEMBOL = "Pulmonary embolism",
  FAILWEAN = "Ventilator > 48 hours",
  OPRENAFL = "Acute renal failure",
  RENAINSF = "Progressive renal insufficiency",
  URNINFEC = "Urinary tract infection",
  CDARREST = "Cardiac arrest requiring CPR",
  CDMI = "Myocardial infarction",
  OTHBLEED = "Bleeding transfusions",
  OTHDVT = "DVT/thrombophlebitis",
  NEURODEF = "Neurologic deficit",
  CNSCOMA = "Coma > 24 hours",
  CNSCVA = "Stroke/CVA",
  OTHGRAFL = "Graft/prosthesis/flap failure",
  OTHSYSEP = "Sepsis",
  OTHSESHOCK = "Septic shock",
  SUPINFEC = "Superficial surgical site infection",
  WNDINFD = "Deep incisional SSI",
  ORGSPCSSI = "Organ space SSI",
  DEHIS = "Wound disruption"
)

# Make readable factor copies for individual complications.
for (v in complication_bin_vars) {
  pretty <- paste0(v, "_label")
  dat[[pretty]] <- factor(dplyr::if_else(dat[[v]] == 1, "Yes", "No", missing = NA_character_), levels = c("No", "Yes"))
}
complication_label_vars <- paste0(complication_bin_vars, "_label")

table3_label_list <- list()
for (v in complication_label_vars) {
  raw_name <- toupper(sub("_bin_label$", "", v))
  label_value <- unname(individual_comp_labels[raw_name])
  if (is.na(label_value) || length(label_value) == 0) label_value <- v
  table3_label_list[[v]] <- label_value
}
# gtsummary labels are most reliable as a formula-style list.
table3_label_formulas <- purrr::imap(
  table3_label_list,
  ~ stats::as.formula(paste0(.y, " ~ \"", .x, "\""))
)

table3_gts <- dat %>%
  dplyr::select(dplyr::all_of(c("procedure", complication_label_vars))) %>%
  tbl_summary(
    by = procedure,
    statistic = dplyr::all_of(complication_label_vars) ~ "{n} ({p}%)",
    label = table3_label_formulas,
    missing = "ifany"
  ) %>%
  add_p(
    test = dplyr::all_of(complication_label_vars) ~ "chisq.test.no.correct",
    pvalue_fun = fmt_p
  ) %>%
  modify_caption("Table 3. Individual postoperative complications by procedure type") %>%
  bold_labels()

table3_ft <- gtsummary_to_three_line(table3_gts, font_size = 7)

# 4) Univariate model summaries ----
# These are optional support tables. They help translate Table 2 into model-based
# estimates when the outcome is binary or count.

univ_logistic <- glm(surgical_complication ~ procedure, data = dat, family = binomial)
univ_count_pois <- glm(number_complications ~ procedure, data = dat, family = poisson)
univ_mortality <- glm(mortality ~ procedure, data = dat, family = binomial)
univ_reop <- glm(reoperation ~ procedure, data = dat, family = binomial)
univ_los <- lm(log(los_days) ~ procedure, data = dat)

univ_model_results <- dplyr::bind_rows(
  tidy_exp_model(univ_logistic, "Any complication, logistic OR"),
  tidy_exp_model(univ_count_pois, "Number of complications, Poisson IRR"),
  tidy_exp_model(univ_mortality, "Mortality, logistic OR"),
  tidy_exp_model(univ_reop, "Reoperation, logistic OR"),
  tidy_exp_model(univ_los, "Length of stay, log-linear ratio")
)
write_csv_safely(univ_model_results, file.path(OUT_TABLES, "01_univariate_model_results.csv"))

univ_model_ft <- univ_model_results %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3)
  ) %>%
  dplyr::select(Model = model, Term = term, `Estimate (95% CI)` = Estimate_95_CI, `P value` = p_value) %>%
  flextable::flextable() %>%
  make_three_line(font_size = 8)

# 5) Multivariable logistic model for complication rate ----
# The exam asks whether the effect of procedure type on complication rate differs by age or BMI.
# This model includes procedure-by-age and procedure-by-BMI interactions.

binary_model_vars <- c("surgical_complication", "procedure", "age_years", "bmi", "sex", "race_ethnicity", "smoking_status", "asa_severity")
binary_dat <- dat %>%
  dplyr::select(dplyr::all_of(binary_model_vars)) %>%
  tidyr::drop_na()

logistic_main <- glm(
  surgical_complication ~ procedure + age_years + bmi + sex + race_ethnicity + smoking_status + asa_severity,
  data = binary_dat,
  family = binomial
)

logistic_interaction <- glm(
  surgical_complication ~ procedure * age_years + procedure * bmi + sex + race_ethnicity + smoking_status + asa_severity,
  data = binary_dat,
  family = binomial
)

# Likelihood ratio test for adding the two interaction terms.
logistic_lrt <- anova(logistic_main, logistic_interaction, test = "LRT")
logistic_lrt_table <- as.data.frame(logistic_lrt) %>%
  tibble::rownames_to_column("Model") %>%
  as_tibble() %>%
  dplyr::mutate(
    `P value` = fmt_p(`Pr(>Chi)`)
  )
write_csv_safely(logistic_lrt_table, file.path(OUT_TABLES, "02_logistic_interaction_lrt.csv"))

logistic_gts <- tbl_regression(
  logistic_interaction,
  exponentiate = TRUE,
  label = list(
    procedure ~ "Procedure type",
    age_years ~ "Age, years",
    bmi ~ "Body mass index, kg/m^2",
    sex ~ "Sex",
    race_ethnicity ~ "Race/ethnicity",
    smoking_status ~ "Current smoker within one year",
    asa_severity ~ "ASA severity"
  ),
  estimate_fun = ~ style_sigfig(.x, digits = 3),
  pvalue_fun = fmt_p
) %>%
  modify_caption("Table 4. Multivariable logistic regression model for any surgical complication") %>%
  bold_labels()
logistic_ft <- gtsummary_to_three_line(logistic_gts, font_size = 7)
write_csv_safely(tidy_exp_model(logistic_interaction, "Multivariable logistic model"), file.path(OUT_TABLES, "03_logistic_interaction_model.csv"))

# 6) Multivariable count model for number of complications ----
count_model_vars <- c("number_complications", "procedure", "age_years", "bmi", "sex", "race_ethnicity", "smoking_status", "asa_severity")
count_dat <- dat %>%
  dplyr::select(dplyr::all_of(count_model_vars)) %>%
  tidyr::drop_na()

count_pois_main <- glm(
  number_complications ~ procedure + age_years + bmi + sex + race_ethnicity + smoking_status + asa_severity,
  data = count_dat,
  family = poisson
)
count_pois_interaction <- glm(
  number_complications ~ procedure * age_years + procedure * bmi + sex + race_ethnicity + smoking_status + asa_severity,
  data = count_dat,
  family = poisson
)

overdispersion_ratio <- sum(residuals(count_pois_interaction, type = "pearson")^2) / df.residual(count_pois_interaction)

if (is.finite(overdispersion_ratio) && overdispersion_ratio > 1.5) {
  count_final <- MASS::glm.nb(
    number_complications ~ procedure * age_years + procedure * bmi + sex + race_ethnicity + smoking_status + asa_severity,
    data = count_dat
  )
  count_model_name <- "Negative binomial regression"
} else {
  count_final <- count_pois_interaction
  count_model_name <- "Poisson regression"
}

count_lrt <- anova(count_pois_main, count_pois_interaction, test = "Chisq")
count_lrt_table <- as.data.frame(count_lrt) %>%
  tibble::rownames_to_column("Model") %>%
  as_tibble() %>%
  dplyr::mutate(`P value` = fmt_p(`Pr(>Chi)`))
write_csv_safely(count_lrt_table, file.path(OUT_TABLES, "04_count_interaction_lrt_poisson.csv"))

count_gts <- tbl_regression(
  count_final,
  exponentiate = TRUE,
  label = list(
    procedure ~ "Procedure type",
    age_years ~ "Age, years",
    bmi ~ "Body mass index, kg/m^2",
    sex ~ "Sex",
    race_ethnicity ~ "Race/ethnicity",
    smoking_status ~ "Current smoker within one year",
    asa_severity ~ "ASA severity"
  ),
  estimate_fun = ~ style_sigfig(.x, digits = 3),
  pvalue_fun = fmt_p
) %>%
  modify_caption(paste0("Table 5. ", count_model_name, " for number of surgical complications")) %>%
  bold_labels()
count_ft <- gtsummary_to_three_line(count_gts, font_size = 7)
write_csv_safely(tidy_exp_model(count_final, paste0("Count model: ", count_model_name)), file.path(OUT_TABLES, "05_count_model.csv"))

count_diagnostic_table <- tibble::tibble(
  Diagnostic = c("Poisson overdispersion ratio", "Final count model selected"),
  Value = c(round(overdispersion_ratio, 3), count_model_name)
)
count_diag_ft <- count_diagnostic_table %>%
  flextable::flextable() %>%
  make_three_line(font_size = 9)

# 7) Length of stay descriptive and optional model ----
los_dat <- dat %>%
  dplyr::select(los_days, procedure, age_years, bmi, sex, race_ethnicity, smoking_status, asa_severity) %>%
  tidyr::drop_na() %>%
  dplyr::filter(los_days > 0)

los_model <- lm(
  log(los_days) ~ procedure + age_years + bmi + sex + race_ethnicity + smoking_status + asa_severity,
  data = los_dat
)

los_gts <- tbl_regression(
  los_model,
  exponentiate = TRUE,
  label = list(
    procedure ~ "Procedure type",
    age_years ~ "Age, years",
    bmi ~ "Body mass index, kg/m^2",
    sex ~ "Sex",
    race_ethnicity ~ "Race/ethnicity",
    smoking_status ~ "Current smoker within one year",
    asa_severity ~ "ASA severity"
  ),
  estimate_fun = ~ style_sigfig(.x, digits = 3),
  pvalue_fun = fmt_p
) %>%
  modify_caption("Appendix Table. Log-linear model for length of hospital stay") %>%
  bold_labels()
los_ft <- gtsummary_to_three_line(los_gts, font_size = 8)
write_csv_safely(tidy_exp_model(los_model, "Log-linear LOS model"), file.path(OUT_TABLES, "06_los_loglinear_model.csv"))

# 8) Figures ----
# Figure 1: outcome rates by procedure.
outcome_plot_df <- dat %>%
  dplyr::filter(!is.na(procedure)) %>%
  dplyr::group_by(procedure) %>%
  dplyr::summarise(
    N = dplyr::n(),
    AnyComplication = mean(surgical_complication == 1, na.rm = TRUE),
    Mortality = mean(mortality == 1, na.rm = TRUE),
    Reoperation = mean(reoperation == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = c(AnyComplication, Mortality, Reoperation), names_to = "Outcome", values_to = "Rate")

fig1 <- ggplot(outcome_plot_df, aes(x = procedure, y = Rate, fill = procedure)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Procedure type", y = "Observed rate", title = "Observed postoperative outcome rates by procedure type") +
  theme_bw(base_size = 11)
fig1_path <- file.path(OUT_FIGURES, "Figure_1_Observed_Outcome_Rates_by_Procedure.png")
save_plot(fig1, fig1_path, width = 7, height = 4.5)

# Figure 2: predicted probability of any complication by age and procedure.
new_age <- expand.grid(
  procedure = levels(binary_dat$procedure),
  age_years = seq(20, 75, by = 1),
  bmi = median(binary_dat$bmi, na.rm = TRUE),
  sex = mode_value(binary_dat$sex),
  race_ethnicity = mode_value(binary_dat$race_ethnicity),
  smoking_status = mode_value(binary_dat$smoking_status),
  asa_severity = mode_value(binary_dat$asa_severity)
) %>% as_tibble()
new_age <- new_age %>%
  dplyr::mutate(
    procedure = factor(procedure, levels = levels(binary_dat$procedure)),
    sex = factor(sex, levels = levels(binary_dat$sex)),
    race_ethnicity = factor(race_ethnicity, levels = levels(binary_dat$race_ethnicity)),
    smoking_status = factor(smoking_status, levels = levels(binary_dat$smoking_status)),
    asa_severity = factor(asa_severity, levels = levels(binary_dat$asa_severity))
  )
new_age$pred <- predict(logistic_interaction, newdata = new_age, type = "response")
fig2 <- ggplot(new_age, aes(x = age_years, y = pred, color = procedure)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Age, years", y = "Adjusted predicted probability", color = "Procedure type", title = "Adjusted predicted probability of any complication by age") +
  theme_bw(base_size = 11)
fig2_path <- file.path(OUT_FIGURES, "Figure_2_Logistic_Interaction_Age.png")
save_plot(fig2, fig2_path, width = 7, height = 4.5)

# Figure 3: predicted probability by BMI and procedure.
new_bmi <- expand.grid(
  procedure = levels(binary_dat$procedure),
  age_years = median(binary_dat$age_years, na.rm = TRUE),
  bmi = seq(35, 70, by = 0.5),
  sex = mode_value(binary_dat$sex),
  race_ethnicity = mode_value(binary_dat$race_ethnicity),
  smoking_status = mode_value(binary_dat$smoking_status),
  asa_severity = mode_value(binary_dat$asa_severity)
) %>% as_tibble()
new_bmi <- new_bmi %>%
  dplyr::mutate(
    procedure = factor(procedure, levels = levels(binary_dat$procedure)),
    sex = factor(sex, levels = levels(binary_dat$sex)),
    race_ethnicity = factor(race_ethnicity, levels = levels(binary_dat$race_ethnicity)),
    smoking_status = factor(smoking_status, levels = levels(binary_dat$smoking_status)),
    asa_severity = factor(asa_severity, levels = levels(binary_dat$asa_severity))
  )
new_bmi$pred <- predict(logistic_interaction, newdata = new_bmi, type = "response")
fig3 <- ggplot(new_bmi, aes(x = bmi, y = pred, color = procedure)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "BMI, kg/m^2", y = "Adjusted predicted probability", color = "Procedure type", title = "Adjusted predicted probability of any complication by BMI") +
  theme_bw(base_size = 11)
fig3_path <- file.path(OUT_FIGURES, "Figure_3_Logistic_Interaction_BMI.png")
save_plot(fig3, fig3_path, width = 7, height = 4.5)

# Figure 4: LOS distribution.
fig4 <- ggplot(dat %>% dplyr::filter(!is.na(los_days), los_days > 0), aes(x = los_days)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "white") +
  labs(x = "Length of hospital stay, days", y = "Number of patients", title = "Distribution of length of hospital stay") +
  theme_bw(base_size = 11)
fig4_path <- file.path(OUT_APPENDIX, "Appendix_LOS_Distribution.png")
save_plot(fig4, fig4_path, width = 7, height = 4.5)

# 9) Diagnostics ----
# Logistic calibration by deciles of predicted risk.
binary_dat$pred_logistic <- predict(logistic_interaction, type = "response")
calib_df <- binary_dat %>%
  dplyr::mutate(pred_decile = dplyr::ntile(pred_logistic, 10)) %>%
  dplyr::group_by(pred_decile) %>%
  dplyr::summarise(
    mean_predicted = mean(pred_logistic, na.rm = TRUE),
    observed = mean(surgical_complication == 1, na.rm = TRUE),
    N = dplyr::n(),
    .groups = "drop"
  )
fig_calib <- ggplot(calib_df, aes(x = mean_predicted, y = observed)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Mean predicted probability", y = "Observed complication rate", title = "Logistic model calibration by predicted-risk decile") +
  theme_bw(base_size = 11)
fig_calib_path <- file.path(OUT_APPENDIX, "Appendix_Logistic_Calibration.png")
save_plot(fig_calib, fig_calib_path, width = 6, height = 5)

# Count model residuals versus fitted values.
count_diag_df <- count_dat %>%
  dplyr::mutate(
    fitted_count = fitted(count_final),
    pearson_resid = residuals(count_final, type = "pearson")
  )
fig_count_resid <- ggplot(count_diag_df, aes(x = fitted_count, y = pearson_resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fitted number of complications", y = "Pearson residuals", title = "Count model residuals versus fitted values") +
  theme_bw(base_size = 11)
fig_count_resid_path <- file.path(OUT_APPENDIX, "Appendix_Count_Residuals.png")
save_plot(fig_count_resid, fig_count_resid_path, width = 6, height = 5)

# LOS residual plot.
los_diag_df <- los_dat %>%
  dplyr::mutate(
    fitted_log_los = fitted(los_model),
    resid_log_los = residuals(los_model)
  )
fig_los_resid <- ggplot(los_diag_df, aes(x = fitted_log_los, y = resid_log_los)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fitted log length of stay", y = "Residuals", title = "Log-linear LOS model residuals versus fitted values") +
  theme_bw(base_size = 11)
fig_los_resid_path <- file.path(OUT_APPENDIX, "Appendix_LOS_Residuals.png")
save_plot(fig_los_resid, fig_los_resid_path, width = 6, height = 5)

# 10) Export Word tables and figures ----
main_doc <- officer::read_docx()
main_doc <- officer::body_add_par(main_doc, PROJECT_TITLE, style = "heading 1")
main_doc <- officer::body_add_par(main_doc, paste0("Generated: ", Sys.Date()), style = "Normal")
main_doc <- officer::body_add_par(
  main_doc,
  "This document contains formatted three-line tables and figures generated from the R analysis script. These outputs are based on synthetic practice data and should not be interpreted as real clinical findings.",
  style = "Normal"
)
main_doc <- officer::body_add_par(main_doc, "", style = "Normal")

main_doc <- add_table_to_doc(
  main_doc,
  "Table 1. Patient characteristics by procedure type",
  table1_ft,
  note = "Continuous variables are summarized as median (IQR) and mean (SD). Categorical variables are summarized as n (%). P values are from Wilcoxon rank-sum tests for continuous variables and chi-square tests for categorical variables."
)
main_doc <- add_table_to_doc(
  main_doc,
  "Table 2. Outcomes by procedure type",
  table2_ft,
  note = "Surgical complication is defined as any listed postoperative complication. Number of complications is the count of listed complications."
)
main_doc <- add_table_to_doc(
  main_doc,
  "Table 3. Individual postoperative complications by procedure type",
  table3_ft,
  note = "Each individual complication is summarized as n (%) by procedure type."
)
main_doc <- add_table_to_doc(
  main_doc,
  "Table 4. Univariate model summaries by procedure type",
  univ_model_ft,
  note = "Exponentiated estimates are odds ratios for binary outcomes, incidence rate ratios for count outcomes, and ratios of geometric means for log length of stay."
)
main_doc <- add_table_to_doc(
  main_doc,
  "Table 5. Multivariable logistic regression for any surgical complication",
  logistic_ft,
  note = "The model includes procedure-by-age and procedure-by-BMI interactions and adjusts for sex, race/ethnicity, smoking status, and ASA severity."
)
main_doc <- add_table_to_doc(
  main_doc,
  "Table 6. Multivariable count model for number of surgical complications",
  count_ft,
  note = paste0("The final count model was selected as: ", count_model_name, ".")
)
main_doc <- add_table_to_doc(
  main_doc,
  "Table 7. Count model overdispersion diagnostic",
  count_diag_ft,
  note = "If the Poisson overdispersion ratio is meaningfully greater than 1, a negative binomial model is usually preferred."
)

for (fig in c(fig1_path, fig2_path, fig3_path)) {
  if (file.exists(fig)) {
    main_doc <- officer::body_add_par(main_doc, tools::file_path_sans_ext(basename(fig)), style = "heading 2")
    main_doc <- officer::body_add_img(main_doc, src = fig, width = 6.5, height = 4.2)
    main_doc <- officer::body_add_par(main_doc, "", style = "Normal")
  }
}

main_word_path <- file.path(OUT_WORD, "QE2024_NSQIP_Bariatric_Main_Tables_ThreeLine.docx")
print(main_doc, target = main_word_path)

appendix_doc <- officer::read_docx()
appendix_doc <- officer::body_add_par(appendix_doc, "Appendix, Diagnostics and Supporting Analyses", style = "heading 1")
appendix_doc <- officer::body_add_par(appendix_doc, paste0("Generated: ", Sys.Date()), style = "Normal")
appendix_doc <- officer::body_add_par(appendix_doc, "", style = "Normal")

flow_ft <- flow_counts %>% flextable::flextable() %>% make_three_line(font_size = 9)
missing_ft <- missingness_table %>% flextable::flextable() %>% make_three_line(font_size = 7)
logistic_lrt_ft <- logistic_lrt_table %>% flextable::flextable() %>% make_three_line(font_size = 8)
count_lrt_ft <- count_lrt_table %>% flextable::flextable() %>% make_three_line(font_size = 8)

appendix_doc <- add_table_to_doc(appendix_doc, "Appendix Table A1. Cohort flow counts", flow_ft)
appendix_doc <- add_table_to_doc(appendix_doc, "Appendix Table A2. Missingness summary", missing_ft)
appendix_doc <- add_table_to_doc(appendix_doc, "Appendix Table A3. Logistic model interaction likelihood-ratio test", logistic_lrt_ft)
appendix_doc <- add_table_to_doc(appendix_doc, "Appendix Table A4. Count model interaction likelihood-ratio test", count_lrt_ft)
appendix_doc <- add_table_to_doc(appendix_doc, "Appendix Table A5. Log-linear model for length of hospital stay", los_ft)

for (fig in c(fig4_path, fig_calib_path, fig_count_resid_path, fig_los_resid_path)) {
  if (file.exists(fig)) {
    appendix_doc <- officer::body_add_par(appendix_doc, tools::file_path_sans_ext(basename(fig)), style = "heading 2")
    appendix_doc <- officer::body_add_img(appendix_doc, src = fig, width = 6.4, height = 4.6)
    appendix_doc <- officer::body_add_par(appendix_doc, "", style = "Normal")
  }
}

appendix_word_path <- file.path(OUT_WORD, "QE2024_NSQIP_Bariatric_Appendix_Diagnostics_ThreeLine.docx")
print(appendix_doc, target = appendix_word_path)

# 11) Session information ----
session_info_path <- file.path(OUT_TABLES, "99_session_info.txt")
writeLines(capture.output(sessionInfo()), session_info_path)

message("Analysis complete.")
message("Main Word output: ", main_word_path)
message("Appendix Word output: ", appendix_word_path)
message("Tables folder: ", OUT_TABLES)
message("Figures folder: ", OUT_FIGURES)
message("Appendix folder: ", OUT_APPENDIX)
