# 0) Setup ----
# SEARCH KEYS: KEY_2022_PFT_SURVIVAL, KEY_FOUR_PFT_SEPARATE_MODELS, KEY_UNIVARIATE_COX
# SEARCH KEYS: KEY_PFT_COMPLICATION_LOGISTIC, KEY_LENGTH_OF_STAY_LOG_LINEAR
# SEARCH KEYS: KEY_COX_PH_SCHOENFELD, KEY_INTERACTION_EFFECT_MODIFICATION
# SEARCH KEYS: KEY_THREE_LINE_WORD_OUTPUT, KEY_MISSING_COMPLETE_CASE
# Project: 2022 PhD Qualifying Exam Applied Practice
# Data: Synthetic lung cancer resection cohort (N approximately 130)
# IMPORTANT: FEV1, FEV1 percent predicted, DLCO, and DLCO percent predicted
#            are analyzed in SEPARATE models, as required by the exam prompt.
# Open the 2022 project folder (or its .Rproj file) before running this script.

rm(list = ls())

PROJECT_TITLE <- "QE2022 Applied Practice: Pulmonary Function, Lung Cancer Outcomes, and Survival"
DATA_PATH <- "data/raw/FEV1_LungCa_synthetic.csv"

OUT_TABLES <- "output/tables"
OUT_FIGURES <- "output/figures"
OUT_APPENDIX <- "output/appendix"
OUT_WORD <- "output/word"
for (d in c(OUT_TABLES, OUT_FIGURES, OUT_APPENDIX, OUT_WORD)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

required_packages <- c(
  "tidyverse", "janitor", "survival", "survminer", "broom",
  "flextable", "officer", "readxl", "haven", "MASS", "splines", "scales"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages first: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

# Do not attach MASS because MASS::select() masks dplyr::select().
library(tidyverse)
library(janitor)
library(survival)
library(survminer)
library(broom)
library(flextable)
library(officer)

# 0.1) Helper functions ----
fmt_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}
fmt_est_ci <- function(est, lo, hi, digits = 2) {
  ifelse(is.na(est), "", paste0(formatC(est, digits = digits, format = "f"), " (",
                                formatC(lo, digits = digits, format = "f"), ", ",
                                formatC(hi, digits = digits, format = "f"), ")"))
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
add_table_to_doc <- function(doc, title, ft, note = NULL) {
  doc <- officer::body_add_par(doc, title, style = "heading 2")
  doc <- flextable::body_add_flextable(doc, value = ft)
  if (!is.null(note)) doc <- officer::body_add_par(doc, note, style = "Normal")
  officer::body_add_par(doc, "", style = "Normal")
}
write_csv_safely <- function(x, path) readr::write_csv(x, path, na = "")
safe_coxph <- function(formula, data) {
  tryCatch(survival::coxph(formula, data = data, x = TRUE, y = TRUE, model = TRUE),
           error = function(e) { message("Cox model failed: ", e$message); NULL })
}
safe_glm <- function(formula, data, family) {
  tryCatch(
    withCallingHandlers(
      stats::glm(formula, data = data, family = family, model = TRUE),
      warning = function(w) {
        message("GLM warning: ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) { message("GLM failed: ", e$message); NULL }
  )
}
num_summary <- function(data, var, label) {
  x <- data[[var]]
  tibble::tibble(
    Characteristic = label,
    N = sum(!is.na(x)),
    `Mean (SD)` = ifelse(sum(!is.na(x)) > 0, sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)), ""),
    `Median (Q1, Q3)` = ifelse(sum(!is.na(x)) > 0,
      sprintf("%.1f (%.1f, %.1f)", median(x, na.rm = TRUE), quantile(x, .25, na.rm = TRUE), quantile(x, .75, na.rm = TRUE)), ""),
    Missing = sum(is.na(x))
  )
}
cat_summary <- function(data, var, label) {
  x <- data[[var]]
  denom <- sum(!is.na(x))
  tibble::tibble(Level = names(table(x, useNA = "no")), n = as.integer(table(x, useNA = "no"))) %>%
    dplyr::mutate(
      Characteristic = label,
      Summary = ifelse(denom > 0, paste0(n, " (", sprintf("%.1f", 100*n/denom), "%)"), "")
    ) %>%
    dplyr::select(Characteristic, Level, Summary)
}
tidy_cox_hr <- function(model, model_name, exposure_label = NULL) {
  if (is.null(model)) return(tibble::tibble())
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::mutate(
      Model = model_name,
      `HR (95% CI)` = fmt_est_ci(estimate, conf.low, conf.high, 2),
      `P value` = fmt_p(p.value)
    ) %>%
    dplyr::select(Model, term, estimate, conf.low, conf.high, `HR (95% CI)`, `P value`)
}
tidy_or <- function(model, model_name) {
  if (is.null(model)) return(tibble::tibble())
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::mutate(Model = model_name,
                  `OR (95% CI)` = fmt_est_ci(estimate, conf.low, conf.high, 2),
                  `P value` = fmt_p(p.value)) %>%
    dplyr::select(Model, term, estimate, conf.low, conf.high, `OR (95% CI)`, `P value`)
}
tidy_log_ratio <- function(model, model_name) {
  if (is.null(model)) return(tibble::tibble())
  broom::tidy(model, conf.int = TRUE) %>%
    dplyr::mutate(
      Model = model_name,
      ratio = exp(estimate), low = exp(conf.low), high = exp(conf.high),
      `Geometric mean ratio (95% CI)` = fmt_est_ci(ratio, low, high, 2),
      `P value` = fmt_p(p.value)
    ) %>%
    dplyr::select(Model, term, ratio, low, high, `Geometric mean ratio (95% CI)`, `P value`)
}

# 1) Import data ----
if (!file.exists(DATA_PATH)) stop("DATA_PATH not found: ", DATA_PATH, call. = FALSE)
if (grepl("\\.csv$", DATA_PATH, ignore.case = TRUE)) {
  dat_raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
} else if (grepl("\\.xlsx$|\\.xls$", DATA_PATH, ignore.case = TRUE)) {
  dat_raw <- readxl::read_excel(DATA_PATH) %>% tibble::as_tibble()
} else if (grepl("\\.sas7bdat$", DATA_PATH, ignore.case = TRUE)) {
  dat_raw <- haven::read_sas(DATA_PATH) %>% tibble::as_tibble()
} else stop("Use CSV, XLS/XLSX, or SAS7BDAT.", call. = FALSE)

# 2) Clean variables and define analysis variables ----
# janitor::clean_names() converts the original mixed-case names to stable snake_case.
dat <- dat_raw %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(trimws(.x), ""))) %>%
  dplyr::mutate(
    id = as.integer(id), age = as.numeric(age), tumor_size_cm = as.numeric(tumor_size_cm),
    fev1 = as.numeric(fev1), fev_percent_of_predicted = as.numeric(fev_percent_of_predicted),
    dlco = as.numeric(dlco), dlco_percent_of_predicted = as.numeric(dlco_percent_of_predicted),
    length_of_hospital_stay = as.numeric(length_of_hospital_stay),
    oasmons = as.numeric(oasmons), died = as.integer(died),
    sex = factor(sex, levels = c("Female", "Male")),
    smoking = factor(smoking, levels = c("Never", "Former", "Current")),
    type_of_surgery = factor(type_of_surgery, levels = c("Wedge/segmentectomy", "Lobectomy", "Pneumonectomy")),
    pathologic_stage = factor(pathologic_stage, levels = c("I", "II", "III")),
    histology = factor(histology),
    dplyr::across(c(hilar_ln_involved, mediastinal_ln_involved, respiratory, cardiac,
                    stroke, ckd, dm, othcomorb, respcomp, cardcomp, deathcomp,
                    infectcomp, anycomp), ~ factor(.x, levels = c("No", "Yes")))
  ) %>%
  dplyr::mutate(
    event = dplyr::if_else(died == 1L, 1L, 0L, missing = NA_integer_),
    followup_months = oasmons,
    anycomp_bin = dplyr::case_when(anycomp == "Yes" ~ 1L, anycomp == "No" ~ 0L, TRUE ~ NA_integer_),
    respcomp_bin = dplyr::case_when(respcomp == "Yes" ~ 1L, respcomp == "No" ~ 0L, TRUE ~ NA_integer_),
    cardcomp_bin = dplyr::case_when(cardcomp == "Yes" ~ 1L, cardcomp == "No" ~ 0L, TRUE ~ NA_integer_),
    infectcomp_bin = dplyr::case_when(infectcomp == "Yes" ~ 1L, infectcomp == "No" ~ 0L, TRUE ~ NA_integer_),
    deathcomp_bin = dplyr::case_when(deathcomp == "Yes" ~ 1L, deathcomp == "No" ~ 0L, TRUE ~ NA_integer_),
    # Clinically interpretable increments for regression.
    fev1_per_0_5l = fev1 / 0.5,
    fev_pct_per_10 = fev_percent_of_predicted / 10,
    dlco_per_5 = dlco / 5,
    dlco_pct_per_10 = dlco_percent_of_predicted / 10
  )

pft_map <- tibble::tribble(
  ~raw_var, ~model_var, ~label, ~unit,
  "fev1", "fev1_per_0_5l", "FEV1", "per 0.5-L increase",
  "fev_percent_of_predicted", "fev_pct_per_10", "FEV1 percent predicted", "per 10-percentage-point increase",
  "dlco", "dlco_per_5", "DLCO", "per 5-unit increase",
  "dlco_percent_of_predicted", "dlco_pct_per_10", "DLCO percent predicted", "per 10-percentage-point increase"
)
PFT_MODEL_VARS <- pft_map$model_var
CORE_COVARIATES <- c("age", "sex", "smoking", "type_of_surgery", "pathologic_stage", "tumor_size_cm")
FULL_CANDIDATE_COVARIATES <- c(CORE_COVARIATES, "histology", "hilar_ln_involved",
                               "mediastinal_ln_involved", "respiratory", "cardiac",
                               "stroke", "ckd", "dm", "othcomorb")
INTERACTION_MODIFIERS <- c("age", "sex", "smoking", "pathologic_stage", "respiratory")

# 3) Cohort flow and missingness ----
flow_counts <- tibble::tibble(
  Step = c("Raw records imported", "Positive nonmissing survival time", "Known event indicator", "Included in survival analysis"),
  N = c(nrow(dat), sum(!is.na(dat$followup_months) & dat$followup_months > 0),
        sum(!is.na(dat$event)), sum(!is.na(dat$followup_months) & dat$followup_months > 0 & !is.na(dat$event)))
)
analysis_vars <- unique(c("age","sex","smoking","type_of_surgery","pathologic_stage","histology",
                          "tumor_size_cm","hilar_ln_involved","mediastinal_ln_involved",
                          pft_map$raw_var, "length_of_hospital_stay","respiratory","cardiac","stroke","ckd","dm","othcomorb",
                          "respcomp","cardcomp","deathcomp","infectcomp","anycomp","followup_months","event"))
missingness_table <- purrr::map_dfr(analysis_vars, ~ tibble::tibble(
  Variable = .x, Missing_N = sum(is.na(dat[[.x]])), Total_N = nrow(dat),
  Missing_Percent = round(100*mean(is.na(dat[[.x]])),1)
)) %>% dplyr::arrange(dplyr::desc(Missing_Percent), Variable)
write_csv_safely(flow_counts, file.path(OUT_TABLES,"00_cohort_flow.csv"))
write_csv_safely(missingness_table, file.path(OUT_TABLES,"00_missingness.csv"))

# 4) Descriptive analysis ----
baseline_num <- dplyr::bind_rows(
  num_summary(dat,"age","Age, years"),
  num_summary(dat,"tumor_size_cm","Tumor size, cm"),
  num_summary(dat,"length_of_hospital_stay","Length of hospital stay, days")
)
baseline_cat <- dplyr::bind_rows(
  cat_summary(dat,"sex","Sex"), cat_summary(dat,"smoking","Smoking status"),
  cat_summary(dat,"type_of_surgery","Type of surgery"), cat_summary(dat,"pathologic_stage","Pathologic stage"),
  cat_summary(dat,"histology","Histology"), cat_summary(dat,"respiratory","Respiratory comorbidity"),
  cat_summary(dat,"cardiac","Cardiac comorbidity"), cat_summary(dat,"dm","Diabetes")
)
pft_summary <- purrr::map2_dfr(pft_map$raw_var, pft_map$label, ~ num_summary(dat,.x,.y))
outcome_summary <- tibble::tibble(
  Outcome = c("Deaths", "Censored", "Any postoperative complication", "Respiratory complication", "Cardiac complication", "Infectious complication", "Postoperative death complication"),
  `n (%)` = c(
    paste0(sum(dat$event==1,na.rm=TRUE)," (",sprintf("%.1f",100*mean(dat$event==1,na.rm=TRUE)),"%)"),
    paste0(sum(dat$event==0,na.rm=TRUE)," (",sprintf("%.1f",100*mean(dat$event==0,na.rm=TRUE)),"%)"),
    paste0(sum(dat$anycomp_bin==1,na.rm=TRUE)," (",sprintf("%.1f",100*mean(dat$anycomp_bin==1,na.rm=TRUE)),"%)"),
    paste0(sum(dat$respcomp_bin==1,na.rm=TRUE)," (",sprintf("%.1f",100*mean(dat$respcomp_bin==1,na.rm=TRUE)),"%)"),
    paste0(sum(dat$cardcomp_bin==1,na.rm=TRUE)," (",sprintf("%.1f",100*mean(dat$cardcomp_bin==1,na.rm=TRUE)),"%)"),
    paste0(sum(dat$infectcomp_bin==1,na.rm=TRUE)," (",sprintf("%.1f",100*mean(dat$infectcomp_bin==1,na.rm=TRUE)),"%)"),
    paste0(sum(dat$deathcomp_bin==1,na.rm=TRUE)," (",sprintf("%.1f",100*mean(dat$deathcomp_bin==1,na.rm=TRUE)),"%)")
  )
)
write_csv_safely(baseline_num, file.path(OUT_TABLES,"01_baseline_continuous.csv"))
write_csv_safely(baseline_cat, file.path(OUT_TABLES,"01_baseline_categorical.csv"))
write_csv_safely(pft_summary, file.path(OUT_TABLES,"02_pft_summary.csv"))
pft_correlation <- stats::cor(dat %>% dplyr::select(dplyr::all_of(pft_map$raw_var)),
                              use = "pairwise.complete.obs", method = "pearson") %>%
  as.data.frame() %>% tibble::rownames_to_column("PFT") %>% tibble::as_tibble()
write_csv_safely(pft_correlation, file.path(OUT_TABLES,"02b_pft_correlation_matrix.csv"))
write_csv_safely(outcome_summary, file.path(OUT_TABLES,"03_outcome_summary.csv"))

# Distribution figures
pft_long <- dat %>% dplyr::select(dplyr::all_of(pft_map$raw_var)) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to="PFT", values_to="Value")
p_pft <- ggplot2::ggplot(pft_long, ggplot2::aes(Value)) + ggplot2::geom_histogram(bins=20) +
  ggplot2::facet_wrap(~PFT, scales="free") + ggplot2::theme_bw(base_size=10) +
  ggplot2::labs(title="Distribution of preoperative pulmonary function measures", y="Count")
ggplot2::ggsave(file.path(OUT_FIGURES,"Figure_1_PFT_distributions.png"), p_pft, width=8, height=5.5, dpi=300)
p_los <- ggplot2::ggplot(dat, ggplot2::aes(length_of_hospital_stay)) + ggplot2::geom_histogram(bins=20) +
  ggplot2::theme_bw(base_size=11) + ggplot2::labs(x="Length of hospital stay, days", y="Count")
ggplot2::ggsave(file.path(OUT_APPENDIX,"Appendix_LOS_distribution.png"), p_los, width=6, height=4, dpi=300)

# 5) Overall survival: KM and univariate Cox ----
surv_dat <- dat %>% dplyr::filter(!is.na(followup_months), followup_months > 0, !is.na(event))
km_overall <- survival::survfit(survival::Surv(followup_months,event) ~ 1, data=surv_dat)
km_plot <- survminer::ggsurvplot(km_overall, data=surv_dat, conf.int=TRUE, risk.table=TRUE,
  xlab="Months after surgery", ylab="Overall survival probability", break.time.by=12,
  ggtheme=ggplot2::theme_bw(base_size=11), risk.table.height=.25)
ggplot2::ggsave(file.path(OUT_FIGURES,"Figure_2_Overall_Kaplan_Meier.png"), km_plot$plot, width=7, height=5, dpi=300)
# Save combined KM + risk table safely
png(file.path(OUT_FIGURES,"Figure_2_Overall_Kaplan_Meier_with_risk_table.png"), width=2100,height=1800,res=300)
print(km_plot)
dev.off()

univ_vars <- c("age","sex","smoking","type_of_surgery","pathologic_stage","histology","tumor_size_cm",
               "hilar_ln_involved","mediastinal_ln_involved","respiratory","cardiac","stroke","ckd","dm","othcomorb", PFT_MODEL_VARS)
univ_results <- purrr::map_dfr(univ_vars, function(v) {
  d <- dat %>% dplyr::select(followup_months,event,dplyr::all_of(v)) %>% tidyr::drop_na()
  m <- safe_coxph(stats::as.formula(paste0("Surv(followup_months,event) ~ `",v,"`")), d)
  tidy_cox_hr(m, paste0("Univariate: ",v))
})
write_csv_safely(univ_results, file.path(OUT_TABLES,"04_univariate_cox.csv"))

# 6) Four separate adjusted Cox models ----
fit_adjusted_cox <- function(model_var, label, unit) {
  needed <- c("followup_months","event",model_var,CORE_COVARIATES)
  d <- dat %>% dplyr::select(dplyr::all_of(needed)) %>% tidyr::drop_na()
  f <- stats::as.formula(paste("Surv(followup_months,event) ~", paste(c(model_var,CORE_COVARIATES),collapse=" + ")))
  m <- safe_coxph(f,d)
  list(data=d, model=m, result=tidy_cox_hr(m,paste0(label," ",unit)))
}
cox_fits <- purrr::pmap(pft_map[,c("model_var","label","unit")], fit_adjusted_cox)
names(cox_fits) <- pft_map$model_var
adjusted_cox_results <- purrr::map_dfr(cox_fits,"result")
write_csv_safely(adjusted_cox_results, file.path(OUT_TABLES,"05_adjusted_cox_four_separate_pft_models.csv"))

# Compact primary-exposure rows for the report
primary_cox_rows <- purrr::map2_dfr(pft_map$model_var, seq_len(nrow(pft_map)), function(v,i) {
  x <- cox_fits[[v]]$result %>% dplyr::filter(term == v)
  if (nrow(x)==0) return(tibble::tibble())
  tibble::tibble(PFT=pft_map$label[i], Increment=pft_map$unit[i], N=nrow(cox_fits[[v]]$data),
                 `Adjusted HR (95% CI)`=x$`HR (95% CI)`, `P value`=x$`P value`)
})
write_csv_safely(primary_cox_rows, file.path(OUT_TABLES,"05_primary_pft_adjusted_hr.csv"))

# Forest plot of four primary PFT effects
forest_dat <- purrr::map2_dfr(pft_map$model_var, seq_len(nrow(pft_map)), function(v,i) {
  x <- cox_fits[[v]]$result %>% dplyr::filter(term==v)
  if (nrow(x)==0) return(tibble::tibble())
  tibble::tibble(PFT=paste0(pft_map$label[i]," (",pft_map$unit[i],")"), HR=x$estimate, low=x$conf.low, high=x$conf.high)
})
if (nrow(forest_dat)>0) {
  p_forest <- ggplot2::ggplot(forest_dat, ggplot2::aes(x=HR,y=reorder(PFT,HR))) +
    ggplot2::geom_vline(xintercept=1,linetype=2) + ggplot2::geom_point() +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin=low,xmax=high),height=.2) +
    ggplot2::scale_x_log10() + ggplot2::theme_bw(base_size=11) +
    ggplot2::labs(x="Adjusted hazard ratio (log scale)",y=NULL,title="Adjusted associations of pulmonary function with overall survival")
  ggplot2::ggsave(file.path(OUT_FIGURES,"Figure_3_Adjusted_PFT_HR_Forest.png"),p_forest,width=7,height=4.5,dpi=300)
}

# 6.1) Full candidate sensitivity models ----
# The exam lists a wider covariate set. Because N and event counts are limited,
# these fuller models are treated as sensitivity analyses rather than automatic
# primary models. The script skips a model when events per parameter are <5.
fit_full_candidate_sensitivity <- function(model_var, label, unit) {
  needed <- c("followup_months", "event", model_var, FULL_CANDIDATE_COVARIATES)
  d <- dat %>% dplyr::select(dplyr::all_of(needed)) %>% tidyr::drop_na()
  rhs <- c(model_var, FULL_CANDIDATE_COVARIATES)
  mm <- stats::model.matrix(stats::as.formula(paste("~", paste(rhs, collapse = " + "))), data = d)
  n_parameters <- ncol(mm) - 1
  n_events <- sum(d$event == 1)
  if (n_parameters <= 0 || n_events / n_parameters < 5) {
    return(tibble::tibble(
      PFT = label, Increment = unit, N = nrow(d), Events = n_events, Parameters = n_parameters,
      `Events per parameter` = round(n_events / max(n_parameters, 1), 1),
      `Adjusted HR (95% CI)` = "Not fit", `P value` = "",
      Note = "Skipped because events per parameter were <5."
    ))
  }
  f <- stats::as.formula(paste("Surv(followup_months,event) ~", paste(rhs, collapse = " + ")))
  m <- safe_coxph(f, d)
  x <- tidy_cox_hr(m, paste0(label, " full candidate sensitivity")) %>% dplyr::filter(term == model_var)
  if (nrow(x) == 0) {
    return(tibble::tibble(PFT=label, Increment=unit, N=nrow(d), Events=n_events, Parameters=n_parameters,
      `Events per parameter`=round(n_events/n_parameters,1), `Adjusted HR (95% CI)`="Model failed",
      `P value`="", Note=""))
  }
  tibble::tibble(PFT=label, Increment=unit, N=nrow(d), Events=n_events, Parameters=n_parameters,
    `Events per parameter`=round(n_events/n_parameters,1),
    `Adjusted HR (95% CI)`=x$`HR (95% CI)`, `P value`=x$`P value`,
    Note="Sensitivity model with the full candidate covariate set.")
}
full_candidate_sensitivity <- purrr::pmap_dfr(
  pft_map[, c("model_var", "label", "unit")], fit_full_candidate_sensitivity
)
write_csv_safely(full_candidate_sensitivity,
                 file.path(OUT_TABLES, "05b_full_candidate_cox_sensitivity.csv"))

# 7) Cox PH assumptions and continuous functional form ----
ph_results <- purrr::imap_dfr(cox_fits, function(obj,name) {
  if (is.null(obj$model)) return(tibble::tibble())
  z <- survival::cox.zph(obj$model)
  as.data.frame(z$table) %>% tibble::rownames_to_column("Term") %>% tibble::as_tibble() %>%
    dplyr::transmute(Model=name,Term,Chisq=round(chisq,3),df=round(df,0),`P value`=fmt_p(p))
})
write_csv_safely(ph_results,file.path(OUT_TABLES,"06_cox_ph_assumption_tests.csv"))
# Save one PH plot file per model
purrr::iwalk(cox_fits,function(obj,name){
  if (!is.null(obj$model)) {
    z <- survival::cox.zph(obj$model)
    grDevices::png(file.path(OUT_APPENDIX,paste0("Appendix_PH_",name,".png")),width=2400,height=1800,res=250)
    plot(z); grDevices::dev.off()
  }
})

# Martingale residual smooths provide a rough functional-form check for the PFT term.
purrr::iwalk(cox_fits, function(obj, name) {
  if (!is.null(obj$model)) {
    d <- obj$data
    d$martingale_residual <- residuals(obj$model, type = "martingale")
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[name]], y = martingale_residual)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(method = "loess", se = TRUE) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::labs(x = name, y = "Martingale residual",
                    title = paste("Functional-form check:", name))
    ggplot2::ggsave(file.path(OUT_APPENDIX, paste0("Appendix_Martingale_", name, ".png")),
                    p, width = 6, height = 4, dpi = 300)
  }
})

# 8) Interaction/effect-modification screening ----
fit_interaction <- function(model_var, modifier, pft_label) {
  needed <- unique(c("followup_months","event",model_var,modifier,CORE_COVARIATES))
  d <- dat %>% dplyr::select(dplyr::all_of(needed)) %>% tidyr::drop_na()
  adjustment <- setdiff(CORE_COVARIATES, modifier)
  f0 <- stats::as.formula(paste("Surv(followup_months,event) ~",paste(c(model_var,modifier,adjustment),collapse=" + ")))
  f1 <- stats::as.formula(paste("Surv(followup_months,event) ~",paste(c(paste0(model_var," * ",modifier),adjustment),collapse=" + ")))
  m0 <- safe_coxph(f0,d); m1 <- safe_coxph(f1,d)
  if (is.null(m0)||is.null(m1)) return(tibble::tibble(PFT=pft_label,Modifier=modifier,N=nrow(d),LRT=NA_real_,df=NA_real_,P_value=NA_real_,`P value`="Model failed"))
  a <- stats::anova(m0,m1,test="LRT")
  p <- as.numeric(a$`Pr(>|Chi|)`[2])
  tibble::tibble(PFT=pft_label,Modifier=modifier,N=nrow(d),LRT=round(as.numeric(a$Chisq[2]),3),df=as.numeric(a$Df[2]),P_value=p,`P value`=fmt_p(p))
}
interaction_results <- purrr::map_dfr(seq_len(nrow(pft_map)), function(i) {
  purrr::map_dfr(INTERACTION_MODIFIERS, ~ fit_interaction(pft_map$model_var[i],.x,pft_map$label[i]))
}) %>% dplyr::arrange(P_value)
write_csv_safely(interaction_results,file.path(OUT_TABLES,"07_pft_interaction_screening.csv"))

# 9) PFT and postoperative complications ----
comp_outcomes <- c(anycomp_bin="Any postoperative complication",respcomp_bin="Respiratory complication",
                   cardcomp_bin="Cardiac complication",infectcomp_bin="Infectious complication",deathcomp_bin="Postoperative death complication")
# Unadjusted comparisons: median PFT by outcome + Wilcoxon test
pft_comp_screen <- purrr::map_dfr(names(comp_outcomes), function(outcome) {
  purrr::map_dfr(seq_len(nrow(pft_map)), function(i) {
    raw <- pft_map$raw_var[i]
    d <- dat %>% dplyr::select(dplyr::all_of(c(outcome,raw))) %>% tidyr::drop_na()
    if (length(unique(d[[outcome]]))<2) return(tibble::tibble())
    wt <- stats::wilcox.test(d[[raw]] ~ d[[outcome]], exact=FALSE)
    tibble::tibble(Outcome=comp_outcomes[[outcome]],PFT=pft_map$label[i],N=nrow(d),
      `No complication, median (IQR)`=sprintf("%.1f (%.1f, %.1f)",median(d[[raw]][d[[outcome]]==0]),quantile(d[[raw]][d[[outcome]]==0],.25),quantile(d[[raw]][d[[outcome]]==0],.75)),
      `Complication, median (IQR)`=sprintf("%.1f (%.1f, %.1f)",median(d[[raw]][d[[outcome]]==1]),quantile(d[[raw]][d[[outcome]]==1],.25),quantile(d[[raw]][d[[outcome]]==1],.75)),
      P_value=wt$p.value,`P value`=fmt_p(wt$p.value))
  })
})
write_csv_safely(pft_comp_screen,file.path(OUT_TABLES,"08_pft_complication_unadjusted_screening.csv"))

# Adjusted logistic models for ANY complication, one PFT at a time.
fit_adjusted_logistic <- function(model_var,label,unit) {
  needed <- c("anycomp_bin",model_var,CORE_COVARIATES)
  d <- dat %>% dplyr::select(dplyr::all_of(needed)) %>% tidyr::drop_na()
  f <- stats::as.formula(paste("anycomp_bin ~",paste(c(model_var,CORE_COVARIATES),collapse=" + ")))
  m <- safe_glm(f,d,stats::binomial())
  list(data=d,model=m,result=tidy_or(m,paste0(label," ",unit)))
}
logit_fits <- purrr::pmap(pft_map[,c("model_var","label","unit")],fit_adjusted_logistic)
names(logit_fits)<-pft_map$model_var
logit_primary <- purrr::map2_dfr(pft_map$model_var,seq_len(nrow(pft_map)),function(v,i){
  x<-logit_fits[[v]]$result %>% dplyr::filter(term==v)
  if(nrow(x)==0)return(tibble::tibble())
  tibble::tibble(PFT=pft_map$label[i],Increment=pft_map$unit[i],N=nrow(logit_fits[[v]]$data),`Adjusted OR (95% CI)`=x$`OR (95% CI)`,`P value`=x$`P value`)
})
write_csv_safely(logit_primary,file.path(OUT_TABLES,"09_adjusted_logistic_any_complication.csv"))

logit_diagnostics <- purrr::map2_dfr(logit_fits, names(logit_fits), function(obj, name) {
  if (is.null(obj$model)) {
    return(tibble::tibble(Model=name, N=NA_integer_, Events=NA_integer_, Parameters=NA_integer_,
                          `Events per parameter`=NA_real_, Converged=FALSE))
  }
  n_parameters <- length(stats::coef(obj$model)) - 1
  n_events <- sum(obj$data$anycomp_bin == 1)
  tibble::tibble(Model=name, N=nrow(obj$data), Events=n_events, Parameters=n_parameters,
                 `Events per parameter`=round(n_events/max(n_parameters,1),1),
                 Converged=isTRUE(obj$model$converged))
})
write_csv_safely(logit_diagnostics, file.path(OUT_TABLES,"09b_logistic_model_diagnostics.csv"))


# Optional selected secondary complication models when unadjusted P < 0.10 and >=10 events.
selected_secondary <- pft_comp_screen %>% dplyr::filter(Outcome!="Any postoperative complication",P_value<.10)
secondary_logit_results <- purrr::pmap_dfr(selected_secondary[,c("Outcome","PFT")], function(Outcome,PFT) {
  outcome <- names(comp_outcomes)[match(Outcome,unname(comp_outcomes))]
  i <- match(PFT,pft_map$label); mv<-pft_map$model_var[i]
  needed<-c(outcome,mv,CORE_COVARIATES); d<-dat %>% dplyr::select(dplyr::all_of(needed)) %>% tidyr::drop_na()
  if(sum(d[[outcome]]==1)<10)return(tibble::tibble(Outcome=Outcome,PFT=PFT,Note="Fewer than 10 events; adjusted model not fit."))
  f<-stats::as.formula(paste(outcome,"~",paste(c(mv,CORE_COVARIATES),collapse=" + ")))
  m<-safe_glm(f,d,stats::binomial()); x<-tidy_or(m,paste(Outcome,PFT)) %>% dplyr::filter(term==mv)
  if(nrow(x)==0)return(tibble::tibble(Outcome=Outcome,PFT=PFT,Note="Model failed."))
  tibble::tibble(Outcome=Outcome,PFT=PFT,N=nrow(d),`Adjusted OR (95% CI)`=x$`OR (95% CI)`,`P value`=x$`P value`,Note="")
})
if (ncol(secondary_logit_results) == 0) {
  secondary_logit_results <- tibble::tibble(
    Outcome = "None", PFT = "", N = NA_integer_, `Adjusted OR (95% CI)` = "",
    `P value` = "", Note = "No secondary complication/PFT pair met the prespecified screening criterion."
  )
}
write_csv_safely(secondary_logit_results,file.path(OUT_TABLES,"10_selected_secondary_complication_models.csv"))

# 10) Length of stay ----
# LOS is right-skewed in most clinical datasets. Keep it continuous and model log(LOS)
# unless there is a prespecified clinically meaningful categorization.
fit_los_model <- function(model_var,label,unit) {
  needed<-c("length_of_hospital_stay",model_var,CORE_COVARIATES)
  d<-dat %>% dplyr::select(dplyr::all_of(needed)) %>% tidyr::drop_na() %>% dplyr::filter(length_of_hospital_stay>0)
  f<-stats::as.formula(paste("log(length_of_hospital_stay) ~",paste(c(model_var,CORE_COVARIATES),collapse=" + ")))
  m<-stats::lm(f,d)
  list(data=d,model=m,result=tidy_log_ratio(m,paste0(label," ",unit)))
}
los_fits<-purrr::pmap(pft_map[,c("model_var","label","unit")],fit_los_model); names(los_fits)<-pft_map$model_var
los_primary<-purrr::map2_dfr(pft_map$model_var,seq_len(nrow(pft_map)),function(v,i){
  x<-los_fits[[v]]$result %>% dplyr::filter(term==v)
  if(nrow(x)==0)return(tibble::tibble())
  tibble::tibble(PFT=pft_map$label[i],Increment=pft_map$unit[i],N=nrow(los_fits[[v]]$data),
                 `Adjusted geometric mean ratio (95% CI)`=x$`Geometric mean ratio (95% CI)`,`P value`=x$`P value`)
})
write_csv_safely(los_primary,file.path(OUT_TABLES,"11_adjusted_los_log_linear_models.csv"))
# Gamma log-link sensitivity
los_gamma<-purrr::map2_dfr(pft_map$model_var,seq_len(nrow(pft_map)),function(v,i){
  needed<-c("length_of_hospital_stay",v,CORE_COVARIATES); d<-dat %>% dplyr::select(dplyr::all_of(needed)) %>% tidyr::drop_na() %>% dplyr::filter(length_of_hospital_stay>0)
  f<-stats::as.formula(paste("length_of_hospital_stay ~",paste(c(v,CORE_COVARIATES),collapse=" + ")))
  m<-safe_glm(f,d,stats::Gamma(link="log")); x<-tidy_or(m,paste0(pft_map$label[i]," Gamma sensitivity")) %>% dplyr::filter(term==v)
  if(nrow(x)==0)return(tibble::tibble())
  tibble::tibble(PFT=pft_map$label[i],Increment=pft_map$unit[i],N=nrow(d),`Adjusted mean ratio (95% CI)`=x$`OR (95% CI)`,`P value`=x$`P value`)
})
write_csv_safely(los_gamma,file.path(OUT_TABLES,"12_los_gamma_sensitivity.csv"))

# LOS residual diagnostics for first PFT model (repeat as needed)
first_los <- los_fits[[1]]$model
if (!is.null(first_los)) {
  png(file.path(OUT_APPENDIX,"Appendix_LOS_residual_diagnostics.png"),width=2200,height=1800,res=250)
  par(mfrow=c(2,2)); plot(first_los); dev.off()
}

# 11) Create three-line Word documents ----
ft_baseline_num <- make_three_line(flextable::flextable(baseline_num),8)
ft_baseline_cat <- make_three_line(flextable::flextable(baseline_cat),8)
ft_pft <- make_three_line(flextable::flextable(pft_summary),8)
ft_outcomes <- make_three_line(flextable::flextable(outcome_summary),8)
ft_univ_cox <- make_three_line(flextable::flextable(univ_results %>% dplyr::select(Model,term,`HR (95% CI)`,`P value`)),7)
ft_primary_cox <- make_three_line(flextable::flextable(primary_cox_rows),8)
ft_interactions <- make_three_line(flextable::flextable(interaction_results %>% dplyr::select(PFT,Modifier,N,LRT,df,`P value`)),7)
ft_comp_screen <- make_three_line(flextable::flextable(pft_comp_screen %>% dplyr::select(Outcome,PFT,N,`No complication, median (IQR)`,`Complication, median (IQR)`,`P value`)),7)
ft_logit <- make_three_line(flextable::flextable(logit_primary),8)
ft_los <- make_three_line(flextable::flextable(los_primary),8)

main_doc <- officer::read_docx()
main_doc <- officer::body_add_par(main_doc,PROJECT_TITLE,style="heading 1")
main_doc <- officer::body_add_par(main_doc,paste0("Generated: ",Sys.Date()),style="Normal")
main_doc <- officer::body_add_par(main_doc,"Synthetic practice data only. Interpretations must be written from the actual final output.",style="Normal")
main_doc <- add_table_to_doc(main_doc,"Table 1A. Baseline continuous characteristics",ft_baseline_num)
main_doc <- add_table_to_doc(main_doc,"Table 1B. Baseline categorical characteristics",ft_baseline_cat)
main_doc <- add_table_to_doc(main_doc,"Table 2. Pulmonary function test summary",ft_pft,note="The four PFT measures should not be entered together in the same multivariable model.")
main_doc <- add_table_to_doc(main_doc,"Table 3. Outcome summary",ft_outcomes)
# Add figures explicitly
if(file.exists(file.path(OUT_FIGURES,"Figure_1_PFT_distributions.png"))){main_doc<-officer::body_add_par(main_doc,"Figure 1. Pulmonary function distributions",style="heading 2");main_doc<-officer::body_add_img(main_doc,file.path(OUT_FIGURES,"Figure_1_PFT_distributions.png"),width=6.8,height=4.7)}
if(file.exists(file.path(OUT_FIGURES,"Figure_2_Overall_Kaplan_Meier_with_risk_table.png"))){main_doc<-officer::body_add_par(main_doc,"Figure 2. Overall Kaplan-Meier survival curve",style="heading 2");main_doc<-officer::body_add_img(main_doc,file.path(OUT_FIGURES,"Figure_2_Overall_Kaplan_Meier_with_risk_table.png"),width=6.8,height=5.4)}
if(file.exists(file.path(OUT_FIGURES,"Figure_3_Adjusted_PFT_HR_Forest.png"))){main_doc<-officer::body_add_par(main_doc,"Figure 3. Adjusted associations of PFT with overall survival",style="heading 2");main_doc<-officer::body_add_img(main_doc,file.path(OUT_FIGURES,"Figure_3_Adjusted_PFT_HR_Forest.png"),width=6.7,height=4.3)}
main_doc <- add_table_to_doc(main_doc,"Table 4. Univariate Cox proportional hazards models",ft_univ_cox)
main_doc <- add_table_to_doc(main_doc,"Table 5. Four separate adjusted Cox models for pulmonary function",ft_primary_cox,note="Each row is from a separate adjusted Cox model. HRs are expressed per clinically interpretable PFT increment.")
main_doc <- add_table_to_doc(main_doc,"Table 6. PFT effect-modification screening",ft_interactions,note="Likelihood-ratio tests compare models with and without the stated PFT-by-modifier interaction.")
main_doc <- add_table_to_doc(main_doc,"Table 7. Unadjusted PFT comparisons by postoperative complication",ft_comp_screen)
main_doc <- add_table_to_doc(main_doc,"Table 8. Adjusted logistic models for any postoperative complication",ft_logit,note="Each row is from a separate multivariable logistic model; OR must not be described as a risk ratio.")
main_doc <- add_table_to_doc(main_doc,"Table 9. Adjusted log-linear models for length of stay",ft_los,note="Exponentiated coefficients are ratios of geometric mean length of stay.")
print(main_doc,target=file.path(OUT_WORD,"QE2022_Lung_Cancer_PFT_Main_Tables_ThreeLine.docx"))

# Appendix
ft_flow <- make_three_line(flextable::flextable(flow_counts),9)
ft_missing <- make_three_line(flextable::flextable(missingness_table),8)
ft_corr <- make_three_line(flextable::flextable(pft_correlation),7)
ft_logit_diag <- make_three_line(flextable::flextable(logit_diagnostics),8)
ft_ph <- make_three_line(flextable::flextable(ph_results),7)
ft_secondary <- make_three_line(flextable::flextable(secondary_logit_results),7)
ft_full_candidate <- make_three_line(flextable::flextable(full_candidate_sensitivity),7)
ft_gamma <- make_three_line(flextable::flextable(los_gamma),8)
appendix_doc <- officer::read_docx()
appendix_doc <- officer::body_add_par(appendix_doc,"Appendix: Diagnostics and Sensitivity Analyses",style="heading 1")
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A1. Cohort flow",ft_flow)
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A2. Missingness summary",ft_missing)
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A3. PFT correlation matrix",ft_corr,note="The PFT measures are related but are analyzed in separate regression models.")
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A4. Cox proportional hazards assumption tests",ft_ph)
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A5. Logistic model stability diagnostics",ft_logit_diag)
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A6. Full candidate Cox sensitivity models",ft_full_candidate, note="These models are secondary because the wider candidate set may be unstable in a small cohort.")
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A7. Selected secondary complication models",ft_secondary)
appendix_doc <- add_table_to_doc(appendix_doc,"Appendix Table A8. Gamma log-link LOS sensitivity models",ft_gamma)
for(img in list.files(OUT_APPENDIX,pattern="\\.png$",full.names=TRUE)){
  appendix_doc<-officer::body_add_par(appendix_doc,tools::file_path_sans_ext(basename(img)),style="heading 2")
  appendix_doc<-officer::body_add_img(appendix_doc,img,width=6.6,height=4.8)
}
print(appendix_doc,target=file.path(OUT_WORD,"QE2022_Lung_Cancer_PFT_Appendix_Diagnostics_ThreeLine.docx"))

# 12) Session information ----
writeLines(capture.output(sessionInfo()), file.path(OUT_APPENDIX,"sessionInfo.txt"))
message("Analysis complete. Review output/word, output/tables, output/figures, and output/appendix.")
