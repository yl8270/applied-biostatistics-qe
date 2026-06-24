# QE2022 Applied: Lung Cancer PFT Project

This is a **synthetic practice project** based on the structure of the 2022 Applied Qualifying Exam. It is not the original clinical dataset and must not be interpreted as real clinical evidence.

## Open and run

1. Unzip the project.
2. Open `QE2022_Applied_Lung_Cancer_PFT_Project.Rproj` in RStudio.
3. If needed, run `source("scripts/00_install_required_packages.R")` once.
4. Run:

```r
source("scripts/QE2022_Applied_Lung_Cancer_PFT_full_analysis.R")
```

The data path is already set to:

```r
DATA_PATH <- "data/raw/FEV1_LungCa_synthetic.csv"
```

## Project contents

- `data/raw/`: synthetic lung cancer dataset, N=130
- `data/dictionary/`: synthetic data dictionary
- `scripts/`: complete end-to-end analysis and package installer
- `docs/`: Chinese guide, blank report template, and general reference handbook
- `output/`: tables, figures, appendix diagnostics, and Word three-line tables

## Key analytic decisions

- Overall survival: Kaplan-Meier plus Cox proportional hazards models.
- The four PFT measures are analyzed in **four separate models**, not simultaneously.
- Continuous PFT effects are reported per clinically interpretable increments.
- Postoperative complications: unadjusted PFT comparisons and separate adjusted logistic models.
- Length of stay: distribution check plus log-linear model; Gamma log-link sensitivity analysis.
- Interaction screening: one PFT-by-modifier interaction at a time using likelihood-ratio tests.
- Missing data: complete-case analysis within each model, with model-specific N reported.

## Search keys

Search the R script for:

- `KEY_2022_PFT_SURVIVAL`
- `KEY_FOUR_PFT_SEPARATE_MODELS`
- `KEY_PFT_COMPLICATION_LOGISTIC`
- `KEY_LENGTH_OF_STAY_LOG_LINEAR`
- `KEY_COX_PH_SCHOENFELD`
- `KEY_INTERACTION_EFFECT_MODIFICATION`
- `KEY_THREE_LINE_WORD_OUTPUT`

## Important exam reminder

The script is a learning and navigation tool. During the exam, you must independently verify variable definitions, model assumptions, effect measures, reference groups, and written interpretations.
