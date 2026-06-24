# QE2024 Applied Practice, NSQIP Bariatric Surgery R Project

This is a practice project for the 2024 PhD Applied Qualifying Exam style task. It includes synthetic NSQIP-like data, a full R analysis script, and a Chinese code/report interpretation guide.

## Important

The dataset in `data/raw/NSQIP_bmi_synthetic.csv` is synthetic practice data. It is not the real exam dataset and should not be interpreted clinically.

## Project structure

```text
QE2024_Applied_NSQIP_Bariatric_R_Project/
├── data/
│   ├── raw/
│   │   └── NSQIP_bmi_synthetic.csv
│   └── dictionary/
│       └── synthetic_data_dictionary.csv
├── scripts/
│   └── QE2024_Applied_NSQIP_Bariatric_full_analysis.R
├── docs/
│   └── QE2024_Applied_R_Code_and_Report_Guide_CN.docx
├── output/
│   ├── tables/
│   ├── figures/
│   ├── appendix/
│   └── word/
└── QE2024_Applied_NSQIP_Bariatric_R_Project.Rproj
```

## How to run

Open the `.Rproj` file in RStudio. Then run:

```r
source("scripts/QE2024_Applied_NSQIP_Bariatric_full_analysis.R")
```

The script will generate:

```text
output/word/QE2024_NSQIP_Bariatric_Main_Tables_ThreeLine.docx
output/word/QE2024_NSQIP_Bariatric_Appendix_Diagnostics_ThreeLine.docx
output/tables/*.csv
output/figures/*.png
output/appendix/*.png
```

## Required R packages

```r
install.packages(c(
  "tidyverse", "janitor", "gtsummary", "flextable", "officer",
  "broom", "MASS", "splines", "scales"
))
```

## Search keys

Use these keywords in RStudio global search:

```text
KEY_TABLE1_PROCEDURE
KEY_TABLE2_OUTCOMES
KEY_TABLE3_INDIVIDUAL_COMPLICATIONS
KEY_BINARY_LOGISTIC_INTERACTION
KEY_COUNT_POISSON_NEGATIVE_BINOMIAL
KEY_OVERDISPERSION
KEY_LENGTH_OF_STAY
KEY_MODEL_DIAGNOSTICS
KEY_THREE_LINE_WORD_OUTPUT
KEY_MISSING_RECODE_NULL_MINUS99
```

## Replace with real data

Change this line near the top of the script:

```r
DATA_PATH <- "data/raw/NSQIP_bmi_synthetic.csv"
```

For the real exam dataset, use something like:

```r
DATA_PATH <- "data/raw/NSQIP_bmi.sas7bdat"
```

The script supports `.csv`, `.xlsx`, `.xls`, and `.sas7bdat`.
