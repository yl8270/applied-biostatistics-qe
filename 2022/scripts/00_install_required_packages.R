# 00) Install required packages ----
packages <- c("tidyverse","janitor","survival","survminer","broom","flextable","officer","readxl","haven","MASS","splines","scales")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)>0) install.packages(missing)
message("Package check complete.")
