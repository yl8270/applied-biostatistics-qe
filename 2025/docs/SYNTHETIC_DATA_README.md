# Synthetic Kidney Transplant Practice Dataset

This CSV is a simulated practice dataset created only for testing the QE2025 Applied Kidney Transplant R script. It is not the original exam dataset and must not be interpreted as real clinical evidence.

Rows: 469
File: `data/raw/Kidney_Transplant_data.csv`

Variables match the QE2025 Applied prompt:

| Variable | Description | Coding |
|---|---|---|
| OBS | Subject number | 1 to 469 |
| AGE | Age at transplant | years |
| SEX | Biological sex | 1 = Female, 0 = Male |
| DIALY | Duration of hemodialysis prior to transplant | days, with a few -99 missing codes |
| DBT | Diabetes status | 1 = Yes, 0 = No |
| PTX | Number of prior transplants | count |
| BLOOD | Amount of blood transfusion | blood units, with a few -99 missing codes |
| MIS | Donor mismatch score | 0 complete match to 6 complete mismatch |
| ALG | Use of ALG immune suppression drug | 1 = Yes, 0 = No |
| MONTH | Follow-up time from transplant to graft failure or censoring | months |
| FAIL | Graft status | 1 = graft failure, 0 = functioning/censored |

Simulation notes:
- ALG use was simulated to be more common among patients with higher mismatch scores or prior transplant history.
- Graft failure times were simulated from a proportional hazards data-generating mechanism, with age, dialysis duration, diabetes, prior transplant count, blood transfusion, mismatch score, and ALG contributing to hazard.
- ALG was simulated as mildly protective on average.
- Limited missingness and a few `-99` missing codes were intentionally included to test cleaning code.

Quick checks from the generated data:
- Total rows: 469
- ALG users: 187
- No ALG users: 282
- Graft failures: 240
- Censored/functioning grafts: 229
- Median follow-up months: 43.3
