# EMPRO

This repository contains all code related to the IRONMAN-EMPRO clinical trial. It includes both the randomization and data analysis files.

## Overview

Study: EMPRO, an IRONMAN sub-study

Primary contacts:

- Lauren Howard
- Terry Hyslop

---

## Repository Structure

### Root Directory

- `README.md`
- `.gitignore` 
- `*.Rproj` 
- `data/`
- `scripts/` 
- `reports/` 
- `randomization/` 

---
### .gitignore
| File/Folder | Description |
|-------------|-------------|
| `data/` | contains input data and output .RData file created by 01_read_empro_data.R|
| `reports/` | contains all output HTML and DOCX reports  |

## Scripts

### scripts/

| File | Description |
|------|-------------|
| `010_read_empro_data.R` | Reads in raw data and creates .RData file to be used in all subsequent files in the analysis |
| `020_empro_quarterly_report.Rmd` | Creates HTML report checking quality and completeness of data, overall and by site |
| `030_empro_edu.Rmd` | Creates HTML report of all the descriptive statistics of the study, including the study cohort and triggers |
| `040_empro_modeling.Rmd` | Creates HTML report of all the outcome analyses |
| `050_empro_report_for_meeting.Rmd | Creates .DOCX report of results to be printed for June 2026 ASCO meeting (not updated) |

Data output:
`data/EMPRO_data_DATE.RData`
- created by `010_read_empro_data.R` and used for all remaining scripts
- includes:
  - `empro_data`
    - Description: Patient_level data; one record per patient
  - `triggers_long`
    - Description: Trigger-level data; one record per trigger, multiple records per patient
  - not_matched, noconsent, lateenroll, screenfail
    - Description: Patient lists used to create the consort diagram  
---

## Reports

### reports/

This folder contains subfolders: 
* `EDA/`
* `Modeling/`
* `Quarterly Reports/`
* `Supplementary/`

---

## Data

### data/

Data inputs(s):

| File | Description |
|------|-------------|
| EMPRO data export from PCCTC (contains PHI - not on GitHub) | Zip file transferred by SFTP |
| `EMPRO-2024-07-trigger-bug-analysis.xlsx` (contains PHI - not on GitHub)| Report from CIRG outlining TruNTH bug|


---

## Randomization

### randomization/

| File | Description |
|------|-------------|
| `01_read_data.R` | read in data available at the time of randomization and prepare for randomization|
| `02_randomization.R` | performs randomization used for the study |
| `021_randomization_test.R` | tests the cvcrand::cptest function |
| `randscheme_final.csv` | output from 020_radnomization; includes the final randomization scheme with site-level characteristics |
| `empro_constrained.csv` | output from 020_radnomization; Constrained randomization space from initial randomization to be used in the permutation test|

---

## Reproducibility Notes

- All file paths should be relative using `here::here()`.
- Raw data should not be modified directly.
- Derived datasets should be generated through scripts.



- Description of major update.

### YYYY-MM-DD

- Description of major update.
