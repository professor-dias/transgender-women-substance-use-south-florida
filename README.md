# Substance use among transgender women in South Florida

R scripts for data preparation, Table 1, and exploratory individual-exposure regressions. Cumulative burden/syndemic analyses are not included.

## Run in RStudio

1. Install R 4.1 or newer and RStudio.
2. Download the repository and open `substance_use.Rproj`.
3. Put your authorized private REDCap export, named `raw.xlsx`, in the project folder. Its first worksheet must contain original coded responses and variable names, including `pid` and `redcap_event_name`.
4. Run `source("install_packages.R")` once.
5. Install Times New Roman if unavailable. Figures require that font; the script stops rather than substituting another font.
6. Run `source("run_all.R")`.

No absolute paths or setwd() edits are needed. Alternatively run `Rscript run_all.R` from the repository folder. Outputs are overwritten when rerun. Non-macOS PDF export requires R with Cairo graphics support.

## Files and outputs

- `create_substance_use_analysis.R`: prepares participant-level CSV/Excel data and Table 1 in CSV, Excel, and HTML. The analysis workbook includes a private race/ethnicity reconciliation sheet.
- `run_substance_use_regressions.R`: produces individual-exposure results, PTSD sensitivity results, session information, and figures in `regression_results/`.
- `Figure_1`, `Figure_2`, and `Figure_3` are generated as PNG/PDF, with descriptive-name copies. Each figure defines its estimates, p-values, and symbols. All fonts are Times New Roman.

This repository contains code only. The raw data, manuscript, codebook, and generated outputs are not distributed. The original export is required to reproduce results. The separate exploratory alternative-outcome script is outside this manuscript workflow.

## Preparation decisions

Baseline participants (`baseline_arm_1`) are matched by unique, nonmissing PID to screening (`screening_arm_1`); follow-up events are excluded. Race and ethnicity are reconciled separately: retain agreement, use one usable visit, and leave conflicting usable responses missing. Other is retained; unknown/refused are missing. Multiple substantive race selections are classified as Multiracial.

DAST-10 and AUDIT-10 are calculated from screening items, not REDCap totals. Ordinarily all items must be valid. The investigator-approved DAST negative-gate skip assumption assigns zero to incomplete scores only when the initial drug-use item is negative and no observed item is positive. The export alone cannot confirm questionnaire administration. AUDIT allows questions 2–8 to be skipped after Never only when observed answers do not contradict the skip and questions 9–10 are answered. K10 requires all ten items, uses the 10–50 scale, and defines distress as at least 20.

The adapted PTSD screen combines nightmares/unwanted thoughts into one domain and uses five symptom domains. Four trauma-gate/symptom conflicts in the study data are classified positive by investigator decision; sensitivity analysis sets them to missing. Other exposure composites retain the rules documented in code, including partial responses where specified. `exposure_missing_count` is a data-quality field, not a burden score.

Monthly income below $3,818 is positive; amounts at/above that threshold are negative. Ranges wholly on one side establish the binary classification; crossing/ambiguous ranges remain missing. Continuous income summaries use exact amounts only. Age uses the baseline interview date and valid baseline DOB, with screening DOB as fallback.

## Statistical methods

The three binary outcomes are:

1. Primary: baseline drug-use response (`basedrugsyn`).
2. Secondary 1: non-injection drug-use response (`othdgs6`).
3. Secondary 2: either drug indicator positive, AUDIT at least 8, or DAST at least 3. Any positive establishes a positive; all four components must be negative to establish a negative.

Each model is Firth logistic regression with an intercept and one exposure, using available cases for that outcome/exposure pair. There is no demographic covariate adjustment. Results report odds ratios, pointwise 95% profile penalized-likelihood confidence intervals, and likelihood-ratio p-values. Benjamini–Hochberg (BH) correction covers 15 tests within each outcome; an additional column corrects all 45 tests together. The three PTSD sensitivity tests use Holm correction. Confidence intervals are not multiplicity-adjusted.

Table 1 reports available-case denominators and missing counts. Its descriptive, unadjusted comparisons use Fisher's exact tests and Wilcoxon rank-sum tests with continuity correction and normal approximation. The table's sample-size text is specific to this study's 101 baseline participants, not a template for unrelated exports.

## Upload to GitHub

Upload only this clean release folder before adding private data or running scripts inside it. `.gitignore` protects ordinary Git operations but cannot prevent manual uploads through GitHub's website. Do not upload raw data, participant-level outputs, reconciliation sheets, or local backups.

No license has been chosen; the repository owner can select one if reuse permissions are intended. Installed package versions are recorded in `regression_results/sessionInfo.txt` after each run; dependencies are not locked.

## Contact

For questions about this code or analysis, contact **Professor Dias** through the contact options on the [@professor-dias GitHub profile](https://github.com/professor-dias).
