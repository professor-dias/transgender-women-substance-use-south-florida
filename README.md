# Mental health and drug use — publication analysis

Created 24 October 2026. Run install_packages.R once if needed. In RStudio, set this folder as your working directory and run run_all.R. Input is ../raw.xlsx; change raw_file in run_all.R if your private input is elsewhere. Times New Roman must be installed.

## Final scope
Five exposures: psychological distress (K10 >=20), adapted PTSD screen, lifetime suicidal ideation, lifetime suicide attempt, and any reported mental health diagnosis (depression, PTSD or anxiety).
Primary: basedrugsyn, self-reported drug use (no explicit recall period or illicit qualifier).
Secondary: othdgs6, non-injection drug use in the past six months.
Ten separate unadjusted Firth logistic regressions, one exposure per model. There is no simultaneous five-predictor model or demographic adjustment.

## Outputs
outputs/table1.html: Table 1, overall and stratified by the primary outcome; demographic variables retained descriptively. Table tests are unadjusted Fisher exact/Wilcoxon tests. Non-injection drug use is shown overall only (no group comparison). The two race categories with zero participants are omitted from display; no participants are removed.
outputs/Figure_1.png and Figure_2.png: 600-dpi Times New Roman figures with ORs, pointwise 95% confidence intervals, raw p and BH-adjusted p. PDF copies also supplied.
outputs/individual_exposure_results.csv: all ten model results, available sample sizes and 2x2 counts.
outputs/PTSD_override_sensitivity.csv: four PTSD override cases set to missing; Holm correction across two outcomes.
outputs/verification.txt and sample_counts.csv: reproducible checks and aggregate counts.

## Multiplicity and interpretation
BH correction is applied across five exposures separately within each outcome. The additional p_BH_all10 column applies BH across all ten tests. Estimates and raw p-values are unchanged from matching earlier models; adjusted p-values differ because the testing family changed. These are exploratory follow-ups after earlier broader analyses, not prospectively prespecified confirmatory tests.
Any diagnosis/primary drug use: OR 4.17 (95% CI 1.52–11.93), raw p=.00535, within-outcome BH p=.02676; all-ten BH p=.05353.
No secondary association survives within-outcome BH correction. PTSD sensitivity for secondary outcome: OR 3.41 (1.17–10.17), raw p=.02534, two-test Holm p=.05068.

## Diagnosis coding
The code follows the supplied data dictionary: `baseevmh___0` = never diagnosed, `___1` = depression, `___2` = PTSD, and `___3` = anxiety. Any diagnosis selected is Yes; never alone is No. Conflicting selections, don't know/refused, and no selection are missing. This gives 52 Yes, 27 No, and 22 missing, including four never-plus-diagnosis conflicts.

Confirmation needed: verify these labels against the deployed questionnaire because translated annotations differ; response patterns cannot establish which labels participants saw.

PTSD retains the four investigator-directed overrides; a positive screen is not a clinical diagnosis. K10 requires all items. Existing demographic reconciliation and missingness rules retained.

## Privacy
This folder contains code and aggregate outputs only. Raw data and participant-level exports are not included. The scripts keep participant data in memory. No files have been uploaded to GitHub.

## Reading the code
Start with `run_all.R`: it runs the three scripts in order.
1. `01_prepare_and_table1.R` reads the data, prepares variables, and lists each Table 1 row explicitly.
2. `02_regressions_and_figures.R` runs the ten separate models and saves the figures.
3. `03_verify.R` checks model counts, odds ratios, corrections, and the Table 1 display.

In the preparation script, `NA` means missing. The small table helper functions reuse the same counting rules for every row: percentages exclude missing responses, and missing counts are shown separately.
