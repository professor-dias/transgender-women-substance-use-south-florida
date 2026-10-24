# Data preparation and Table 1. Run via run_all.R.
library(readxl)
library(dplyr)
library(readr)

# 1. Read responses as text to preserve PID and free-text income.
raw <- read_excel(raw_file, col_types = "text", na = "")
baseline <- filter(raw, redcap_event_name == "baseline_arm_1")
screening <- filter(raw, redcap_event_name == "screening_arm_1")

baseline$pid <- trimws(baseline$pid)
screening$pid <- trimws(screening$pid)
stopifnot(
  nrow(baseline) > 0, nrow(screening) > 0,
  !anyNA(baseline$pid), !anyNA(screening$pid),
  all(baseline$pid != ""), all(screening$pid != ""),
  !anyDuplicated(baseline$pid), !anyDuplicated(screening$pid),
  all(baseline$pid %in% screening$pid)
)

# 2. Small helpers used below.
# Only explicitly allowed codes count as valid scale or yes/no responses.
valid_code <- function(x, allowed) {
  x <- suppressWarnings(as.numeric(x))
  x[!(x %in% allowed)] <- NA_real_
  x
}

data <- baseline

# 3. Calculate K10 on the conventional 10-50 scale.
k10_items <- c(
  "basetired30", "basenerv30", "basecalm30", "basehelp30", "basefidg30",
  "basestill30", "basedepp30", "baseeff30", "basesad30", "baseworth30"
)
k10_responses <- data |>
  select(all_of(k10_items)) |>
  mutate(across(everything(), ~ valid_code(.x, 0:4)))
data$K10_composite <- rowSums(k10_responses, na.rm = FALSE) + 10

# 4. Adapted PTSD screen: trauma is a gate, not a symptom point.
ptsd_items <- c(
  "baseptsdnight", "baseptsdthought", "baseptsdavd",
  "baseptsdgd", "baseptsdnum", "baseptsdguil"
)
ptsd <- data |>
  select(all_of(ptsd_items)) |>
  mutate(across(everything(), ~ valid_code(.x, 0:1)))
trauma <- valid_code(data$basetrauma, 0:1)

# Nightmares OR unwanted thoughts represent one symptom domain.
# One yes establishes this domain even if the other response is missing.
intrusion <- case_when(
  ptsd$baseptsdnight == 1 | ptsd$baseptsdthought == 1 ~ 1,
  ptsd$baseptsdnight == 0 & ptsd$baseptsdthought == 0 ~ 0,
  TRUE ~ NA_real_
)
ptsd_domains <- data.frame(
  intrusion, avoidance = ptsd$baseptsdavd, vigilance = ptsd$baseptsdgd,
  numbness = ptsd$baseptsdnum, guilt = ptsd$baseptsdguil
)
ptsd_total <- rowSums(ptsd_domains, na.rm = FALSE)
symptom_reported <- rowSums(ptsd == 1, na.rm = TRUE) > 0
# Investigator-requested classification override, independent of the >=4
# symptom cutoff. This does not establish a clinical diagnosis.
ptsd_override <- !is.na(trauma) & trauma == 0 & symptom_reported
data$PTSD_composite <- case_when(
  ptsd_override ~ ptsd_total,
  trauma == 0 ~ 0,
  trauma == 1 ~ ptsd_total,
  TRUE ~ NA_real_
)

# Mental health exposures only; original definitions retained.
exposures <- tibble(
 psychological_distress = as.integer(data$K10_composite >= 20),
 PTSD_positive = ifelse(ptsd_override, 1L, as.integer(data$PTSD_composite >= 4)),
 lifetime_suicidal_ideation = valid_code(data$baseevthsde, 0:1),
 lifetime_suicide_attempt = valid_code(data$baseevdosde, 0:1)
)

# 5. Prepare outcomes.

outcomes <- tibble(
 primary_drug_use = valid_code(data$basedrugsyn, 0:1),
 secondary1_noninjection_drug_use = valid_code(data$othdgs6, 0:1)
)

# Main REDCap codes supplied by investigator. Contradictions remain missing.
m <- as.data.frame(lapply(data[paste0('baseevmh___',c(0:3,555,777))],valid_code,allowed=0:1))
diagnosis_yes <- rowSums(m[2:4]==1,na.rm=TRUE)>0
diagnosis_conflict <- m[[1]]==1 & diagnosis_yes
diagnosis_sentinel <- rowSums(m[5:6]==1,na.rm=TRUE)>0
exposures$mental_health_diagnosis <- case_when(
 diagnosis_sentinel | diagnosis_conflict ~ NA_real_,
 diagnosis_yes ~ 1,
 rowSums(!is.na(m))==6 & m[[1]]==1 ~ 0,
 TRUE ~ NA_real_)

# 6. Assemble the participant-level analysis file.
# Retain each participant even when a score is NA.
substance_use_analysis <- bind_cols(
  tibble(pid = data$pid),
  outcomes,
  tibble(
    K10_composite = data$K10_composite,
    PTSD_composite = data$PTSD_composite
  ),
  exposures,
  tibble(
    exposure_missing_count = rowSums(is.na(exposures))
  )
)


# 7. Race and ethnicity: screening and baseline reconciled separately.
# Codebook: race is select-all-that-apply; ethnicity is one response.
# Analysis convention previously agreed: multiple substantive races, or
# explicit Multiracial, become Multiracial. Other remains a valid category.
race_labels <- c("White", "Black/AA", "Native Hawaiian/Pacific Islander",
                 "Asian", "American Indian/Alaska Native", "Multiracial", "Other")
ethnicity_labels <- c("Hispanic/Latina/o/x", "Non-Hispanic/non-Latina/o/x", "Other")
race_at_visit <- function(visit, prefix) {
  columns <- paste0(prefix, "race___", c(0:5, 888, 555, 777))
  items <- as.data.frame(lapply(visit[columns], valid_code, allowed = 0:1))
  result <- rep(NA_character_, nrow(visit))
  reason <- rep("Missing/incomplete response", nrow(visit))
  for (i in seq_len(nrow(visit))) {
    row <- unlist(items[i, ], use.names = FALSE)
    selected <- which(row[1:7] == 1)
    sentinel <- any(row[8:9] == 1, na.rm = TRUE)
    if (sentinel) {
      reason[i] <- if (length(selected) > 0) "Race plus unknown/refused selected" else "Unknown/refused"
    } else if (!anyNA(row) && length(selected) > 0) {
      result[i] <- if (6 %in% selected || length(selected) > 1) "Multiracial" else race_labels[selected]
      reason[i] <- "Usable"
    }
  }
  tibble(value = result, response_status = reason)
}
ethnicity_at_visit <- function(x) {
  x <- valid_code(x, c(0, 1, 888))
  ethnicity_labels[match(x, c(0, 1, 888))]
}
reconcile <- function(screen, base) {
  case_when(
    !is.na(screen) & !is.na(base) & screen != base ~ NA_character_,
    !is.na(base) ~ base,
    TRUE ~ screen
  )
}
reconciliation_status <- function(screen, base) {
  case_when(
    is.na(screen) & is.na(base) ~ "Neither visit usable",
    is.na(screen) ~ "Baseline only",
    is.na(base) ~ "Screening only",
    screen == base ~ "Agreement",
    TRUE ~ "Conflict between visits"
  )
}
# Explicit PID matching, independent of spreadsheet row order.
screen_match <- screening[match(data$pid, screening$pid), ]
stopifnot(identical(data$pid, screen_match$pid))
screen_race <- race_at_visit(screen_match, "scrn")
base_race <- race_at_visit(data, "base")
race_ethnicity_review <- tibble(
  pid = data$pid,
  screening_race = screen_race$value,
  baseline_race = base_race$value,
  screening_race_status = screen_race$response_status,
  baseline_race_status = base_race$response_status,
  screening_ethnicity = ethnicity_at_visit(screen_match$scrnethnic),
  baseline_ethnicity = ethnicity_at_visit(data$baseethnic),
  screening_ethnicity_raw = screen_match$scrnethnic,
  baseline_ethnicity_raw = data$baseethnic
) |>
  mutate(
    race = reconcile(screening_race, baseline_race),
    ethnicity = reconcile(screening_ethnicity, baseline_ethnicity),
    race_status = reconciliation_status(screening_race, baseline_race),
    ethnicity_status = reconciliation_status(screening_ethnicity, baseline_ethnicity)
  )
# Preserve raw race selections and Other write-ins for review, without guessing
# a category from free text or combining different visits into Multiracial.
raw_race_columns <- c(paste0("scrnrace___", c(0:5, 888, 555, 777)), "scrnraceoth", "scrnethnicoth")
raw_base_columns <- c(paste0("baserace___", c(0:5, 888, 555, 777)), "baseraceoth", "baseethnicoth")
race_ethnicity_review <- bind_cols(race_ethnicity_review,
  screen_match[raw_race_columns], data[raw_base_columns])
substance_use_analysis <- substance_use_analysis |>
  mutate(race = race_ethnicity_review$race,
         ethnicity = race_ethnicity_review$ethnicity) |>
  relocate(race, ethnicity, .after = pid)

# 8. Prepare demographics for Table 1.
# Excel dates were imported as serial text. Use each baseline interview date.
excel_date <- function(x) {
 z<-suppressWarnings(as.numeric(x))
 as.Date(z,origin='1899-12-30')
}
interview_date<-excel_date(data$date_entry)
base_birth<-excel_date(data$basedob)
screen_birth<-excel_date(screen_match$scrndob)
base_age<-as.numeric(interview_date-base_birth)/365.25
screen_age<-as.numeric(interview_date-screen_birth)/365.25
base_age[!is.finite(base_age)|base_age<18|base_age>100]<-NA_real_
screen_age[!is.finite(screen_age)|screen_age<18|screen_age>100]<-NA_real_
# Baseline DOB preferred; valid screening DOB used only when baseline unusable.
age_years<-ifelse(!is.na(base_age),base_age,screen_age)
education_code<-valid_code(data$baseedu,c(0:8,888))
education<-case_when(
 education_code %in% 0:2 ~ 'Less than high school',
 education_code %in% 3:4 ~ 'High school / GED',
 education_code %in% 5:6 ~ 'Associate / technical degree',
 education_code %in% 7:8 ~ "Bachelor's or higher",
 education_code==888 ~ 'Other',TRUE~NA_character_)
# Preserve the agreed no-midpoint policy: only explicit amounts enter the
# continuous income summary.
income_text<-tolower(trimws(data$baseinc))
income_text<-gsub('[$,[:space:]]','',income_text)
income_text[income_text %in% c('none','no','noincome','zeroincome')]<-'0'
monthly_income<-rep(NA_real_,nrow(data))
exact_income<-!is.na(income_text)&grepl('^[0-9]+(\\.[0-9]+)?$',income_text)
monthly_income[exact_income]<-as.numeric(income_text[exact_income])
source_income<-valid_code(data$baseincmth,c(0:4,888))
formal_income_source<-as.integer(source_income==0)
substance_use_analysis<-substance_use_analysis |>
 mutate(age_years=age_years,education=education,monthly_income=monthly_income,
        formal_income_source=formal_income_source) |>
 relocate(age_years,education,monthly_income,formal_income_source,.after=ethnicity)

# Participant data stay in memory; export only aggregate outputs.
notes <- tibble(Notes=c(
 'Five mental health exposures; two drug-use outcomes; 101 baseline participants.',
 'Primary: basedrugsyn, self-reported drug use without a stated recall period. Secondary: othdgs6, non-injection drug use in the past six months.',
 'PTSD is an adapted screen including four investigator-directed overrides, not a clinical diagnosis.',
 'Any diagnosis: depression, PTSD or anxiety selected. Never alone is No; contradictions, unknown/refused and no selection are missing.',
 'Diagnosis uses investigator-supplied main REDCap codes. Translated annotations conflict; confirm deployed questionnaire mapping before publication.',
 'Race and ethnicity reconciled independently by PID. Other retained; conflicts remain missing.',
 'Cells: n/available N (%) or median [Q1–Q3]. Missing primary outcomes remain in Overall only.',
 'Non-injection drug use is reported overall only, without stratification or a comparison test.',
 'Race categories with zero participants omitted: Native Hawaiian/Pacific Islander and American Indian/Alaska Native.',
 'Table p-values: unadjusted Fisher exact or Wilcoxon tests. Not used to select predictors.',
 'Exploratory follow-up: this narrowed scope was chosen after earlier analyses.'
))

# 9. Build Table 1. Most rows compare Yes and No for PRIMARY drug use.
# The secondary outcome is shown overall only.
# Missing primary outcome stays in Overall and is excluded from group tests.
y <- substance_use_analysis$primary_drug_use
masks <- list(
  Overall = rep(TRUE, length(y)),
  Drug_Yes = !is.na(y) & y == 1,
  Drug_No = !is.na(y) & y == 0
)

# Display small p-values clearly.
pretty_p <- function(p) {
  if (is.na(p)) return("—")
  if (p < .001) return("<0.001")
  sprintf("%.3f", p)
}

# Compare categorical responses between the two primary drug-use groups.
cat_test <- function(x) {
  keep <- !is.na(x) & !is.na(y)
  counts <- table(x[keep], y[keep])
  counts <- counts[rowSums(counts) > 0, colSums(counts) > 0, drop = FALSE]
  if (nrow(counts) < 2 || ncol(counts) < 2) return(NA_real_)
  fisher.test(counts, workspace = 2e7)$p.value
}

# Compare age or income between the two groups.
continuous_test <- function(x) {
  no <- x[!is.na(y) & y == 0 & !is.na(x)]
  yes <- x[!is.na(y) & y == 1 & !is.na(x)]
  if (length(no) == 0 || length(yes) == 0) return(NA_real_)
  wilcox.test(no, yes, exact = FALSE, correct = TRUE)$p.value
}

# Count a category and divide by the number with a known response.
cat_summary <- function(x, level, mask) {
  responses <- x[mask]
  available <- sum(!is.na(responses))
  count <- sum(responses == level, na.rm = TRUE)
  if (available == 0) return("Not available")
  sprintf("%d/%d (%.1f%%)", count, available, 100 * count / available)
}

# Summarize age or income using the median and quartiles.
cont_summary <- function(x, mask) {
  responses <- x[mask]
  responses <- responses[!is.na(responses)]
  if (length(responses) == 0) return("Not available")
  quartiles <- quantile(responses, c(.5, .25, .75), names = FALSE)
  sprintf("%.1f [%.1f–%.1f]; n=%d",
          quartiles[1], quartiles[2], quartiles[3], length(responses))
}

# One row for a Yes/No variable. Count Yes (coded 1).
make_binary <- function(variable, label, section) {
  x <- substance_use_analysis[[variable]]
  tibble(
    Section = section, Variable = label,
    Overall = cat_summary(x, 1, masks$Overall),
    Drug_Yes = cat_summary(x, 1, masks$Drug_Yes),
    Drug_No = cat_summary(x, 1, masks$Drug_No),
    p = pretty_p(cat_test(x)),
    Missing_Overall = sum(is.na(x))
  )
}

# One row for age or income.
make_cont <- function(variable, label, section) {
  x <- substance_use_analysis[[variable]]
  tibble(
    Section = section, Variable = label,
    Overall = cont_summary(x, masks$Overall),
    Drug_Yes = cont_summary(x, masks$Drug_Yes),
    Drug_No = cont_summary(x, masks$Drug_No),
    p = pretty_p(continuous_test(x)),
    Missing_Overall = sum(is.na(x))
  )
}

# A heading plus one row per category (race, ethnicity, or education).
# The heading's p-value compares the complete variable, not individual rows.
make_category <- function(variable, label, levels) {
  x <- substance_use_analysis[[variable]]
  rows <- tibble(
    Section = "Demographics", Variable = label,
    Overall = paste0("n=", sum(!is.na(x))),
    Drug_Yes = paste0("n=", sum(!is.na(x[masks$Drug_Yes]))),
    Drug_No = paste0("n=", sum(!is.na(x[masks$Drug_No]))),
    p = pretty_p(cat_test(x)),
    Missing_Overall = sum(is.na(x))
  )
  for (level in levels) {
    category_row <- tibble(
      Section = "Demographics", Variable = paste0("   ", level),
      Overall = cat_summary(x, level, masks$Overall),
      Drug_Yes = cat_summary(x, level, masks$Drug_Yes),
      Drug_No = cat_summary(x, level, masks$Drug_No),
      p = "", Missing_Overall = NA_integer_
    )
    rows <- bind_rows(rows, category_row)
  }
  rows
}

# The two omitted race categories have no participants in this dataset.
# Only hide their table rows. Keep all participants and the original race coding.
races_to_display <- c("White", "Black/AA", "Asian", "Other", "Multiracial")
omitted_races <- c("Native Hawaiian/Pacific Islander", "American Indian/Alaska Native")
stopifnot(!any(substance_use_analysis$race %in% omitted_races))

# Show the secondary outcome overall, without group comparisons or a p-value.
noninjection <- substance_use_analysis$secondary1_noninjection_drug_use
secondary_row <- tibble(
  Section = "Substance-use outcomes",
  Variable = "Non-injection drug use, past 6 months",
  Overall = cat_summary(noninjection, 1, masks$Overall),
  Drug_Yes = "",
  Drug_No = "",
  p = "",
  Missing_Overall = sum(is.na(noninjection))
)

# List each Table 1 row explicitly, in the order it should appear.
table1 <- bind_rows(
  tibble(
    Section = "Sample", Variable = "Participants",
    Overall = as.character(nrow(substance_use_analysis)),
    Drug_Yes = as.character(sum(masks$Drug_Yes)),
    Drug_No = as.character(sum(masks$Drug_No)),
    p = "", Missing_Overall = sum(is.na(y))
  ),
  make_cont("age_years", "Age, years, median [Q1–Q3]", "Demographics"),
  make_category("race", "Race (mutually exclusive)", races_to_display),
  make_category("ethnicity", "Ethnicity", ethnicity_labels),
  make_category("education", "Education", c(
    "Less than high school", "High school / GED", "Associate / technical degree",
    "Bachelor's or higher", "Other"
  )),
  make_cont("monthly_income", "Monthly income, $, median [Q1–Q3]", "Demographics"),
  make_binary("formal_income_source", "Formal employment (W-2) as reported income source", "Demographics"),
  make_binary("psychological_distress", "Psychological distress (K10 >=20)", "Mental health"),
  make_binary("PTSD_positive", "PTSD-positive (including overrides)", "Mental health"),
  make_binary("lifetime_suicidal_ideation", "Lifetime suicidal ideation", "Mental health"),
  make_binary("lifetime_suicide_attempt", "Lifetime suicide attempt", "Mental health"),
  make_binary("mental_health_diagnosis", "Any mental health diagnosis", "Mental health"),
  secondary_row
)

# 10. Save the table as CSV and a formatted HTML page.
# The HTML below controls appearance only; calculations are finished above.
write_csv(table1,file.path(output_folder,'table1.csv'),na='')
escape_html<-function(x){x<-gsub('&','&amp;',as.character(x),fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
body<-character();last<-''
for(i in seq_len(nrow(table1))){
 row<-table1[i,];if(row$Section!=last){body<-c(body,paste0('<tr class="section"><td colspan="6">',escape_html(row$Section),'</td></tr>'));last<-row$Section}
 values<-as.character(unlist(row[c('Variable','Overall','Drug_Yes','Drug_No','p','Missing_Overall')],use.names=FALSE));values[is.na(values)]<-''
 body<-c(body,paste0('<tr>',paste0('<td>',escape_html(values),'</td>',collapse=''),'</tr>'))
}
writeLines(c('<!doctype html><html><head><meta charset="utf-8"><title>Table 1</title><style>',
 'body{font:12pt "Times New Roman",serif;color:#111;max-width:1100px;margin:35px auto;padding:0 20px}h1{font-size:16pt}table{border-collapse:collapse;width:100%;font-size:11pt}th{border-top:2px solid #111;border-bottom:1px solid #111;padding:8px;text-align:left}td{padding:6px 8px;vertical-align:top;border-bottom:1px solid #eee}td:first-child{white-space:pre-wrap;width:33%}.section td{font-weight:bold;border-top:1px solid #111;background:#f2f2f2}li{margin:6px 0}thead{display:table-header-group}@media print{body{margin:0}tr{break-inside:avoid}}',
 '</style></head><body><h1>Table 1. Participant characteristics by primary drug-use outcome</h1><p>Overall N=101; Drug Yes n=62; Drug No n=24. Primary outcome missing: 15.</p><table><thead><tr><th>Characteristic</th><th>Overall</th><th>Drug Yes</th><th>Drug No</th><th>p</th><th>Missing overall</th></tr></thead><tbody>',
 body,'</tbody></table><h2>Table notes</h2><ul>',paste0('<li>',escape_html(notes$Notes),'</li>'),'</ul></body></html>'),file.path(output_folder,'table1.html'))
cat('Table 1 generated with',sum(!is.na(age_years)),'valid ages and',sum(!is.na(monthly_income)),'exact income amounts.\n')
