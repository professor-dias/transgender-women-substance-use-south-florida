# DATA PREPARATION ONLY. No regressions.
# Put raw.xlsx in your RStudio working folder, then run this whole script.
# getwd() shows that folder. The CSV is saved there too.
# Run once if needed: install.packages(c("readxl", "dplyr", "readr", "writexl"))
library(readxl)
library(dplyr)
library(readr)

# 1. Read responses as text to preserve PID and free-text income.
raw <- read_excel("raw.xlsx", col_types = "text", na = "")
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

# Original exposure cleaning: remove unknown/refused; retain Other (888).
clean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[x %in% c(555, 777)] <- NA_real_
  x
}

# Preserve the original any-exposure rule: any positive = 1;
# all missing = NA; otherwise 0, even if some items are missing.
any_exposure <- function(items, exact_yes = FALSE) {
  items <- as.data.frame(lapply(items, clean))
  positive <- if (exact_yes) items == 1 else items >= 1
  result <- as.numeric(rowSums(positive, na.rm = TRUE) > 0)
  result[rowSums(!is.na(items)) == 0] <- NA_real_
  result
}

# 3. Calculate screening DAST-10 and AUDIT-10 FROM RAW ITEMS.
# Do not use scrn_dast_score or scrn_audit_score.
dast_items <- c(
  "scrndrugs", "scrnpolydrug", "scrnstopdrug", "scrnfgtdrug", "scrndrgglt",
  "scrndrgsps", "scrndrgfam", "scrndrgcrim", "scrndrugwdl", "scrndrgheal"
)
audit_items <- c(
  "scrnyraud", "scrndayaud", "scrndrinks6", "scrnyrnostop", "scrnyrmiss",
  "scrnyrmorn", "scrnyrgult", "scrnyrfgt", "scrnyrinj", "scrndocwrd"
)

screening <- screening |>
  mutate(
    across(all_of(dast_items), ~ valid_code(.x, 0:1)),
    across(all_of(audit_items[1:8]), ~ valid_code(.x, 0:4)),
    across(all_of(audit_items[9:10]), ~ valid_code(.x, c(0, 2, 4)))
  )
# scrnstopdrug is ALREADY reverse-coded: No = 1, Yes = 0.
# na.rm = FALSE makes the total NA if ANY item is missing/invalid.
screening_scores <- tibble(
  pid = screening$pid,
  DAST_composite = rowSums(screening[dast_items], na.rm = FALSE),
  AUDIT_composite = rowSums(screening[audit_items], na.rm = FALSE)
)

# AUDIT: "Never" allows skipping questions 2–8.
# Questions 9 and 10 must still be answered.
# Do not replace contradictory responses.

audit_skip <- which(
  screening$scrnyraud == 0 &
    is.na(screening_scores$AUDIT_composite) &
    rowSums(screening[audit_items[2:8]] > 0, na.rm = TRUE) == 0 &
    !is.na(screening$scrnyrinj) &
    !is.na(screening$scrndocwrd)
)

screening_scores$AUDIT_composite[audit_skip] <-
  screening$scrnyrinj[audit_skip] +
  screening$scrndocwrd[audit_skip]

cat("AUDIT scores recovered:", length(audit_skip), "\n")

# DAST: investigator-approved assumption: recover negative-gate skips.
# The export alone does not establish whether missing items were skipped.
# Preserve complete scores and do not overwrite any positive item response.

dast_skip <- which(
  screening$scrndrugs == 0 &
    is.na(screening_scores$DAST_composite) &
    rowSums(screening[dast_items] > 0, na.rm = TRUE) == 0
)

screening_scores$DAST_composite[dast_skip] <- 0

cat("DAST scores recovered:", length(dast_skip), "\n")

data <- left_join(baseline, screening_scores, by = "pid")
stopifnot(nrow(data) == nrow(baseline))

# 4. Calculate K10 on the conventional 10-50 scale.
k10_items <- c(
  "basetired30", "basenerv30", "basecalm30", "basehelp30", "basefidg30",
  "basestill30", "basedepp30", "baseeff30", "basesad30", "baseworth30"
)
k10_responses <- data |>
  select(all_of(k10_items)) |>
  mutate(across(everything(), ~ valid_code(.x, 0:4)))
data$K10_composite <- rowSums(k10_responses, na.rm = FALSE) + 10

# 5. Adapted PTSD screen: trauma is a gate, not a symptom point.
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

# 6. Income: classify exact monthly amounts or ranges, without midpoints.
# Exact amounts below 3818 = 1; amounts at/above 3818 = 0.
# A range crossing 3818, ambiguous text, or missing income = NA.
# baseinc is a free-text dollar amount: do not assume a numeric dollar
# amount such as 555 is a sentinel unless the source explicitly says so.
below_living_wage <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[$,]", "", x)
  x <- gsub("[\u2013\u2014]", "-", x)
  x <- gsub("\\s+to\\s+", "-", x)
  x <- gsub("\\s+", "", x)
  x[x %in% c("none", "no", "noincome", "zeroincome")] <- "0"
  result <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    if (grepl("^[0-9]+(\\.[0-9]+)?$", x[i])) {
      result[i] <- as.numeric(as.numeric(x[i]) < 3818)
    } else if (grepl("^[0-9]+(\\.[0-9]+)?-[0-9]+(\\.[0-9]+)?$", x[i])) {
      bounds <- as.numeric(strsplit(x[i], "-", fixed = TRUE)[[1]])
      if (bounds[1] > bounds[2]) next
      if (bounds[2] < 3818) result[i] <- 1
      if (bounds[1] >= 3818) result[i] <- 0
    }
  }
  result
}

# 7. Institutional discrimination: use the five actual experience items.
# feeltreat measures coping; it must not override these responses.
# Any Yes = 1; all five No = 0; otherwise NA (including skipped items).
institutional <- data |>
  select(discrimempedu, discrimserve, discrimloan, discrimpublic, discrimlaw) |>
  mutate(across(everything(), ~ valid_code(.x, 0:1)))
institutional_positive <- case_when(
  rowSums(institutional == 1, na.rm = TRUE) > 0 ~ 1,
  rowSums(!is.na(institutional)) == 5 ~ 0,
  TRUE ~ NA_real_
)

# 8. Create the 15 binary exposure variables. No sex-partner variable.
exposures <- tibble(
  psychological_distress = as.integer(data$K10_composite >= 20),
  PTSD_positive = ifelse(ptsd_override, 1L, as.integer(data$PTSD_composite >= 4)),
  lifetime_suicidal_ideation = valid_code(data$baseevthsde, 0:1),
  lifetime_suicide_attempt = valid_code(data$baseevdosde, 0:1),
  intimate_partner_violence = any_exposure(data[c("ipvpart", "ipvsex", "parthide")]),
  hate_crime = valid_code(data$basehatcrim, 0:1),
  identity_abuse_harassment = any_exposure(data[c("discrimverb", "discrimphys")]),
  institutional_discrimination = institutional_positive,
  housing_discrimination = valid_code(data$discrimtx, 0:1),
  healthcare_microaggressions = any_exposure(data[c(
    "mcroaddr", "mcroinsens", "mcrodeny", "mcroverident", "mcrostereo", "mcromin"
  )]),
  housing_instability = ifelse(
    is.na(clean(data$baselivnow)), NA_real_,
    as.numeric(clean(data$baselivnow) %in% c(2:7, 888))
  ),
  uninsured = as.integer(clean(data$baseins) == 0),
  no_legal_gender_marker_change = as.integer(clean(data$gmarchg) == 0),
  `Below Living Wage` = below_living_wage(data$baseinc),
  healthcare_access_barrier = any_exposure(
    data[c("baseaccdis", "basemhacc")], exact_yes = TRUE
  )
)
# discrimtx is asked only when treatex=0 (gender-related discrimination).
# A skipped item does not establish absence: retain NA rather than invent No.

# 9. Prepare outcomes.
outcomes <- tibble(
  primary_drug_use = valid_code(data$basedrugsyn, 0:1),
  secondary1_noninjection_drug_use = valid_code(data$othdgs6, 0:1),
  AUDIT_positive = as.integer(data$AUDIT_composite > 7),
  DAST_positive = as.integer(data$DAST_composite > 2)
)
positive_components <- rowSums(outcomes == 1, na.rm = TRUE)
answered_components <- rowSums(!is.na(outcomes))
outcomes$secondary2_combined_substance_use <- case_when(
  positive_components > 0 ~ 1,
  answered_components == 4 ~ 0,
  TRUE ~ NA_real_
)

# 10. Assemble the participant-level analysis file.
# Retain each participant even when a score is NA.
substance_use_analysis <- bind_cols(
  tibble(pid = data$pid),
  outcomes,
  tibble(
    DAST_composite = data$DAST_composite,
    AUDIT_composite = data$AUDIT_composite,
    K10_composite = data$K10_composite,
    PTSD_composite = data$PTSD_composite
  ),
  exposures,
  tibble(
    exposure_missing_count = rowSums(is.na(exposures))
  )
)


# 12. Race and ethnicity: screening and baseline reconciled separately.
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

# Additional manuscript demographics; do not alter the 15 exposure definitions.
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
# continuous income summary. Ranges still inform the binary threshold.
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

# 13. Save the updated analysis data BEFORE making Table 1.
library(writexl)
notes <- tibble(Notes = c(
  "Source: raw.xlsx; baseline participants matched to screening by PID.",
  "Race and ethnicity are separate. Codebook labels are retained, with expanded race abbreviations.",
  "Multiracial: explicit code 5 or multiple substantive race choices in one visit (analysis convention).",
  "Other (888) is retained. Unknown/refused (555/777) are not substantive categories.",
  "Race checkbox blanks are not assumed unchecked. Sentinel selections invalidate that visit's race classification.",
  "Agreement: retain value. One usable visit: use it. Different usable responses: final NA, flag conflict.",
  "Race and ethnicity are reconciled independently. Conflicts are not assigned a preferred visit.",
  "The reconciliation sheet preserves visit classifications, raw choices and Other write-ins.",
  "Table 1 percentages use participants with a usable response; missing counts are reported separately.",
  "Missing race/ethnicity includes unresolved conflicts. No participants are removed from the file.",
  "Binary rows report Yes. Continuous rows report median [25th, 75th percentile]. No hypothesis tests.",
  "PTSD-positive includes the four investigator-requested overrides; symptom totals are not inflated.",
  "AUDIT/DAST use screening items; other exposures use baseline responses. Timeframes differ.",
  "Table 1 covers the prepared dataset; age has not been derived or added in this update."
))
write_csv(substance_use_analysis, "substance_use_analysis.csv", na = "NA")
write_xlsx(list(Analysis = substance_use_analysis,
               Reconciliation = race_ethnicity_review, Notes = notes),
           "substance_use_analysis.xlsx")

# Manuscript Table 1: overall and stratified by PRIMARY drug-use response.
# Missing primary outcome stays in Overall and is excluded from group tests.
y<-substance_use_analysis$primary_drug_use
masks<-list(Overall=rep(TRUE,length(y)),Drug_Yes=!is.na(y)&y==1,Drug_No=!is.na(y)&y==0)
pretty_p<-function(p)if(is.na(p))'—' else if(p<.001)'<0.001' else sprintf('%.3f',p)
cat_test<-function(x){
 keep<-!is.na(x)&!is.na(y);tab<-table(x[keep],y[keep])
 tab<-tab[rowSums(tab)>0,colSums(tab)>0,drop=FALSE]
 if(nrow(tab)<2||ncol(tab)<2)return(NA_real_)
 fisher.test(tab,workspace=2e7)$p.value
}
continuous_test<-function(x){
 z0<-x[!is.na(y)&y==0&!is.na(x)];z1<-x[!is.na(y)&y==1&!is.na(x)]
 if(length(z0)==0||length(z1)==0)return(NA_real_)
 wilcox.test(z0,z1,exact=FALSE,correct=TRUE)$p.value
}
# Each cell states n/available N (%), so missingness cannot be hidden.
cat_summary<-function(x,level,mask){
 v<-x[mask];den<-sum(!is.na(v));n<-sum(v==level,na.rm=TRUE)
 if(den==0)'Not available' else sprintf('%d/%d (%.1f%%)',n,den,100*n/den)
}
cont_summary<-function(x,mask){
 v<-x[mask];v<-v[!is.na(v)]
 if(length(v)==0)return('Not available')
 q<-quantile(v,c(.5,.25,.75),names=FALSE)
 sprintf('%.1f [%.1f–%.1f]; n=%d',q[1],q[2],q[3],length(v))
}
make_binary<-function(v,label,section){
 x<-substance_use_analysis[[v]]
 tibble(Section=section,Variable=label,
 Overall=cat_summary(x,1,masks$Overall),Drug_Yes=cat_summary(x,1,masks$Drug_Yes),Drug_No=cat_summary(x,1,masks$Drug_No),
 p=pretty_p(cat_test(x)),Missing_Overall=sum(is.na(x)))
}
make_cont<-function(v,label,section){
 x<-substance_use_analysis[[v]]
 tibble(Section=section,Variable=label,
 Overall=cont_summary(x,masks$Overall),Drug_Yes=cont_summary(x,masks$Drug_Yes),Drug_No=cont_summary(x,masks$Drug_No),
 p=pretty_p(continuous_test(x)),Missing_Overall=sum(is.na(x)))
}
make_category<-function(v,label,levels){
 x<-substance_use_analysis[[v]]
 header<-tibble(Section='Demographics',Variable=label,
 Overall=paste0('n=',sum(!is.na(x))),Drug_Yes=paste0('n=',sum(!is.na(x[masks$Drug_Yes]))),Drug_No=paste0('n=',sum(!is.na(x[masks$Drug_No]))),
 p=pretty_p(cat_test(x)),Missing_Overall=sum(is.na(x)))
 children<-bind_rows(lapply(levels,function(level)tibble(Section='Demographics',Variable=paste0('   ',level),
 Overall=cat_summary(x,level,masks$Overall),Drug_Yes=cat_summary(x,level,masks$Drug_Yes),Drug_No=cat_summary(x,level,masks$Drug_No),p='',Missing_Overall=NA_integer_)))
 bind_rows(header,children)
}
# Follow the manuscript's organization but retain CURRENT agreed definitions.
expo_order<-c('identity_abuse_harassment','lifetime_suicidal_ideation','lifetime_suicide_attempt','no_legal_gender_marker_change','PTSD_positive','Below Living Wage','psychological_distress','healthcare_microaggressions','intimate_partner_violence','hate_crime','institutional_discrimination','housing_discrimination','healthcare_access_barrier','uninsured','housing_instability')
expo_labels<-c('Identity-related abuse/harassment','Lifetime suicidal ideation','Lifetime suicide attempt','No legal gender marker change','PTSD-positive (including overrides)','Below living wage (<$3,818/month)','Psychological distress (K10 >=20)','Healthcare microaggressions','Intimate partner violence','Hate crime victimization','Institutional discrimination','Housing discrimination','Healthcare access barrier','Uninsured','Housing instability (including Other)')
table1<-bind_rows(
 tibble(Section='Sample',Variable='Participants',Overall=as.character(nrow(substance_use_analysis)),Drug_Yes=as.character(sum(masks$Drug_Yes)),Drug_No=as.character(sum(masks$Drug_No)),p='',Missing_Overall=sum(is.na(y))),
 make_cont('age_years','Age, years, median [Q1–Q3]','Demographics'),
 make_category('race','Race (mutually exclusive)',race_labels[c(1:5,7,6)]),
 make_category('ethnicity','Ethnicity',ethnicity_labels),
 make_category('education','Education',c('Less than high school','High school / GED','Associate / technical degree',"Bachelor's or higher",'Other')),
 make_cont('monthly_income','Monthly income, $, median [Q1–Q3]','Demographics'),
 make_binary('formal_income_source','Formal employment (W-2) as reported income source','Demographics'),
 bind_rows(lapply(seq_along(expo_order),function(i)make_binary(expo_order[i],expo_labels[i],'Psychosocial and structural factors'))),
 make_binary('secondary1_noninjection_drug_use','Non-injection drug use','Substance-use outcomes'),
 make_binary('AUDIT_positive','AUDIT-10 >=8','Substance-use outcomes'),
 make_cont('AUDIT_composite','AUDIT-10 score, median [Q1–Q3]','Substance-use outcomes'),
 make_binary('DAST_positive','DAST-10 >=3','Substance-use outcomes'),
 make_cont('DAST_composite','DAST-10 score, median [Q1–Q3]','Substance-use outcomes'),
 make_binary('secondary2_combined_substance_use','Combined substance use','Substance-use outcomes')
)
notes<-bind_rows(notes,tibble(Notes=c(
 'Table 1 follows the manuscript layout, with overall and primary-drug-use groups; it does not restore superseded exposure definitions.',
 'Overall includes 101 participants. Drug Yes includes 62 and Drug No 24; 15 with missing primary outcome are excluded from comparisons.',
 'Cells report n/available N (%) or median [Q1–Q3]; continuous cells include their available n. Missing_Overall is not a test statistic.',
 'p-values are unadjusted descriptive comparisons: two-sided Fisher exact tests for categorical variables; Wilcoxon rank-sum with continuity correction and normal approximation for continuous variables. One overall test per multicategory variable, not per level.',
 'Secondary drug and combined outcomes overlap the primary definition; their group-comparison p-values do not provide independent validation.',
 'Age uses actual baseline interview date_entry and valid baseline DOB; valid screening DOB is a fallback. Impossible ages (<18 or >100) remain NA. Excel-corrupted birth dates are not guessed.',
 'Monthly income summaries use explicit amounts only, including explicit no income as zero; ranges and approximate/ambiguous values are excluded. Ranges can still establish the below-living-wage indicator.',
 'Education codes 5/6 are associate/technical degree, not some college. Code 888 is Other. Formal employment reflects baseincmth=0, not all possible concurrent employment.',
 'The manuscript contains superseded methods, counts and effect estimates; those prose passages need revision before submission.'
)))
# Replace outdated descriptive notes from the previous unstratified table.
notes<-notes[!grepl('No hypothesis tests|age has not been derived',notes$Notes),,drop=FALSE]
write_csv(substance_use_analysis,'substance_use_analysis.csv',na='NA')
write_xlsx(list(Analysis=substance_use_analysis,Reconciliation=race_ethnicity_review,Notes=notes),'substance_use_analysis.xlsx')
write_csv(table1,'table1.csv',na='')
write_xlsx(list(Table1=table1,Notes=notes),'table1.xlsx')
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
 body,'</tbody></table><h2>Table notes</h2><ul>',paste0('<li>',escape_html(notes$Notes),'</li>'),'</ul></body></html>'),'table1.html')
cat('Table 1 generated with',sum(!is.na(age_years)),'valid ages and',sum(!is.na(monthly_income)),'exact income amounts.\n')
