# Run through run_all.R after preparing the data in script 01.
# Install once: install.packages(c("readr", "dplyr", "ggplot2", "patchwork", "logistf", "readxl", "ragg", "systemfonts"))
# Outcomes are binary. Each model has ONE predictor and an intercept.
# Firth penalization reduces sparse-data bias; it does not fix a small sample.
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(logistf)

data <- substance_use_analysis
stopifnot(!anyNA(data$pid), !anyDuplicated(data$pid))

outcome_names <- c(
  primary_drug_use = "Primary | Self-reported drug use",
  secondary1_noninjection_drug_use = "Secondary | Non-injection drug use"
)
exposure_names <- c(
  psychological_distress = "Psychological distress (K10 >=20)",
  PTSD_positive = "PTSD screen / investigator override",
  lifetime_suicidal_ideation = "Lifetime suicidal ideation",
  lifetime_suicide_attempt = "Lifetime suicide attempt",
  mental_health_diagnosis = "Any mental health diagnosis"
)
binary_columns <- c(names(outcome_names), names(exposure_names))
stopifnot(all(vapply(data[binary_columns], function(x) all(is.na(x) | x %in% 0:1), logical(1))))

# Available cases separately for EACH outcome-predictor pair.
# Profile penalized-likelihood intervals and likelihood-ratio p-values.
fit_one <- function(dataset, outcome, predictor) {
  sample <- data.frame(y = dataset[[outcome]], x = dataset[[predictor]])
  sample <- sample[complete.cases(sample), ]
  counts <- table(factor(sample$x, levels = 0:1), factor(sample$y, levels = 0:1))
  fit <- logistf(y ~ x, data = sample, pl = TRUE,
    control = logistf.control(maxit = 1000),
    plcontrol = logistpl.control(maxit = 1000))
  stopifnot(length(unique(sample$y)) == 2, length(unique(sample$x)) > 1,
            all(is.finite(c(fit$coefficients[2], fit$ci.lower[2], fit$ci.upper[2], fit$prob[2]))))
  tibble(outcome = outcome, predictor = predictor, n = nrow(sample),
    outcome_yes = sum(sample$y == 1), outcome_no = sum(sample$y == 0),
    log_OR = unname(fit$coefficients[2]), OR = exp(fit$coefficients[2]),
    CI_low = exp(fit$ci.lower[2]), CI_high = exp(fit$ci.upper[2]),
    p = unname(fit$prob[2]),
    zero_cell = any(counts == 0),
    no_exposure_no_outcome = counts[1,1],
    no_exposure_yes_outcome = counts[1,2],
    exposure_no_outcome = counts[2,1],
    exposure_yes_outcome = counts[2,2])
}

individual <- bind_rows(lapply(names(outcome_names), function(y) {
  bind_rows(lapply(names(exposure_names), function(x) fit_one(data, y, x)))
})) |>
  group_by(outcome) |>
  mutate(p_adjusted = p.adjust(p, method = "BH", n = 5)) |>
  ungroup() |>
  mutate(p_BH_all10 = p.adjust(p, method = "BH", n = 10))

# Sensitivity to the investigator's four PTSD overrides: return those
# classifications to NA and refit the two PTSD models.
# Match raw baseline responses by PID; never use row positions for joining.
library(readxl)
raw <- read_excel(raw_file, col_types = "text")
baseline <- filter(raw, redcap_event_name == "baseline_arm_1")
baseline$pid <- trimws(baseline$pid)
stopifnot(!anyDuplicated(baseline$pid), all(data$pid %in% baseline$pid))
baseline <- baseline[match(data$pid, baseline$pid), ]
ptsd_items <- c("baseptsdnight", "baseptsdthought", "baseptsdavd",
                "baseptsdgd", "baseptsdnum", "baseptsdguil")
override <- !is.na(baseline$basetrauma) & baseline$basetrauma == "0" &
  rowSums(baseline[ptsd_items] == "1", na.rm = TRUE) > 0
sensitivity_data <- data
sensitivity_data$PTSD_positive[override] <- NA
sensitivity <- bind_rows(lapply(names(outcome_names), function(y) {
  fit_one(sensitivity_data, y, "PTSD_positive")
})) |>
  group_by(predictor) |>
  mutate(p_Holm_2 = p.adjust(p, "holm")) |>
  ungroup()

classify <- function(d) d |>
  mutate(status = case_when(p_adjusted < .05 ~ "Survives correction",
                           p < .05 ~ "Raw p < .05 only",
                           TRUE ~ "Not significant"))
individual <- classify(individual)
write_csv(individual, file.path(output_folder, "individual_exposure_results.csv"))
write_csv(sensitivity, file.path(output_folder, "PTSD_override_sensitivity.csv"))

# Manuscript-sized figures: Times New Roman throughout, at a 6.5-inch width.
# Exact estimates and per-model counts remain in the companion CSV tables.
library(ragg)
font_family <- 'Times New Roman'
stopifnot(font_family %in% systemfonts::system_fonts()$family)
theme_set(theme_minimal(base_family=font_family,base_size=10))
update_geom_defaults('text',list(family=font_family))
update_geom_defaults('label',list(family=font_family))
colours <- c('Survives correction'='#20B83F','Raw p < .05 only'='#E6BE22','Not significant'='#536878')
pretty_names <- c(
 mental_health_diagnosis='Any mental health diagnosis',
 psychological_distress='Psychological distress',PTSD_positive='PTSD-positive',
 lifetime_suicidal_ideation='Lifetime suicidal ideation',lifetime_suicide_attempt='Lifetime suicide attempt',
 intimate_partner_violence='Intimate partner violence',hate_crime='Hate crime victimization',
 identity_abuse_harassment='Identity-related abuse/harassment',institutional_discrimination='Institutional discrimination',
 housing_discrimination='Housing discrimination',healthcare_microaggressions='Healthcare microaggressions',
 housing_instability='Housing instability',uninsured='Uninsured',no_legal_gender_marker_change='No legal gender marker change',
 'Below Living Wage'='Below living wage',healthcare_access_barrier='Healthcare access barrier')
format_p <- function(x) ifelse(x<.001,'<.001',sprintf('%.3f',x))
format_or <- function(x) ifelse(x<.01,'<0.01',sprintf('%.2f',x))
figure_captions <- character()
forest <- function(d,labels,title,correction,filename) {
 nr<-nrow(d);d$row<-rev(seq_len(nr));d$label<-vapply(labels,function(x)paste(strwrap(x,width=27),collapse='\n'),character(1))
 d$effect<-paste0(format_or(d$OR),' (',format_or(d$CI_low),'–',format_or(d$CI_high),')')
 d$tests<-paste0('p ',format_p(d$p),'   ',correction,' p ',format_p(d$p_adjusted))
 d$symbol<-ifelse(d$p_adjusted<.05,18,ifelse(d$p<.05,17,16))
 ylim<-c(.3,nr+.85)
 common<-theme_void(base_family=font_family,base_size=10)+theme(plot.margin=margin(3,2,3,2),legend.position='none')
 ys<-scale_y_continuous(limits=ylim,expand=c(0,0))
 left<-ggplot(d,aes(y=row))+geom_text(aes(x=0,label=label),hjust=0,lineheight=.95,size=3.35,colour='#172B3A')+
 annotate('text',x=0,y=nr+.65,label='Exposure',hjust=0,size=3.5,fontface='bold')+
 scale_x_continuous(limits=c(0,1))+ys+common
 mid<-ggplot(d,aes(y=row,colour=status))+
 geom_vline(xintercept=1,linetype='dashed',colour='#9AA9B2',linewidth=.35)+
 geom_segment(aes(x=CI_low,xend=CI_high,yend=row),linewidth=.55)+
 geom_point(aes(x=OR,shape=factor(symbol)),size=2.2)+scale_shape_manual(values=c('16'=16,'17'=17,'18'=18))+
 scale_colour_manual(values=colours)+scale_x_log10(breaks=c(.01,.1,1,10,100,1000),labels=scales::label_number())+
 ys+common+labs(x='Odds ratio')+theme(axis.text.x=element_text(size=8,family=font_family),axis.title.x=element_text(size=9,family=font_family,margin=margin(t=5)),panel.grid.major.x=element_line(colour='#EEF1F3',linewidth=.3))
 right<-ggplot(d,aes(y=row))+
 geom_text(aes(x=0,y=row+.13,label=effect),hjust=0,size=3.25,colour='#172B3A')+
 geom_text(aes(x=0,y=row-.18,label=tests),hjust=0,size=3.0,colour='#172B3A')+
 annotate('text',x=0,y=nr+.65,label='OR (95% CI); p-values',hjust=0,size=3.5,fontface='bold')+
 scale_colour_manual(values=colours)+scale_x_continuous(limits=c(0,1))+ys+common
 plot<-(left+mid+right)+plot_layout(widths=c(2.4,1.5,2.6))+
 plot_annotation(title=paste(strwrap(title, width=65), collapse="\n"),
 caption=paste0(
  'OR = odds ratio; CI = confidence interval. Bars: pointwise 95% CIs.\n',
  'p = unadjusted p-value; BH p = Benjamini–Hochberg adjusted p-value (5 tests per outcome).\n',
  'Bright green diamond: adjusted p < .05; yellow triangle: only unadjusted p < .05.\n',
  'Slate circle: unadjusted p >= .05. Separate unadjusted Firth logistic models.'),
 theme=theme(text=element_text(family=font_family),
 plot.caption=element_text(family=font_family,size=8.5,hjust=0,lineheight=1.1,margin=margin(t=10)),plot.title=element_text(size=12,face='bold',family=font_family,colour='#172B3A',margin=margin(b=8)),plot.margin=margin(8,6,6,6),plot.background=element_rect(fill='white',colour=NA)))
 h<-5.0
 ggsave(file.path(output_folder,paste0(filename,'.png')),plot,width=6.5,height=h,dpi=600,device=ragg::agg_png,bg='white')
 # Native macOS PDF embeds Times New Roman without requiring XQuartz.
 if(capabilities('aqua')) {
  quartz(type='pdf',file=file.path(output_folder,paste0(filename,'.pdf')),width=6.5,height=h,family=font_family)
  print(plot);dev.off()
 } else {
  ggsave(file.path(output_folder,paste0(filename,'.pdf')),plot,width=6.5,height=h,device=cairo_pdf,family=font_family)
 }
 invisible(plot)
}
figure_titles <- c(
 primary_drug_use='Figure 1. Mental health and self-reported drug use',
 secondary1_noninjection_drug_use='Figure 2. Mental health and non-injection drug use')
for(y in names(outcome_names)) {
 d<-filter(individual,outcome==y)
 forest(d,pretty_names[d$predictor],unname(figure_titles[y]),'BH',paste0(y,'_forest'))
 # Numbered copies for straightforward manuscript insertion.
 number <- match(y,names(outcome_names))
 for(extension in c('png','pdf')) file.copy(
  file.path(output_folder,paste0(y,'_forest.',extension)),
  file.path(output_folder,paste0('Figure_',number,'.',extension)),overwrite=TRUE)
}
writeLines(c('Ten separate unadjusted Firth models: five exposures and two outcomes.',
 'BH correction across five exposures per outcome; additional all-ten BH column provided.',
 'PTSD sensitivity sets override cases to missing; Holm correction across two tests.',
 'Pointwise 95% profile penalized-likelihood intervals are not multiplicity adjusted.'),file.path(output_folder,'model_notes.txt'))
capture.output(sessionInfo(),file=file.path(output_folder,'sessionInfo.txt'))
print(individual,width=Inf)
