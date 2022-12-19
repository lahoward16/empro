#######################################
# EMPRO study
# Create dataset for quarterly reports
#######################################

library(tidyverse)
library(lubridate)
library(here)
library(PROscorer)
library(dplyr)
library(labelled)
library(stringr)
library(DescTools)

# update date and export folder name
datafolder <- "2022-11-28_EMPRO"
datafile <- "2022-11-28_empro.RData"

# load ironman package
#devtools::install_github("tgerke/ironer", auth_token="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")
# note 10/4/22: ironer package was moved here:
# Repo: https://github.com/pcctc/ironer
# Website: https://pcctc.github.io/ironer/
#devtools::install_github("pcctc/ironer")

library(ironer)

# create empty list for formatted data to go into
formatted = list()

### questions
# hospitalizations?

# new function
`not.na` <- Negate(`is.na`)


###############################
# read in data
###############################

## Load Medidata
load(here::here("data", datafolder, datafile))


###############################
# pull out data elements 
###############################


# pull list of subjects
formatted$subject_data <- medidata$subject %>%
  select(subject, site = site_name_true_nth, site_country, arm) 


# Pull out fields from COHORT table
formatted$cohort_data <- medidata$cohort %>%
  select(subject, cohort, cohort_std)

# Pull out fields from IRONMAN table
# Pull out EMPRO consent date
# calculate enrollment month
formatted$consent_data <- medidata$ic %>%
  select(subject, cnstdate) %>%
  full_join(medidata$sub_icds %>%
              select(subject, empro_cnstdate = sub_icdsstdat)) %>%
  # find which IRONMAN visit patients enrolled in EMPRO
  mutate(enrollment_month = RoundTo(interval(cnstdate, empro_cnstdate) %/% months(1), 3))


# Pull out fields from MHDIAGBX table
formatted$diagnosis_data <- medidata$mhdiagbx %>%
  select(subject, diagbx_date_int)

# pull data from DEMOGRAPHICS table
formatted$demo_data <- medidata$dm %>%
  select(subject, brthdat_int)


# Pull out EMPRO off study info
formatted$empro_off <- medidata$sub_off %>%
  select(subject, sub_offstdat, suboffstudyreason)

# pull death data from PSTATUS
formatted$death_data <- medidata$pstatus %>%
  select(subject, deathdate_int, cod_std) %>%
  mutate(dead=1)

# pull demographics - race
# function to count the NA columns
count_na_func <- function(x) sum(is.na(x))
# pull race from PROM data
# patients can select multiple races
formatted$race_data <- proms %>%
  rename(subject=study_id) %>%
  mutate_at(vars(starts_with("race")), str_to_title) %>%
  mutate(ethnicity_27 = str_to_title(ethnicity_27)) %>%
  select(subject, starts_with("race"), ethnicity_27) %>%
  distinct() %>%
  # variable of all races
  unite(c(race_26_1:race_26_7), col = "race_all", sep="; ", remove=FALSE, na.rm=TRUE) %>%
  mutate(# count number of races selected
    count_na = apply(.[3:10], 1, count_na_func),
    #indicator for black race
    race_black= !is.na(race_26_1),
    # combined race field
    race = case_when(
      !is.na(race_26_other) ~ NA_character_,
      count_na==7 ~ race_all,
      count_na<7 ~ "More than one race",
      TRUE ~ NA_character_)) %>%
  rename(race_other = race_26_other) %>%
  select(subject, race, race_black, race_all, race_other, ethnicity_27)

# pull demographics - education
formatted$educ_data <- proms %>% 
  rename(subject=study_id) %>%
  select(subject, irondemog_16, irondemog_16_other, 
         irondemog_v3_16, irondemog_v3_16_other) %>%
  mutate(education_mc = coalesce(irondemog_16, irondemog_v3_16),
         education_other = coalesce(irondemog_16_other, irondemog_v3_16_other)) %>%
  filter(!is.na(education_mc) | !is.na(education_other)) %>% 
  mutate(education_other = toupper(education_other),
         education = case_when(
           grepl(c("GRADUATE|MS|DOCTORATE|DDS|MA|MASTER|MBA|MD|PHD|M. ED.|PH.D.|MEDICAL"), education_other) ~ 7L,
           grepl("HIGH", education_other) ~ 3L,
           grepl("UNIVERSITY", education_other) ~ 4L,
           !is.na(education_mc) ~ as.integer(education_mc)
         ),
         education = factor(education,
                            levels = 1:7,
                            labels = c("Less than secondary", "Some secondary", 
                                       "Secondary school diploma", "Some university",
                                       "University 2 yr", "University 4 yr", 
                                       "Graduate/professional degree")))

# Triggers data
# Question: how to tell if patient engaged in TrueNTH self management tool
# Question: does clinician_status mean that they contacted the patient?
formatted$triggers <- qre_timeline %>%
  rename(subject=study_id) %>%
  filter(!is.na(EMPRO_questionnaire_completion_date)) %>%
  filter(!(is.na(soft_trigger_domains) & is.na(hard_trigger_domains))) %>%
  mutate(soft_trigger = ifelse(!is.na(soft_trigger_domains), 1, 0),
         hard_trigger = ifelse(!is.na(hard_trigger_domains), 1, 0),
         n_soft_trigger = ifelse(is.na(soft_trigger_domains), NA, str_count(soft_trigger_domains, ",") + 1),
         n_hard_trigger = ifelse(is.na(hard_trigger_domains), NA, str_count(hard_trigger_domains, ",") + 1),
         pain_soft_trigger = ifelse(grepl("pain", soft_trigger_domains), 1, 0),
         pain_hard_trigger = ifelse(grepl("pain", hard_trigger_domains), 1, 0),
         clinician_contact = ifelse(clinician_status=="Completed", 1, 0),
         visit = as.numeric(gsub("Month ", "", visit)),
         hard_trigger_contact = ifelse(clinician_status=="Completed", 1, NA)) %>%
  select(subject, visit, clinician_contact, soft_trigger, hard_trigger, n_soft_trigger, n_hard_trigger, 
         pain_soft_trigger, pain_hard_trigger, hard_trigger_contact) %>%
  # transpose data
  arrange(subject, visit) %>%
  pivot_wider(id_cols = "subject",
              names_from=visit,
              values_from=c(clinician_contact, soft_trigger, hard_trigger, 
                            n_soft_trigger, n_hard_trigger, hard_trigger_contact,
                            pain_soft_trigger, pain_hard_trigger)) %>%
  mutate(n_visit_soft_trigger = select(., starts_with("soft_trigger_")) %>% rowSums(na.rm=TRUE),
         n_visit_hard_trigger = select(., starts_with("hard_trigger_")) %>% rowSums(na.rm=TRUE),
         n_total_soft_trigger = select(., starts_with("n_soft_trigger_")) %>% rowSums(na.rm=TRUE),
         n_total_hard_trigger = select(., starts_with("n_hard_trigger_")) %>% rowSums(na.rm=TRUE),
         n_clinician_contact = select(., starts_with("clinician_contact")) %>% rowSums(na.rm=TRUE),
         pain_hard_trigger = ifelse(select(., starts_with("pain_hard_trigger")) %>% rowSums(na.rm=TRUE) > 0, 1, 0),
         clinician_contact = ifelse(select(., starts_with("clinician_contact")) %>% rowSums(na.rm=TRUE) > 0, 1, 0))

# completed questionnaires
formatted$completed_q <- empro %>%
  rename(subject=study_id) %>%
  filter(status=="completed") %>%
  select(subject, timepoint) %>%
  mutate(complete=1,
         timepoint = tolower(gsub(" ", "_", timepoint))) %>%
  pivot_wider(names_from=timepoint, values_from=complete, names_prefix = "q_")
  
# EORTC COMPOSITE SCORES
# Global health status/QoL: eortc.29, eortc.30
# Physical functioning: eortc.1 - eortc.5
# Emotional functioning: eortc.21 - eortc.24
# Social functioning: eortc.26, eortc.27
# Fatigue scale: eortc.10, eortc.12, eortc.18
# ref: https://rdrr.io/cran/PROscorer/man/qlq_c30.html
formatted$eortc <- proms %>%
  rename(subject=study_id) %>%
  mutate_at(vars(starts_with("eortc")), as.numeric) %>%
  bind_cols(qlq_c30(., 'eortc_')) %>%
  mutate(timepoint = as.numeric(ifelse(timepoint=="Baseline", 0, gsub("Month", "", timepoint)))) %>%
  arrange(subject, timepoint, eortc_1) %>%
  group_by(subject, timepoint) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(formatted$consent_data, by="subject") %>%
  mutate(timepoint = ifelse(!is.na(enrollment_month), timepoint - enrollment_month, timepoint)) %>%
  filter(timepoint %in% seq(0,12,3)) %>%
  select(subject, timepoint, qlqtotal = QLQTOTAL) %>%
  filter(!is.na(qlqtotal)) %>%
  arrange(timepoint) %>%
  pivot_wider(id_cols=subject, names_from=timepoint, values_from=qlqtotal, names_prefix = "eortc_") 
# TO DO: relabel EORTC questionnaires for control group after finding baseline time point

# flag for whether patient completed each EOTRC
formatted$eortc_yn = formatted$eortc %>%
  mutate_at(vars(starts_with("eortc")), not.na) %>%
  rename_with(stringr::str_replace, 
              pattern = "eortc", replacement = "yn_eortc", 
              matches("eortc"))

###############################
# score PROAE CTC questionnaires 
###############################

# There are no standardized scoring rules for how to combine attributes 
# into a single score or how best to analyze PRO-CTCAE data longitudinally.

###############################
# merge all datasets
###############################

all_data = Reduce(function(...) merge(..., by="subject", all=T), formatted) 

###############################
# create new variables
###############################

# QUESTION: one patient was recruited at 51 months. Will he be able to complete one year on EMPRO?
ynlev2 = c(0,1)
ynlab2 = c("No", "Yes")

empro_data <- all_data %>%
  mutate(# age  
    age_baseline = as.numeric(floor((empro_cnstdate - brthdat_int)/365)),
    year_emp_enrollment = factor(year(empro_cnstdate)),
    year_iron_enrollment = factor(year(cnstdate)),
    race_gp = case_when(race=="White / Caucasian" ~ 1,
                        race=="African / African American / Black / Black British / Caribbean" ~ 2,
                        TRUE ~ 3),
    race_gp = factor(race_gp, 
                     levels = 1:3,
                     labels = c("White", "Black", "Other/Unknown")),
    diag_enroll = interval(diagbx_date_int, empro_cnstdate) %/% months(1),
    dead = ifelse(is.na(dead), 0, dead),
    dead = factor(dead, levels=ynlev2, labels=ynlab2),
    pain_hard_trigger = ifelse(is.na(pain_hard_trigger) & arm=="intervention", 0, pain_hard_trigger),
    pain_hard_trigger = factor(pain_hard_trigger, levels=ynlev2, labels=ynlab2),
    n_total_soft_trigger = ifelse(is.na(n_total_soft_trigger) & arm=="intervention", 0, n_total_soft_trigger),
    n_visit_soft_trigger = ifelse(is.na(n_visit_soft_trigger) & arm=="intervention", 0, n_visit_soft_trigger),
    n_total_hard_trigger = ifelse(is.na(n_total_hard_trigger) & arm=="intervention", 0, n_total_hard_trigger),
    n_visit_hard_trigger = ifelse(is.na(n_visit_hard_trigger) & arm=="intervention", 0, n_visit_hard_trigger),
    n_clinician_contact = ifelse(is.na(n_clinician_contact) & n_visit_hard_trigger>0, 0, n_clinician_contact)) 

# label variables
var_label(empro_data) <- list(
  site_country = "Country",
  age_baseline = "Age at study entry",
  race_gp = "Race",
  diag_enroll = "Months from diagnosis to enrollment",
  year_emp_enrollment = "Year of EMPRO enrollment",
  year_iron_enrollment = "Year of IRONMAN enrollment",
  enrollment_month = "Study visit at entry",
  cohort = "Study cohort",
  education = "Education level",
  yn_eortc_0 = "EORTC QLQ-C30 completed at baseline",
  yn_eortc_3 = "EORTC QLQ-C30 completed at 3 mo",
  yn_eortc_6 = "EORTC QLQ-C30 completed at 6 mo",
  yn_eortc_9 = "EORTC QLQ-C30 completed at 9 mo",
  yn_eortc_12 = "EORTC QLQ-C30 completed at 12 mo",
  eortc_0 = "EORTC QLQ-C30 at baseline",
  eortc_3 = "EORTC QLQ-C30 at 3 mo",
  eortc_6 = "EORTC QLQ-C30 at 6 mo",
  eortc_9 = "EORTC QLQ-C30 at 9 mo",
  eortc_12 = "EORTC QLQ-C30 at 12 mo",
  dead = "Died during follow-up",
  n_visit_soft_trigger = "Number of surveys that released a soft trigger",
  n_visit_hard_trigger = "Number of surveys that released a hard trigger",
  n_total_soft_trigger = "Total number of soft triggers",
  n_total_hard_trigger = "Total number of hard triggers",
  n_clinician_contact = "Number of times a patient was contacted by the clinical team after a hard trigger",
  pain_hard_trigger = "Reported pain as a symptom that resulted in a hard trigger",
  clinician_contact = "Contacted by the clinical team after a hard trigger was released",
  q_month_1 = "Completed PRO-CTCAE questionnare at 1 mo",
  q_month_2 = "Completed PRO-CTCAE questionnare at 2 mo",
  q_month_3 = "Completed PRO-CTCAE questionnare at 3 mo",
  q_month_4 = "Completed PRO-CTCAE questionnare at 4 mo",
  q_month_5 = "Completed PRO-CTCAE questionnare at 5 mo",
  q_month_6 = "Completed PRO-CTCAE questionnare at 6 mo",
  q_month_7 = "Completed PRO-CTCAE questionnare at 7 mo")

##Save the result for analysis 
today_rec <- format(Sys.Date(), format="%Y%m%d")
savfile<-here::here("data", paste0("EMPRO_data_", today_rec, ".RData"))
save(empro_data,file= savfile)
tools::md5sum(savfile)

    

