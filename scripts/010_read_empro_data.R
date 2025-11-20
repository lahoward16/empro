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
library(rollmatch)
library(purrr)
library(readxl)

# date
date1="2025-11-14"
date2="2025-11-14"

# load ironman package
# devtools::install_github("tgerke/ironer", auth_token="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")
# note 10/4/22: ironer package was moved here:
# Repo: https://github.com/pcctc/ironer
# Website: https://pcctc.github.io/ironer/
# devtools::install_github("pcctc/ironer")

library(ironer)

# create empty list for formatted data to go into
formatted = list()

### questions
# hospitalizations?

# new function
`not.na` <- Negate(`is.na`)
`%notin%` <- Negate(`%in%`)

###############################
# read in data
###############################

## Load Medidata
load(paste0("V:/EMPRO/", date1, "_EMPRO/", date1, "_EMPRO/", date2, "_empro.RData"))

trigreport <- read_xlsx("V:/EMPRO/EMPRO-2024-07-trigger-bug-analysis.xlsx") %>% 
  rename(user_id = "Tnth\nID",
         visit = "Visit\nMonth",
         empro_completed = "EMPRO\nrecorded",
         email_sent = "\"View your\nIRONMAN EMPRO\nreport and \nget support\"\nemail sent",
         soft_trigger = "Soft\nTrigger",
         hard_trigger = "Hard\nTrigger",
         soft_or_hard_trigger = "Either Soft\nor Hard\nTrigger",
         self_mgmt_before_email = "Tailored\ncontent \naccessed\nprior\nto email",
         self_mgmt_after_email = "Tailored\ncontent \naccessed\nafter \nemail\nsent",
         after_bug = "Qnr\npost\nbugfix") %>%
  mutate(soft_trigger = ifelse(!is.na(soft_trigger), 1, 0),
         hard_trigger = ifelse(!is.na(hard_trigger), 1, 0),
         soft_or_hard_trigger = as.numeric(soft_or_hard_trigger),
         self_mgmt_before_email = ifelse(!is.na(self_mgmt_before_email), 1, 0),
         self_mgmt_after_email = ifelse(!is.na(self_mgmt_after_email), 1, 0),
         after_bug = ifelse(!is.na(after_bug), 1, 0)) %>%
  select(user_id, visit, empro_completed, email_sent, soft_trigger, hard_trigger, 
         soft_or_hard_trigger, self_mgmt_before_email, self_mgmt_after_email, after_bug)

###############################
# pull out data elements 
###############################

# pull list of subjects
formatted$subject_data <- medidata$subject %>%
  select(subject, site = site_name_true_nth, country=site_country, arm) %>%
  #groups hospitals that fall in the Queensland system
  mutate(site = ifelse(site %in% c("Princess Alexandra Hospital",
                                   "Redland Hospital", 
                                   "Royal Brisbane & Women’s Hospital"), 
                       "Queensland University of Technology",
                       site))


# Pull out fields from COHORT table
formatted$cohort_data <- medidata$cohort %>%
  select(subject, cohort, cohort_std)

# Pull out fields from IRONMAN table
# Pull out EMPRO consent date
# see if there is a visit date that matches up
# if not, estimate enrollment month
formatted$consent_data <- medidata$ic %>%
  select(subject, cnstdate) %>%
  full_join(medidata$sub_icds %>%
              select(subject, empro_cnstdate = sub_icdsstdat), by="subject") %>%
  full_join(medidata$cyc_date %>%
               select(subject, visit_date, month, instance_repeat_number), by="subject", multiple="all") %>%
  mutate(month = ifelse(subject=="170-82-099" & instance_repeat_number==10, "33", month)) %>%
  filter(!is.na(empro_cnstdate)) %>%
  mutate(diff = abs((visit_date %--% empro_cnstdate) / days(1))) %>%
  #filter(!is.na(diff)) %>%
  arrange(subject, diff) %>%
  group_by(subject) %>%
  slice(1) %>%
  ungroup() %>%
  # estimate IRONMAN visit date patients enrolled in EMPRO
  mutate(est_enrollment_month = RoundTo(interval(cnstdate, empro_cnstdate) %/% months(1), 3),
         enrollment_month = as.numeric(ifelse(diff<=30 & !is.na(diff), month, est_enrollment_month))) %>%
  select(-visit_date, -month, -est_enrollment_month, -diff)

  
# Pull out fields from MHDIAGBX table
formatted$diagnosis_data <- medidata$mhdiagbx %>%
  select(subject, diagbx_date_int) 

# pull data from DEMOGRAPHICS table
formatted$demo_data <- medidata$dm %>%
  select(subject, brthdat)


# Pull out EMPRO off study info
formatted$empro_off <- medidata$sub_off %>%
  select(subject, sub_offstdat, suboffstudyreason)

# pull death data from PSTATUS
formatted$death_data <- medidata$pstatus %>%
  select(subject, deathdate_int, cod_std) %>%
  mutate(dead=1)

## Censor date
# last cycle date 
formatted$censor_date <- medidata$cyc_date %>% 
  select(subject, visit_date) %>% 
  arrange(desc(visit_date)) %>% 
  group_by(subject) %>% 
  slice(1) %>% 
  rename(censor_date = visit_date)


# pull CRPC/HSPC diagnosis dates
# find diagnosis date closest to but before EMPRO consent date
formatted$dis_state <- medidata$mhdx %>% 
  full_join(formatted$consent_data %>% select(subject, empro_cnstdate), by="subject") %>% 
  full_join(formatted$cohort_data, by="subject") %>% 
  mutate(cohort_empro = case_when(
    mcrpcdat <= empro_cnstdate ~ "CRPC",
    crpcdat <= empro_cnstdate ~ "CRPC",
    mhspcdat <= empro_cnstdate ~ "mHSPC", 
    TRUE ~ cohort
  ),
  cohort_std_empro = ifelse(cohort_empro=="CRPC", 2, 1)) %>% 
  select(subject, mhspcdat, crpcdat, mcrpcdat, cohort_empro, cohort_std_empro)
    # 8/6/25 - 11 patients were enrolled as IRONMAN as mHSPC but progressed 
     # to CRPC by the time of EMRPO enrollment

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
           grepl(c("GRADUATE|MS|DOCTORATE|DDS|MA|MASTER|MBA|MD|PHD|M. ED.|PH.D.|MEDICAL|M.D.|LAW|M.A.|GRAD|PH. D|DOCTOR|MED.|J.D.|DVM"), education_other) ~ 7L,
           grepl("HIGH|TRADE|TECH|APPREN|TECK", education_other) ~ 3L,
           grepl(c("UNIVERSITY|LLB|COLLEGE|3YR|3 YEARS"), education_other) ~ 6L,
           grepl(c("SECONDARY SCHOOL UNTIL 16|SOME SECONDARY"), education_other) ~ 2L,
           !is.na(education_mc) ~ as.integer(education_mc)
         ),
         education = factor(education,
                            levels = 1:7,
                            labels = c("Less than secondary", "Some secondary", 
                                       "Secondary school diploma", "Some university",
                                       "University 2 yr", "University 4 yr", 
                                       "Graduate/professional degree")))


# EMPRO PROM completeness
# function to count the not NA columns
count_not_na_func <- function(x) sum(!is.na(x))
formatted$proms <- empro %>%
  rename(subject=study_id) %>%
  mutate(visit = as.numeric(gsub("Month ", "", timepoint)),
         # percentage of PROM questions answered
         percent_prom_complete = 100*apply(.[paste0("ironman_ss_", 1:20)], 1, count_not_na_func)/20,
         prom_submitted = 1) %>%
  arrange(subject, visit) %>%
  pivot_wider(id_cols = "subject",
              names_from=visit,
              values_from=c(percent_prom_complete, prom_submitted))


# Trigger data - long format 
ynlev2 = c(0,1)
ynlab2 = c("No", "Yes")
triggers_long <- qre_timeline %>%
  rename(subject=study_id) %>%
  # filter out empty records
  filter(!is.na(EMPRO_questionnaire_completion_date) & status=="Completed") %>%
  # separate soft and hard triggers into separate variables for each domain
  mutate(soft_anxious = ifelse(grepl("anxious", soft_trigger_domains) & !grepl("anxious", hard_trigger_domains), 1, 0),
         soft_discouraged = ifelse(grepl("discouraged", soft_trigger_domains) & !grepl("discouraged", hard_trigger_domains), 1, 0),
         soft_fatigue = ifelse(grepl("fatigue", soft_trigger_domains) & !grepl("fatigue", hard_trigger_domains), 1, 0),
         soft_general_pain = ifelse(grepl("general_pain", soft_trigger_domains) & !grepl("general_pain", hard_trigger_domains), 1, 0),
         soft_insomnia = ifelse(grepl("insomnia", soft_trigger_domains) & !grepl("insomnia", hard_trigger_domains), 1, 0),
         soft_joint_pain = ifelse(grepl("joint_pain", soft_trigger_domains) & !grepl("joint_pain", hard_trigger_domains), 1, 0),
         soft_sad = ifelse(grepl("sad", soft_trigger_domains) & !grepl("sad", hard_trigger_domains), 1, 0),
         soft_social_isolation = ifelse(grepl("social_isolation", soft_trigger_domains) & 
                                          !grepl("social_isolation", hard_trigger_domains), 1, 0),
         hard_anxious = ifelse(grepl("anxious", hard_trigger_domains), 1, 0),
         hard_discouraged = ifelse(grepl("discouraged", hard_trigger_domains), 1, 0),
         hard_fatigue = ifelse(grepl("fatigue", hard_trigger_domains), 1, 0),
         hard_general_pain = ifelse(grepl("general_pain", hard_trigger_domains), 1, 0),
         hard_insomnia = ifelse(grepl("insomnia", hard_trigger_domains), 1, 0),
         hard_joint_pain = ifelse(grepl("joint_pain", hard_trigger_domains), 1, 0),
         hard_sad = ifelse(grepl("sad", hard_trigger_domains), 1, 0),
         hard_social_isolation = ifelse(grepl("social_isolation", hard_trigger_domains), 1, 0),
         # extract date
         q_date = as.Date(as.POSIXct(EMPRO_questionnaire_completion_date, format = "%d-%b-%Y %H:%M:%S"))) %>%
  left_join(trigreport, by = c("user_id", "visit")) %>%
  # clean up variables
  mutate(soft_trigger = case_when(
    is.na(soft_trigger) & soft_anxious + soft_discouraged + soft_fatigue + soft_general_pain +
      soft_insomnia + soft_joint_pain + soft_sad + soft_social_isolation ==0 ~ 0,
    is.na(soft_trigger) & soft_anxious + soft_discouraged + soft_fatigue + soft_general_pain +
      soft_insomnia + soft_joint_pain + soft_sad + soft_social_isolation >0  ~ 1,
    TRUE ~ soft_trigger),
    hard_trigger = case_when(
      is.na(hard_trigger) & hard_anxious + hard_discouraged + hard_fatigue + hard_general_pain +
        hard_insomnia + hard_joint_pain + hard_sad + hard_social_isolation ==0 ~ 0,
      is.na(hard_trigger) & hard_anxious + hard_discouraged + hard_fatigue + hard_general_pain +
        hard_insomnia + hard_joint_pain + hard_sad + hard_social_isolation >0  ~ 1,
      TRUE ~ hard_trigger),
    soft_or_hard_trigger = ifelse(soft_trigger==1 | hard_trigger==1, 1, 0),
    soft_anxious = factor(soft_anxious, levels = ynlev2, labels = ynlab2),
    soft_discouraged = factor(soft_discouraged, levels = ynlev2, labels = ynlab2),
    soft_fatigue = factor(soft_fatigue, levels = ynlev2, labels = ynlab2),
    soft_general_pain = factor(soft_general_pain, levels = ynlev2, labels = ynlab2),
    soft_insomnia = factor(soft_insomnia, levels = ynlev2, labels = ynlab2),
    soft_joint_pain = factor(soft_joint_pain, levels = ynlev2, labels = ynlab2),
    soft_sad = factor(soft_sad, levels = ynlev2, labels = ynlab2),
    soft_social_isolation = factor(soft_social_isolation, levels = ynlev2, labels = ynlab2),
    hard_anxious = factor(hard_anxious, levels = ynlev2, labels = ynlab2),
    hard_discouraged = factor(hard_discouraged, levels = ynlev2, labels = ynlab2),
    hard_fatigue = factor(hard_fatigue, levels = ynlev2, labels = ynlab2),
    hard_general_pain = factor(hard_general_pain, levels = ynlev2, labels = ynlab2),
    hard_insomnia = factor(hard_insomnia, levels = ynlev2, labels = ynlab2),
    hard_joint_pain = factor(hard_joint_pain, levels = ynlev2, labels = ynlab2),
    hard_sad = factor(hard_sad, levels = ynlev2, labels = ynlab2),
    hard_social_isolation = factor(hard_social_isolation, levels = ynlev2, labels = ynlab2),
    soft_trigger = factor(soft_trigger, levels = ynlev2, labels = ynlab2),
    hard_trigger = factor(hard_trigger, levels = ynlev2, labels = ynlab2),
    soft_or_hard_trigger = factor(soft_or_hard_trigger, levels = ynlev2, labels = ynlab2),
    after_bug = case_when(
      is.na(after_bug) & q_date < ymd("2024-07-21") ~ 0,
      is.na(after_bug) & q_date >= ymd("2024-07-21") ~ 1,
      TRUE ~ after_bug),
    after_bug = factor(after_bug, levels = 0:1, labels = c("Bug present", "Bug fixed")),
    self_mgmt_before_email = ifelse(after_bug=="Bug fixed", NA, self_mgmt_before_email),
    self_mgmt_after_email = ifelse(after_bug=="Bug fixed", NA, self_mgmt_after_email),
    self_mgmt = case_when(!is.na(content_domains_accessed) ~ 1,
                          self_mgmt_before_email==1 | self_mgmt_after_email==1 ~ 1,
                          TRUE ~ 0),
    self_mgmt_before_email = factor(self_mgmt_before_email, levels = ynlev2, labels = ynlab2),
    self_mgmt_after_email = factor(self_mgmt_after_email, levels = ynlev2, labels = ynlab2),
    self_mgmt = factor(self_mgmt, levels = ynlev2, labels = ynlab2)) %>%
  set_variable_labels(soft_anxious = "Anxious",
                      soft_discouraged = "Discouraged",
                      soft_fatigue = "Fatigue",
                      soft_general_pain = "General pain",
                      soft_insomnia = "Insomnia",
                      soft_joint_pain = "Joint pain",
                      soft_sad = "Sad",
                      soft_social_isolation = "Social Isolation",
                      hard_anxious = "Anxious",
                      hard_discouraged = "Discouraged",
                      hard_fatigue = "Fatigue",
                      hard_general_pain = "General pain",
                      hard_insomnia = "Insomnia",
                      hard_joint_pain = "Joint pain",
                      hard_sad = "Sad",
                      hard_social_isolation = "Social Isolation",                      
                      soft_trigger = "Any soft trigger",
                      hard_trigger = "Any hard trigger",
                      soft_or_hard_trigger = "Any soft or hard trigger",
                      self_mgmt_before_email = "Accessed content before email",
                      self_mgmt_after_email = "Accessed content after email",
                      self_mgmt = "Accessed content",
                      after_bug = "TrueNTH bug status")


# Triggers data
# Question: how to tell if patient engaged in TrueNTH self management tool
# Question: does clinician_status mean that they contacted the patient?
  # function to erase soft triggers that are also hard triggers
formatted$triggers <- qre_timeline %>%
  rename(subject=study_id) %>%
  # filter out empty records
  filter(!is.na(EMPRO_questionnaire_completion_date) & status=="Completed") %>%
  # separate soft triggers into separate variables
  separate_wider_delim(soft_trigger_domains, ", ", names_sep = "", too_few = "align_start") %>%
  # transpose to long
  pivot_longer(cols = starts_with("soft_trigger_domains"),
               names_to = "num",
               names_prefix = "soft_trigger_domains",
               values_to = "soft_trigger",
               values_drop_na = TRUE) %>%
  # replace repeated soft triggers to NA (double listed in hard and soft)
  rowwise() %>%
  mutate(soft_trigger = ifelse(grepl(soft_trigger, hard_trigger_domains), NA, soft_trigger)) %>%
  ungroup() %>%
  # mutate back to wide
  pivot_wider(id_cols = c(subject, visit, hard_trigger_domains, clinician_status, content_domains_accessed),
              names_from = num,
              names_prefix = "soft_trigger_domains", 
              values_from = soft_trigger) %>%
  # unite soft triggers into one column
  unite(soft_trigger_domains, starts_with("soft_trigger_domains"), sep = ", ", na.rm=TRUE) %>%
  mutate(soft_trigger_domains = ifelse(soft_trigger_domains=="", NA, soft_trigger_domains), 
         # number of surveys that released a trigger 
         soft_trigger = ifelse(!is.na(soft_trigger_domains), 1, 0),
         hard_trigger = ifelse(!is.na(hard_trigger_domains), 1, 0),
         # number of triggers
         n_soft_trigger = ifelse(is.na(soft_trigger_domains), NA, str_count(soft_trigger_domains, ",") + 1),
         n_hard_trigger = ifelse(is.na(hard_trigger_domains), NA, str_count(hard_trigger_domains, ",") + 1),
         pain_soft_trigger = ifelse(grepl("pain", soft_trigger_domains), 1, 0),
         pain_hard_trigger = ifelse(grepl("pain", hard_trigger_domains), 1, 0),
         clinician_contact = ifelse(clinician_status=="Completed", 1, 0),
         visit = as.numeric(gsub("Month ", "", visit)),
         hard_triggers_contact = ifelse(clinician_status=="Completed", 1, NA),
         self_mgmt_soft = case_when(
           !is.na(content_domains_accessed) & soft_trigger==1 ~ 1,
           is.na(content_domains_accessed) & soft_trigger==1 ~ 0
         ),
         self_mgmt_hard = case_when(
           !is.na(content_domains_accessed) & hard_trigger==1 ~ 1,
           is.na(content_domains_accessed) & hard_trigger==1 ~ 0
         )) %>% 
  select(subject, visit, clinician_contact, soft_trigger, hard_trigger, n_soft_trigger, n_hard_trigger, 
         pain_soft_trigger, pain_hard_trigger, hard_triggers_contact, self_mgmt_soft, self_mgmt_hard) %>%
  # transpose data
  arrange(subject, visit) %>%
  pivot_wider(id_cols = "subject",
              names_from=visit,
              values_from=c(clinician_contact, soft_trigger, hard_trigger, 
                            n_soft_trigger, n_hard_trigger, hard_triggers_contact,
                            pain_soft_trigger, pain_hard_trigger, self_mgmt_soft, self_mgmt_hard)) %>%
  mutate(n_visit_soft_trigger = select(., starts_with("soft_trigger_")) %>% rowSums(na.rm=TRUE),
         n_visit_hard_trigger = select(., starts_with("hard_trigger_")) %>% rowSums(na.rm=TRUE),
         n_total_soft_trigger = select(., starts_with("n_soft_trigger_")) %>% rowSums(na.rm=TRUE),
         n_total_hard_trigger = select(., starts_with("n_hard_trigger_")) %>% rowSums(na.rm=TRUE),
         n_clinician_contact = select(., starts_with("clinician_contact")) %>% rowSums(na.rm=TRUE),
         pain_hard_trigger = ifelse(select(., starts_with("pain_hard_trigger")) %>% rowSums(na.rm=TRUE) > 0, 1, 0),
         clinician_contact = ifelse(select(., starts_with("clinician_contact")) %>% rowSums(na.rm=TRUE) > 0, 1, 0),
         any_hard_trigger = ifelse(n_total_hard_trigger>0, 1, 0),
         any_soft_trigger = ifelse(n_total_soft_trigger>0, 1, 0),
         self_mgmt_soft = ifelse(select(., starts_with("self_mgmt_soft")) %>% rowSums(na.rm=TRUE) > 0, 1, 0),
         self_mgmt_hard = ifelse(select(., starts_with("self_mgmt_hard")) %>% rowSums(na.rm=TRUE) > 0, 1, 0))

# Last questionnaire completed
formatted$last_q <- qre_timeline %>%
  rename(subject=study_id) %>%
  # filter out empty records
  filter(!is.na(EMPRO_questionnaire_completion_date) & status=="Completed") %>%
  # choose last record for each patient
  mutate(last_empro_q = as.numeric(gsub("Month ", "", visit))) %>%
  arrange(subject, desc(last_empro_q)) %>% 
  group_by(subject) %>%
  slice(1) %>%
  select(subject, last_empro_q)
  

# completed questionnaires
# NOTE: this is not shifted based on enrollment month
# formatted$completed_q <- empro %>%
#   rename(subject=study_id) %>%
#   filter(status=="completed") %>%
#   select(subject, timepoint) %>%
#   mutate(complete=1,
#          timepoint = tolower(gsub(" ", "_", timepoint))) %>%
#   pivot_wider(names_from=timepoint, values_from=complete, names_prefix = "q_")
  
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
  mutate(timepoint = ifelse(!is.na(enrollment_month), timepoint - enrollment_month, timepoint),
         percent_eortc_complete = 100*apply(.[paste0("eortc_", 1:30)], 1, count_not_na_func)/30) %>%
  filter(timepoint %in% seq(0,12,3)) %>%
  select(subject, timepoint, eortc = QLQTOTAL, percent_eortc_complete) %>%
  filter(!is.na(eortc)) %>%
  arrange(timepoint) %>%
  pivot_wider(id_cols=subject, 
              names_from=timepoint, 
              values_from=c(percent_eortc_complete, eortc)) %>%
  mutate_at(vars(starts_with("percent_eortc_complete")), ~replace_na(.,0))

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
# exclude patients affected by 
# technical bug 
###############################

all_data <- all_data %>%
  filter(!(subject %in% c("170-48-051", "170-48-053", "170-48-055", "170-48-058", "170-48-063", 
         "170-48-047", "170-40-004", "170-40-033", "170-06-096", "170-06-099",
         "170-96-014", "170-96-024", "170-96-035", "170-96-073", "170-96-076")))

###############################
# select control group
###############################

# subjects have no EMPRO consent date (exclude for now)
noconsent <- all_data %>% filter(arm=="intervention" & is.na(empro_cnstdate)) %>% pull(subject)

# subjects recruited after 45 months (exclude)
lateenroll <- all_data %>% filter(arm=="intervention" & enrollment_month>45) %>% pull(subject)


# match on:
    # country
    # time from diagnosis to enrollment
    # enrollment year
    # study visit at entry
    # disease status at entry
    # age at entry
    # race
    # education level
matchdata = all_data %>%
  select(subject, diagbx_date_int, enrollment_month, cohort_empro, cohort_std, cohort,
         mcrpcdat, crpcdat, mhspcdat,
         brthdat, race, education, arm, empro_cnstdate) %>%
  mutate(race_gp = case_when(race=="White / Caucasian" ~ 1,
                             race=="African / African American / Black / Black British / Caribbean" ~ 2,
                             TRUE ~ 99),
         educ_gp = case_when(
           education %in% c("Less than secondary", "Some secondary", "Secondary school diploma") ~ 1,
           education %in% c("Some university", "University 2 yr", "University 4 yr", "Graduate/professional degree") ~ 2,
           TRUE ~ 99)) %>% 
  # subjects missing EMPRO consent date
  filter(subject %notin% noconsent) %>%
  # subject with late enrollment (>45 months) 
  filter(subject %notin% lateenroll) %>%
  # note: 7 subjects are not in cyc_date table
  # unique(arm_set$subject[arm_set$subject %notin% medidata$cyc_date$subject])
  left_join(medidata$cyc_date %>% 
              select(subject, visit_date, month, instance_repeat_number) %>% 
              filter(!month %in% c("48", "51", "54", "57", "60")), 
            by="subject", multiple="all") %>%
  mutate(visit_date = as.Date(ifelse(is.na(visit_date), empro_cnstdate, visit_date), origin = "1970-01-01"),
         # disease state at visit
         cohort_empro = case_when(
           !is.na(cohort_empro) ~ cohort_empro,
           mcrpcdat <= visit_date ~ "CRPC",
           crpcdat <= visit_date ~ "CRPC",
           mhspcdat <= visit_date ~ "mHSPC", 
           TRUE ~ cohort),
         cohort_std_empro = ifelse(cohort_empro=="CRPC", 2, 1)) %>% 
  filter(!is.na(visit_date) | arm == "intervention") %>% 
  mutate(month = ifelse(month==999, NA, month),
         age_visit = as.numeric(floor((visit_date - brthdat)/365.25)),
         enrollment_year = year(visit_date),
         dx_to_enroll = (diagbx_date_int %--% visit_date) / months(1),
         month = as.numeric(month),
         arm = as.numeric(ifelse(arm=="control", 0, 1)),
         # everything must be positive so shift +1 month
         #enrollment_month = enrollment_month + 1,
         #month = month + 1,
         month = ifelse(is.na(month), enrollment_month, month),
         enrollment_month = ifelse(is.na(enrollment_month), month, enrollment_month),
         # categorize month
         month_gp = case_when(
           month==0 ~ 1,
           month>0 & month<=12 ~ 2,
           month> 12 & month<=24 ~ 3,
           month> 24 & month<=36 ~ 4,
           month>36 ~ 5
         ),
         enrollment_month_gp = case_when(
           enrollment_month==0 ~ 1,
           enrollment_month>0 & enrollment_month<=12 ~ 2,
           enrollment_month> 12 & enrollment_month<=24 ~ 3,
           enrollment_month> 24 & enrollment_month<=36 ~ 4,
           enrollment_month>36 ~ 5
         )) %>% 
  select(-diagbx_date_int, -brthdat, -race, -education, -visit_date, -empro_cnstdate, -cohort_std, -cohort, -cohort_empro, -mcrpcdat, -crpcdat, -mhspcdat) %>%
  # 8/30/23 - this drops 2 patients in intervention arm missing cohort_std
  # 170-20-050, 170-20-051
  # mutate(drop = ifelse(!is.na(month) & !is.na(cohort_std_empro) & month!=999 & !is.na(age_visit), 0, 1)) %>%
  # group_by(subject) %>%
  # mutate(n_per_subject = n(), n_drop = sum(drop)) %>%
  # filter(n_per_subject==n_drop)
  filter(!is.na(month) & !is.na(cohort_std_empro) & month!=999 & !is.na(age_visit)) %>% 
  left_join(formatted$subject_data %>% select(subject, country), by="subject") %>%
  mutate(country_std = case_when(
    country=="Australia" ~ 1,
    country=="Canada" ~ 2,
    country=="England" ~ 3,
    country=="USA" ~ 4)) %>% 
  select(-month)
  

# # 8/7/25 - no longer needed
# # add row for all EMPRO patients since some are missing data around their enrollment data
# # only do this for patients who don't have a match
# all_emp <- matchdata1 %>% filter(arm==1) %>% pull(subject) %>% unique()
# match_emp <- matchdata1 %>% filter(arm==1 & enrollment_month==month) %>% pull(subject) %>% unique()
# nomatch_emp <- setdiff(all_emp, match_emp)
# matchdata2 = matchdata1 %>%
#   filter(arm==1 & subject %in% nomatch_emp) %>%
#   arrange(subject, month) %>%
#   group_by(subject) %>%
#   slice(1) %>%
#   ungroup() %>%
#   mutate(age_visit = floor(age_visit + (enrollment_month - month)/12),
#          enrollment_year = floor(enrollment_year + (enrollment_month - month)/12),
#          month = enrollment_month) 
#   
# # add extra rows, eliminate duplicates
# matchdata <- matchdata1 %>%
#   bind_rows(matchdata2)

# check number of subjects in matchdata
matchdata %>% filter(arm==1) %>% pull(subject) %>% unique() %>% length()

# this did not work because it requires lookback so I accessed the github code
# reduced_data <- reduce_data(data = matchdata, treat = "arm",
#                             tm = "month", entry = "enrollment_month",
#                             id = "subject", lookback = 3)

# reduce data
arm_set <- matchdata[matchdata$arm == 1 &
                    (matchdata$month_gp == matchdata$enrollment_month_gp), ] 
# patients without data entered get dropped at this step
comp_set  <- matchdata[matchdata$arm == 0 &
                    (matchdata$month_gp %in% unique(arm_set$month_gp)), ]
reduced_data <- dplyr::bind_rows(arm_set, comp_set)


# add propensity scores
fm <- as.formula(arm ~ as.factor(country_std) + as.factor(enrollment_year) + as.factor(enrollment_month_gp) + 
                   as.factor(cohort_std_empro) + age_visit + as.factor(race_gp) + as.factor(educ_gp))
vars <- all.vars(fm)
scored_data <- score_data(reduced_data = reduced_data,
                          model_type = "logistic", match_on = "logit",
                          fm = fm, treat = "arm",
                          tm = "month_gp", entry = "enrollment_month_gp", id = "subject")

# perform matching
# output <- rollmatch(scored_data, data=matchdata, treat = "arm",
#                     tm = "month", entry = "enrollment_month", id = "subject",
#                     vars = vars, lookback = 3, alpha = .5,
#                     standard_deviation = "average", num_matches = 1,
#                     replacement = FALSE)
# Create pool of possible matches
comparison_pool <-
  dplyr::inner_join(scored_data[scored_data$arm == 1, ],
                    scored_data[scored_data$arm == 0, ], by = "month_gp", multiple = "all",
                    relationship = "many-to-many")
comparison_pool$difference <-
  abs(comparison_pool$score.x - comparison_pool$score.y)
comparison_pool <- comparison_pool %>%
  dplyr::select(dplyr::one_of("month_gp", paste0("subject", ".x"), paste0("subject", ".y")),
                paste0("enrollment_month_gp", ".x"), paste0("enrollment_month_gp", ".y"),
                "score.x", "score.y", "difference", "enrollment_month.x", "enrollment_month.y")
names(comparison_pool) <- c("month_gp", "arm_subject", "control_subject",
                            paste0("arm_", "enrollment_month_gp"),
                            paste0("control_", "enrollment_month_gp"),
                            "arm_score", "control_score", "difference", 
                            "arm_enrollment_month", "control_enrollment_month")

# Trim pool based on specified caliper
s_df <- scored_data

var_treat <- var(s_df[(s_df$arm == 1), "score"])
var_untreat <- var(s_df[(s_df$arm == 0), "score"])

pooled_stdev <- sqrt( (var_treat + var_untreat) / 2)

width <- 0.5 * pooled_stdev
trimmed_pool <-
  dplyr::filter(comparison_pool,
                comparison_pool$difference <= as.numeric(width))

trimmed_pool <-
  trimmed_pool[order(trimmed_pool$month_gp,
                     trimmed_pool$arm_subject, trimmed_pool$difference), ]

    
# Using matching algorithm to find top matches
create_matches <- function(trimmed_pool, tm, num_matches = 1,
                           replacement = FALSE){

  names(trimmed_pool)[1] <- "time"
  difference <- ""; num.assigned <- ""; arm_subject <- ""; control_subject <- "";
  
  # initialize matches as empty
  matches <- trimmed_pool[0, ]
  
  count <- 1
  # Loop
  repeat {
    # first_choice is the first entry in the comparison pool for each arm_subject
    first_choice <- trimmed_pool[!duplicated(trimmed_pool$arm_subject), ]
    
    if (nrow(first_choice) == 0)
      break
    
    else {
      first_choice <- first_choice[order(first_choice$difference), ]
      first_choice <- first_choice[!duplicated(first_choice$control_subject), ]
      current_matches <- first_choice[order(first_choice$difference), ]
    }
    # Break out of loop if no matches were assigned
    if (nrow(current_matches) == 0)
      break
    
    matches <- dplyr::bind_rows(matches, current_matches)
    
    if (nrow(trimmed_pool) > 0) {
      #filter out assigned treatment/match pairs
      trimmed_pool <- dplyr::setdiff(trimmed_pool, current_matches)
      
      # Keep records where control_subject is not in unique(matches$control_subject)
      diff_control_subject <-
        dplyr::filter(trimmed_pool,
                      !(control_subject %in%
                          unique(current_matches$control_subject) ))
      
      #combine the rows to keep and re-sort comparison pool
      trimmed_pool <-
        dplyr::arrange(dplyr::bind_rows(diff_control_subject),
                       time, arm_subject, difference)
      
      # If num_matches matches have been assigned, remove the treatments
      # from the comparison pool
      matches_count <- matches[matches$arm_subject %in%
                                 unique(current_matches$arm_subject), ] %>%
        dplyr::group_by(arm_subject) %>%
        dplyr::summarise(num.assigned = n()) %>%
        dplyr::filter(num.assigned == num_matches)
      trimmed_pool <-
        dplyr::filter(trimmed_pool,
                      !(arm_subject %in% matches_count$arm_subject ))
      
    } else break  # break out of loop if comparison pool is empty
    count <- count + 1
  }
  names(matches)[1] <- "time"
  return(matches)
}


## find matches 
matches <- create_matches(trimmed_pool, trimmed_pool$month_gp, num_matches=1)

# patients without matches
ptlist = matchdata %>% filter(arm==1) %>% pull(subject) %>% unique()
not_matched <- ptlist[ptlist %notin% matches$arm_subject]; not_matched
# March 2023: "170-06-103" "170-06-104" "170-37-004"
# Aug 2023: "170-48-068" "170-48-071" "170-48-072" "170-48-073" "170-48-075"
# Nov 2023: "170-48-071" "170-48-078" "170-56-059"
# March 2024: "170-37-041" "170-48-071" "170-48-075" "170-48-078" "170-48-080" "170-48-081"
# June 2024: "170-20-052" "170-48-071" "170-48-080" "170-48-081" "170-56-081" "170-56-082"
# June 2024 v2: "170-20-052" "170-37-041" "170-37-045" "170-48-071" "170-48-080" "170-56-081" "170-56-082"
# Sept 2024: "170-37-031" "170-37-041" "170-37-045" "170-37-046" "170-37-047" "170-37-048" "170-48-071" "170-48-078" "170-48-080" "170-48-081" "170-48-083" "170-48-085"
# Dec 2024: "170-20-052" "170-20-054" "170-20-056" "170-37-046" "170-37-049" "170-37-050" "170-48-071" "170-48-078" "170-48-080" "170-48-083" "170-48-085"
# February 2025: "170-37-050" "170-48-083" "170-48-088" "170-48-090" "170-48-091"
# May 2025: "170-48-083" "170-48-088" "170-48-091"
# August 2025: all matched (same dataset as previous but categorized enrollment month and fixed a few things)
# August 2025 (new data): all matched
# November 2025: all matched

# select necessary variables to merge into big dataset
controls <- matches %>% select(subject = control_subject, 
                               control_enrollment_month_gp,
                               control_enrollment_month) %>%
  mutate(match = 1) %>%
  left_join(medidata$cyc_date %>% 
              select(subject, visit_date, month, instance_repeat_number) %>%
              mutate(month = as.numeric(month)), 
            by=c("subject", "control_enrollment_month"="month")) %>%
  #mutate(enrollment_month_gp = ifelse(subject=="170-82-099" & instance_repeat_number==10, "33", enrollment_month_gp)) %>%
  select(-instance_repeat_number) %>% 
  rename(enrollment_month2=control_enrollment_month, visit_date2=visit_date)



###############################
# create new variables
###############################

# QUESTION: one patient was recruited at 57 months. Will he be able to complete one year on EMPRO?
ynlev2 = c(0,1)
ynlab2 = c("No", "Yes")

# shift questionnaires for controls based on matched enrollment month
control_eortc <- proms %>%
  rename(subject=study_id) %>%
  mutate_at(vars(starts_with("eortc")), as.numeric) %>%
  right_join(controls, by="subject") %>%
  bind_cols(qlq_c30(., 'eortc_')) %>%
  mutate(timepoint = as.numeric(ifelse(timepoint=="Baseline", 0, gsub("Month", "", timepoint)))) %>%
  arrange(subject, timepoint, eortc_1) %>%
  group_by(subject, timepoint) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(timepoint = timepoint - enrollment_month2,
         new_percent_eortc_complete = 100*apply(.[paste0("eortc_", 1:30)], 1, count_not_na_func)/30) %>%
  filter(timepoint %in% seq(0,12,3)) %>%
  select(subject, timepoint, new_eortc = QLQTOTAL, new_percent_eortc_complete) %>%
  filter(!is.na(new_eortc)) %>%
  arrange(timepoint) %>%
  pivot_wider(id_cols=subject, 
              names_from=timepoint, 
              values_from=c(new_percent_eortc_complete, new_eortc)) %>%
  mutate_at(vars(starts_with("new_percent_eortc_complete")), ~replace_na(.,0))

# flag for whether patient completed each EOTRC (updated for control data)
control_eortc_yn = control_eortc %>%
  select(subject, starts_with("new_eortc")) %>%
  mutate_at(vars(starts_with("new_eortc")), not.na) %>%
  rename_with(stringr::str_replace, 
              pattern = "new_eortc", replacement = "new_yn_eortc", 
              matches("new_eortc"))

empro_data <- all_data %>%
  left_join(controls, by="subject") %>%
  filter(arm=="intervention" | match ==1) %>%
  # # subjects missing EMPRO consent date
  # filter(subject %notin% noconsent) %>%
  # # subject with late enrollment (>45 months) 
  # filter(subject %notin% lateenroll) %>%
  # merge in updated EORTC data for controls and replace with current
  left_join(control_eortc, by="subject") %>%
  left_join(control_eortc_yn, by="subject") %>%
  mutate(eortc_0 = ifelse(arm=="control", new_eortc_0, eortc_0),
         eortc_3 = ifelse(arm=="control", new_eortc_3, eortc_3),
         eortc_6 = ifelse(arm=="control", new_eortc_6, eortc_6),
         eortc_9 = ifelse(arm=="control", new_eortc_9, eortc_9),
         eortc_12 = ifelse(arm=="control", new_eortc_12, eortc_12),
         percent_eortc_complete_0 = ifelse(arm=="control", new_percent_eortc_complete_0, percent_eortc_complete_0),
         percent_eortc_complete_3 = ifelse(arm=="control", new_percent_eortc_complete_3, percent_eortc_complete_3),
         percent_eortc_complete_6 = ifelse(arm=="control", new_percent_eortc_complete_6, percent_eortc_complete_6),
         percent_eortc_complete_9 = ifelse(arm=="control", new_percent_eortc_complete_9, percent_eortc_complete_9),
         percent_eortc_complete_12 = ifelse(arm=="control", new_percent_eortc_complete_12, percent_eortc_complete_12),
         yn_eortc_0 = ifelse(arm=="control", new_yn_eortc_0, yn_eortc_0),
         yn_eortc_3 = ifelse(arm=="control", new_yn_eortc_3, yn_eortc_3),
         yn_eortc_6 = ifelse(arm=="control", new_yn_eortc_6, yn_eortc_6),
         yn_eortc_9 = ifelse(arm=="control", new_yn_eortc_9, yn_eortc_9),
         yn_eortc_12 = ifelse(arm=="control", new_yn_eortc_12, yn_eortc_12)) %>%
  select(-starts_with("new_")) %>%
  mutate(
    # combine match data (proxy controls have theoretical entry date)
    empro_cnstdate = as.Date(ifelse(is.na(empro_cnstdate), visit_date2, empro_cnstdate), 
                             origin = "1970-01-01"),
    enrollment_month = ifelse(is.na(enrollment_month), enrollment_month2, enrollment_month),
    enrollment_month_gp = case_when(
      enrollment_month==0 ~ 1,
      enrollment_month>0 & enrollment_month<=12 ~ 2,
      enrollment_month> 12 & enrollment_month<=24 ~ 3,
      enrollment_month> 24 & enrollment_month<=36 ~ 4,
      enrollment_month>36 ~ 5
    ),
    enrollment_month_gp = factor(enrollment_month_gp,
                                  levels = 1:5,
                                  labels = c("Baseline", "Month 3-12", "Month 15-24", "Month 27-36", "After month 36")),
    # other variables
    age_baseline = as.numeric(floor((empro_cnstdate - brthdat)/365.25)),
    year_emp_enrollment = factor(year(empro_cnstdate)),
    year_iron_enrollment = factor(year(cnstdate)),
    race_gp = case_when(race=="White / Caucasian" ~ 1,
                        race=="African / African American / Black / Black British / Caribbean" ~ 2,
                        TRUE ~ 3),
    race_gp = factor(race_gp, 
                     levels = 1:3,
                     labels = c("White", "Black", "Other/Unknown")),
    educ_gp = case_when(
      education %in% c("Less than secondary", "Some secondary", "Secondary school diploma") ~ 1,
      education %in% c("Some university", "University 2 yr", "University 4 yr", "Graduate/professional degree") ~ 2,
      TRUE ~ 99),
    educ_gp = factor(educ_gp, levels = c(1,2,99), labels = c("Secondary school or less", "College or more", "Unknown")),
    diag_enroll = interval(diagbx_date_int, empro_cnstdate) %/% months(1),
    dead = ifelse(is.na(dead), 0, dead),
    dead = factor(dead, levels=ynlev2, labels=ynlab2),
    limbo = ifelse(dead=="Yes", 
                   time_length(deathdate_int - empro_cnstdate, unit = "month"),
                   time_length(censor_date - empro_cnstdate, unit = "month")),
    dead12 = ifelse(dead=="Yes" & limbo<12, 1, 0),
    pain_hard_trigger = ifelse(is.na(pain_hard_trigger) & arm=="intervention", 0, pain_hard_trigger),
    pain_hard_trigger = factor(pain_hard_trigger, levels=ynlev2, labels=ynlab2),
    any_soft_trigger = ifelse(is.na(any_soft_trigger),0,1),
    any_soft_trigger = factor(any_soft_trigger, levels=ynlev2, labels=ynlab2), 
    any_hard_trigger = ifelse(is.na(any_hard_trigger),0,1),
    any_hard_trigger = factor(any_hard_trigger, levels=ynlev2, labels=ynlab2),     
    n_total_soft_trigger = ifelse(is.na(n_total_soft_trigger) & arm=="intervention", 0, n_total_soft_trigger),
    n_visit_soft_trigger = ifelse(is.na(n_visit_soft_trigger) & arm=="intervention", 0, n_visit_soft_trigger),
    n_total_hard_trigger = ifelse(is.na(n_total_hard_trigger) & arm=="intervention", 0, n_total_hard_trigger),
    n_visit_hard_trigger = ifelse(is.na(n_visit_hard_trigger) & arm=="intervention", 0, n_visit_hard_trigger),
    n_clinician_contact = ifelse(is.na(n_clinician_contact) & n_visit_hard_trigger>0, 0, n_clinician_contact),
    self_mgmt_soft = factor(self_mgmt_soft, levels=ynlev2, labels=ynlab2),
    self_mgmt_hard = factor(self_mgmt_hard, levels=ynlev2, labels=ynlab2),
    #clinician_contact = ifelse(any_hard_trigger==0, NA, clinician_contact),
    # q_month_1 = ifelse(is.na(q_month_1) & arm=="intervention", 0, q_month_1),
    # q_month_2 = ifelse(is.na(q_month_2) & arm=="intervention", 0, q_month_2),
    # q_month_3 = ifelse(is.na(q_month_3) & arm=="intervention", 0, q_month_3),
    # q_month_4 = ifelse(is.na(q_month_4) & arm=="intervention", 0, q_month_4),
    # q_month_5 = ifelse(is.na(q_month_5) & arm=="intervention", 0, q_month_5),
    # q_month_6 = ifelse(is.na(q_month_6) & arm=="intervention", 0, q_month_6),
    # q_month_7 = ifelse(is.na(q_month_7) & arm=="intervention", 0, q_month_7),
    yn_eortc_0 = ifelse(is.na(yn_eortc_0), FALSE, yn_eortc_0),
    yn_eortc_3 = ifelse(is.na(yn_eortc_3), FALSE, yn_eortc_3),
    yn_eortc_6 = ifelse(is.na(yn_eortc_6), FALSE, yn_eortc_6),
    yn_eortc_9 = ifelse(is.na(yn_eortc_9), FALSE, yn_eortc_9),
    yn_eortc_12 = ifelse(is.na(yn_eortc_12), FALSE, yn_eortc_12),
    prom_submitted_1 = ifelse(is.na(prom_submitted_1),0,1),
    prom_submitted_2 = ifelse(is.na(prom_submitted_2),0,1),
    prom_submitted_3 = ifelse(is.na(prom_submitted_3),0,1),
    prom_submitted_4 = ifelse(is.na(prom_submitted_4),0,1),
    prom_submitted_5 = ifelse(is.na(prom_submitted_5),0,1),
    prom_submitted_6 = ifelse(is.na(prom_submitted_6),0,1),
    prom_submitted_7 = ifelse(is.na(prom_submitted_7),0,1),
    prom_submitted_8 = ifelse(is.na(prom_submitted_8),0,1),
    prom_submitted_9 = ifelse(is.na(prom_submitted_9),0,1),
    prom_submitted_10 = ifelse(is.na(prom_submitted_10),0,1),
    prom_submitted_11 = ifelse(is.na(prom_submitted_11),0,1),
    prom_submitted_12 = ifelse(is.na(prom_submitted_12),0,1),
    off_study = case_when(
      !is.na(suboffstudyreason) & arm=="intervention" ~ 1,
      is.na(suboffstudyreason) & arm=="intervention" ~ 0,
      TRUE ~ NA),
    remaining_q = ifelse(off_study==1, 0, 12-last_empro_q)) %>% 
  # fix cohort variable
  select(-cohort_std_empro, -cohort_empro) %>% 
  left_join(matchdata %>% select(subject, enrollment_month, cohort_std_empro), 
            by=c("subject", "enrollment_month")) %>% 
  mutate(cohort_empro = ifelse(cohort_std_empro==1, "mHSPC", "CRPC")) %>% 
  distinct()
              

# which EMPRO visits a patient has completed
visits <- empro_data %>%
  select(subject, enrollment_month) %>%
  left_join(medidata$cyc_date %>% select(subject, visit_date, month, instance_repeat_number), by="subject", multiple="all") %>%
  mutate(month = as.numeric(month)) %>%
  mutate(month = ifelse(subject=="170-82-099" & instance_repeat_number==10, 33, month)) %>%
  filter((month >= enrollment_month | is.na(month)) & month!=999) %>%
  mutate(month_on_empro = paste0("completed_month_", month - enrollment_month),
         complete=1) %>%
  select(subject, month_on_empro, complete) %>%
  unique() %>%
  pivot_wider(id_cols = "subject",
              names_from=month_on_empro,
              values_from=complete)
empro_data <- empro_data %>%
  left_join(visits, by="subject")


# label variables
var_label(empro_data) <- list(
  country = "Country",
  age_baseline = "Age at study entry",
  race_gp = "Race",
  diag_enroll = "Months from diagnosis to enrollment",
  year_emp_enrollment = "Year of EMPRO enrollment",
  year_iron_enrollment = "Year of IRONMAN enrollment",
  enrollment_month = "Study visit at entry",
  enrollment_month_gp = "IRONMAN visit at entry",
  cohort = "Study cohort",
  cohort_empro = "Study cohort",
  education = "Education level",
  educ_gp = "Education level",
  yn_eortc_0 = "EORTC QLQ-C30 submitted at baseline",
  yn_eortc_3 = "EORTC QLQ-C30 submitted at 3 mo",
  yn_eortc_6 = "EORTC QLQ-C30 submitted at 6 mo",
  yn_eortc_9 = "EORTC QLQ-C30 submitted at 9 mo",
  yn_eortc_12 = "EORTC QLQ-C30 submitted at 12 mo",
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
  any_hard_trigger = "Any hard trigger released",
  any_soft_trigger = "Any soft trigger released",
  self_mgmt_soft = "Engaged in TrueNTH self-management tool after a soft trigger was released",
  self_mgmt_hard = "Engaged in TrueNTH self-management tool after a hard trigger was released",
  percent_prom_complete_1 = "% PROMs completed at 1 mo", 
  percent_prom_complete_2 = "% PROMs completed at 2 mo", 
  percent_prom_complete_3 = "% PROMs completed at 3 mo", 
  percent_prom_complete_4 = "% PROMs completed at 4 mo", 
  percent_prom_complete_5 = "% PROMs completed at 5 mo", 
  percent_prom_complete_6 = "% PROMs completed at 6 mo", 
  percent_prom_complete_7 = "% PROMs completed at 7 mo", 
  percent_prom_complete_8 = "% PROMs completed at 8 mo", 
  percent_prom_complete_9 = "% PROMs completed at 9 mo", 
  percent_prom_complete_10 = "% PROMs completed at 10 mo", 
  percent_prom_complete_11 = "% PROMs completed at 11 mo", 
  percent_prom_complete_12 = "% PROMs completed at 12 mo", 
  prom_submitted_1 = "PROM submitted at 1 mo", 
  prom_submitted_2 = "PROM submitted at 2 mo", 
  prom_submitted_3 = "PROM submitted at 3 mo", 
  prom_submitted_4 = "PROM submitted at 4 mo", 
  prom_submitted_5 = "PROM submitted at 5 mo", 
  prom_submitted_6 = "PROM submitted at 6 mo", 
  prom_submitted_7 = "PROM submitted at 7 mo", 
  prom_submitted_8 = "PROM submitted at 8 mo", 
  prom_submitted_9 = "PROM submitted at 9 mo", 
  prom_submitted_10 = "PROM submitted at 10 mo",
  prom_submitted_11 = "PROM submitted at 11 mo", 
  prom_submitted_12 = "PROM submitted at 12 mo",
  percent_eortc_complete_0 = "% EORTC complete at baseline",
  percent_eortc_complete_3 = "% EORTC complete at 3 mo",
  percent_eortc_complete_6 = "% EORTC complete at 6 mo",
  percent_eortc_complete_9 = "% EORTC complete at 9 mo",
  percent_eortc_complete_12 = "% EORTC complete at 12 mo")
  # q_month_1 = "Completed PRO-CTCAE questionnare at 1 mo",
  # q_month_2 = "Completed PRO-CTCAE questionnare at 2 mo",
  # q_month_3 = "Completed PRO-CTCAE questionnare at 3 mo",
  # q_month_4 = "Completed PRO-CTCAE questionnare at 4 mo",
  # q_month_5 = "Completed PRO-CTCAE questionnare at 5 mo",
  # q_month_6 = "Completed PRO-CTCAE questionnare at 6 mo",
  # q_month_7 = "Completed PRO-CTCAE questionnare at 7 mo")

##Save the result for analysis 
today_rec <- format(Sys.Date(), format="%Y%m%d")
savfile<-here::here("data", paste0("EMPRO_data_", today_rec, ".RData"))
save(empro_data, not_matched, noconsent, lateenroll, triggers_long, file= savfile)
tools::md5sum(savfile)

    

