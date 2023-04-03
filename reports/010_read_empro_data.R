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


#testing push


#install.packages("purrr")

# update date and export folder name
datafolder <- "2023-02-16_EMPRO"
datafile <- "2023-02-16_empro.RData"

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
load(here::here("data", datafolder, datafile))


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
               select(subject, visit_date, month), by="subject", multiple="all") %>%
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
         hard_trigger_contact = ifelse(clinician_status=="Completed", 1, NA),
         self_mgmt_soft = case_when(
           !is.na(content_domains_accessed) & soft_trigger==1 ~ 1,
           is.na(content_domains_accessed) & soft_trigger==1 ~ 0
         ),
         self_mgmt_hard = case_when(
           !is.na(content_domains_accessed) & hard_trigger==1 ~ 1,
           is.na(content_domains_accessed) & hard_trigger==1 ~ 0
         )) %>% 
  select(subject, visit, clinician_contact, soft_trigger, hard_trigger, n_soft_trigger, n_hard_trigger, 
         pain_soft_trigger, pain_hard_trigger, hard_trigger_contact, self_mgmt_soft, self_mgmt_hard) %>%
  # transpose data
  arrange(subject, visit) %>%
  pivot_wider(id_cols = "subject",
              names_from=visit,
              values_from=c(clinician_contact, soft_trigger, hard_trigger, 
                            n_soft_trigger, n_hard_trigger, hard_trigger_contact,
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
# exclude patients affected by 
# technical bug 
###############################

all_data <- all_data %>%
  filter(!(subject %in% c("170-48-051", "170-48-053", "170-48-055", "170-48-058", "170-48-063", 
         "170-48-047", "170-40-004", "170-40-033", "170-06-096", "170-06-099",
         "170-96-014", "170-96-024", "170-96-035", "170-96-073", "170-96-076")))

# patient "170-48-056" 3 month survey data was deleted

# These patients are in the subject table (arm=intervention) but not in the match
# data, i.e. missing critical data somewhere.
# "170-06-096" - bug
# "170-06-099" - bug
# "170-40-004" - bug
# "170-40-033" - bug
# "170-48-047" - bug
# "170-48-051" - bug
# "170-48-053" - bug
# "170-48-055" - bug
# "170-48-058" - bug
# "170-48-063" - bug
# "170-96-006" - no EMPRO consent date
# "170-96-014" - bug
# "170-96-023" - no EMPRO consent date
# "170-96-024" - bug
# "170-96-035" - bug
# "170-96-038" - no EMPRO consent date
# "170-96-048" - not in cyc_date table; was able to add back in
# "170-96-073" - bug
# "170-96-074" - not in cyc_date table; was able to add back in
# "170-96-076" - bug
# "170-96-078" - not in cyc_date table; was able to add back in

###############################
# select control group
###############################

# subjects have no EMPRO consent date (exclude for now)
noconsent <- all_data %>% filter(arm=="intervention" & is.na(empro_cnstdate)) %>% pull(subject)


# match on:
    # country
    # time from diagnosis to enrollment
    # enrollment year
    # study visit at entry
    # disease status
    # age at entry
    # race
    # education level
matchdata1 = all_data %>%
  select(subject, diagbx_date_int, enrollment_month, cohort_std, brthdat_int, race, education_mc, arm, empro_cnstdate) %>%
  mutate(race_gp = case_when(race=="White / Caucasian" ~ 1,
                             race=="African / African American / Black / Black British / Caribbean" ~ 2,
                             TRUE ~ 99),
         educ_gp = case_when(
           education_mc %in% c("less than secondary", "some secondary", "secondary school diploma") ~ 1,
           education_mc %in% c("some university", "university 2 yr", "university 4 yr") ~ 2,
           TRUE ~ 99)) %>% 
  # note: 4 subjects have no EMPRO consent date (exclude for now)
  # all_data %>% filter(arm=="intervention" & is.na(empro_cnstdate)) %>% pull(subject)
  filter(subject %notin% noconsent) %>%
  # note: 7 subjects are not in cyc_date table
  # unique(arm_set$subject[arm_set$subject %notin% medidata$cyc_date$subject])
  full_join(medidata$cyc_date %>% select(subject, visit_date, month), by="subject", multiple="all") %>%
  mutate(visit_date = as.Date(ifelse(is.na(visit_date), empro_cnstdate, visit_date), origin = "1970-01-01")) %>% 
  filter(!is.na(visit_date) | arm == "intervention") %>% 
  mutate(month = ifelse(month==999, NA, month),
         age_visit = as.numeric(floor((visit_date - brthdat_int)/365.25)),
         enrollment_year = year(visit_date),
         dx_to_enroll = (diagbx_date_int %--% visit_date) / months(1),
         month = as.numeric(month),
         arm = as.numeric(ifelse(arm=="control", 0, 1)),
         # everything must be positive so shift +1 month
         enrollment_month = enrollment_month + 1,
         month = month + 1,
         month = ifelse(is.na(month), enrollment_month, month),
         enrollment_month = ifelse(is.na(enrollment_month), month, enrollment_month)
         ) %>% 
  select(-diagbx_date_int, -brthdat_int, -race, -education_mc, -visit_date, -empro_cnstdate) %>%
  filter(!is.na(month) & !is.na(cohort_std) & month!=999 & !is.na(age_visit)) %>% 
  left_join(formatted$subject_data %>% select(subject, country), by="subject") %>%
  mutate(country_std = case_when(
    country=="Australia" ~ 1,
    country=="Canada" ~ 2,
    country=="England" ~ 3,
    country=="USA" ~ 4))

# add row for all EMPRO patients since some are missing data around their enrollment data
# only do this for patients who don't have a match
all_emp <- matchdata1 %>% filter(arm==1) %>% pull(subject) %>% unique()
match_emp <- matchdata1 %>% filter(arm==1 & enrollment_month==month) %>% pull(subject) %>% unique()
nomatch_emp <- setdiff(all_emp, match_emp)
matchdata2 = matchdata1 %>%
  filter(arm==1 & subject %in% nomatch_emp) %>%
  arrange(subject, month) %>%
  group_by(subject) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(age_visit = floor(age_visit + (enrollment_month - month)/12),
         enrollment_year = floor(enrollment_year + (enrollment_month - month)/12),
         month = enrollment_month) 
  
# add extra rows, eliminate duplicates
matchdata <- matchdata1 %>%
  bind_rows(matchdata2)

# check number of subjects in matchdata
matchdata %>% filter(arm==1) %>% pull(subject) %>% unique() %>% length()

# this did not work because it requires lookback so I accessed the github code
# reduced_data <- reduce_data(data = matchdata, treat = "arm",
#                             tm = "month", entry = "enrollment_month",
#                             id = "subject", lookback = 3)

# reduce data
arm_set <- matchdata[matchdata$arm == 1 &
                    (matchdata$month == matchdata$enrollment_month), ] 
# patients without data entered get dropped at this step
comp_set  <- matchdata[matchdata$arm == 0 &
                    (matchdata$month %in% unique(arm_set$month)), ]
reduced_data <- dplyr::bind_rows(arm_set, comp_set)


# add propensity scores
fm <- as.formula(arm ~ as.factor(country_std) + enrollment_year + enrollment_month + 
                   as.factor(cohort_std) + age_visit + as.factor(race_gp) + as.factor(educ_gp))
vars <- all.vars(fm)
scored_data <- score_data(reduced_data = reduced_data,
                          model_type = "logistic", match_on = "logit",
                          fm = fm, treat = "arm",
                          tm = "month", entry = "enrollment_month", id = "subject")

# perform matching
# output <- rollmatch(scored_data, data=matchdata, treat = "arm",
#                     tm = "month", entry = "enrollment_month", id = "subject",
#                     vars = vars, lookback = 3, alpha = .5,
#                     standard_deviation = "average", num_matches = 1,
#                     replacement = FALSE)
# Create pool of possible matches
comparison_pool <-
  dplyr::inner_join(scored_data[scored_data$arm == 1, ],
                    scored_data[scored_data$arm == 0, ], by = "month", multiple = "all")
comparison_pool$difference <-
  abs(comparison_pool$score.x - comparison_pool$score.y)
comparison_pool <- comparison_pool %>%
  dplyr::select(dplyr::one_of("month", paste0("subject", ".x"), paste0("subject", ".y")),
                paste0("enrollment_month", ".x"), paste0("enrollment_month", ".y"),
                "score.x", "score.y", "difference")
names(comparison_pool) <- c("month", "arm_subject", "control_subject",
                            paste0("arm_", "enrollment_month"),
                            paste0("control_", "enrollment_month"),
                            "arm_score", "control_score", "difference")

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
  trimmed_pool[order(trimmed_pool$month,
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
  names(matches)[1] <- tm
  return(matches)
}


## find matches 
matches <- create_matches(trimmed_pool, trimmed_pool$month, num_matches=1)

# patients without matches - March 2023
ptlist = matchdata %>% filter(arm==1) %>% pull(subject) %>% unique()
ptlist[ptlist %notin% matches$arm_subject]
# "170-06-103" "170-06-104" "170-37-004"

# select necessary variables to merge into big dataset
controls <- matches %>% select(subject = control_subject, 
                               enrollment_month = control_enrollment_month) %>%
  mutate(enrollment_month = enrollment_month - 1, 
         match = 1) %>%
  left_join(medidata$cyc_date %>% 
              select(subject, visit_date, month) %>%
              mutate(month = as.numeric(month)), 
            by=c("subject", "enrollment_month"="month")) %>%
  rename(enrollment_month2=enrollment_month, visit_date2=visit_date)


###############################
# create new variables
###############################

# QUESTION: one patient was recruited at 51 months. Will he be able to complete one year on EMPRO?
ynlev2 = c(0,1)
ynlab2 = c("No", "Yes")

empro_data <- all_data %>%
  left_join(controls, by="subject") %>%
  filter(arm=="intervention" | match ==1) %>%
  # note: 3 subjects have no EMPRO consent date (exclude for now)
  filter(subject %notin% noconsent) %>%
  # technical bug 
  filter(!(subject %in% c("170-48-051", "170-48-053", "170-48-055", "170-48-058", "170-48-063", 
                          "170-48-047", "170-40-004", "170-40-033", "170-06-096", "170-06-099",
                          "170-96-014", "170-96-024", "170-96-035", "170-96-073", "170-96-076"))) %>%
  mutate(
    # combine match data (proxy controls have theoretical entry date)
    empro_cnstdate = as.Date(ifelse(is.na(empro_cnstdate), visit_date2, empro_cnstdate), 
                             origin = "1970-01-01"),
    enrollment_month = ifelse(is.na(enrollment_month), enrollment_month2, enrollment_month),
    # other variables
    age_baseline = as.numeric(floor((empro_cnstdate - brthdat_int)/365.25)),
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
    any_soft_trigger = factor(any_soft_trigger, levels=ynlev2, labels=ynlab2), 
    any_hard_trigger = factor(any_hard_trigger, levels=ynlev2, labels=ynlab2),     
    n_total_soft_trigger = ifelse(is.na(n_total_soft_trigger) & arm=="intervention", 0, n_total_soft_trigger),
    n_visit_soft_trigger = ifelse(is.na(n_visit_soft_trigger) & arm=="intervention", 0, n_visit_soft_trigger),
    n_total_hard_trigger = ifelse(is.na(n_total_hard_trigger) & arm=="intervention", 0, n_total_hard_trigger),
    n_visit_hard_trigger = ifelse(is.na(n_visit_hard_trigger) & arm=="intervention", 0, n_visit_hard_trigger),
    n_clinician_contact = ifelse(is.na(n_clinician_contact) & n_visit_hard_trigger>0, 0, n_clinician_contact),
    self_mgmt_soft = factor(self_mgmt_soft, levels=ynlev2, labels=ynlab2),
    self_mgmt_hard = factor(self_mgmt_hard, levels=ynlev2, labels=ynlab2),
    #clinician_contact = ifelse(any_hard_trigger==0, NA, clinician_contact),
    q_month_1 = ifelse(is.na(q_month_1) & arm=="intervention", 0, q_month_1),
    q_month_2 = ifelse(is.na(q_month_2) & arm=="intervention", 0, q_month_2),
    q_month_3 = ifelse(is.na(q_month_3) & arm=="intervention", 0, q_month_3),
    q_month_4 = ifelse(is.na(q_month_4) & arm=="intervention", 0, q_month_4),
    q_month_5 = ifelse(is.na(q_month_5) & arm=="intervention", 0, q_month_5),
    q_month_6 = ifelse(is.na(q_month_6) & arm=="intervention", 0, q_month_6),
    q_month_7 = ifelse(is.na(q_month_7) & arm=="intervention", 0, q_month_7),
    yn_eortc_0 = ifelse(is.na(yn_eortc_0), FALSE, yn_eortc_0),
    yn_eortc_3 = ifelse(is.na(yn_eortc_3), FALSE, yn_eortc_3),
    yn_eortc_6 = ifelse(is.na(yn_eortc_6), FALSE, yn_eortc_6),
    yn_eortc_9 = ifelse(is.na(yn_eortc_9), FALSE, yn_eortc_9),
    yn_eortc_12 = ifelse(is.na(yn_eortc_12), FALSE, yn_eortc_12)) 

# label variables
var_label(empro_data) <- list(
  country = "Country",
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
  any_hard_trigger = "Any hard trigger released",
  any_soft_trigger = "Any soft trigger released",
  self_mgmt_soft = "Engaged in TrueNTH self-management tool after a soft trigger was released",
  self_mgmt_hard = "Engaged in TrueNTH self-management tool after a hard trigger was released",
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

    

