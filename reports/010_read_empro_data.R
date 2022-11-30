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

# update date and export folder name
datafolder <- "2022-11-14_EMPRO"
datafile <- "2022-11-14_empro.RData"

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
  select(subject, site = site_name_true_nth, site_country) 


# Pull out fields from COHORT table
formatted$cohort_data <- medidata$cohort %>%
  select(subject, cohort, cohort_std)

# Pull out fields from CONSENT table
formatted$consent_data <- medidata$ic %>%
  select(subject, cnstdate)

# Pull out fields from MHDIAGBX table
formatted$diagnosis_data <- medidata$mhdiagbx %>%
  select(subject, diagbx_date_int)

# pull data from DEMOGRAPHICS table
formatted$demo_data <- medidata$dm %>%
  select(subject, brthdat_int)

# Pull out EMPRO consent date
formatted$empro_consent <- medidata$sub_icds %>%
  select(subject, sub_icdsstdat)

# Pull out EMPRO off study info
formatted$empro_off <- medidata$sub_off %>%
  select(subject, sub_offstdat, suboffstudyreason)

# pull death data from PSTATUS
formatted$death_data <- medidata$pstatus %>%
  select(subject, deathdate_int, cod_std) %>%
  mutate(dead=1)

###############################
# score questionnaires 
###############################

# There are as yet no standardized scoring rules for how to combine attributes 
# into a single score or how best to analyze PRO-CTCAE data longitudinally.

###############################
# merge all datasets
###############################

all_data = Reduce(function(...) merge(..., by="subject", all=T), formatted) 
