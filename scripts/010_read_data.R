##############################
# IRONMAN study
# create dataset of:
#   patient ID
#   site
#   country
#   PC diagnosis date
#   IRONMAN enrollment date
#   disease status (CRPC vs. mHSPC)
##############################

library(readxl)

source('ironman_github/utils/load_sas_medidata.R')

# Directory where data are stored
#dir<-'H:\\GU\\IRONMAN\\Data\\2020-12-14\\'
dir<-'H:\\GU\\IRONMAN\\Data\\2021-04-08\\'
#dir <- 'C:\\Users\\nn31\\u_data\\ironman_empro\\'

# TOP_LEVEL_DATA_DIRECTORY = "C:\\Users\\nn31\\u_data\\ironman_empro\\c16_170_IRONMAN_635_20201211_115908"
#TOP_LEVEL_DATA_DIRECTORY = paste0(dir, "c16_170_IRONMAN_635_20201211_115908")
TOP_LEVEL_DATA_DIRECTORY = paste0(dir, "c16_170_IRONMAN_680_20210201_180923")

# Load All SAS Medidata
sasdfs <- load_sas_medidata(
  TOP_LEVEL_DATA_DIRECTORY
)

# Enumerate number of patients
number_of_patients <- sum(sasdfs$cohort %>%
  group_by(subjectId) %>%
  tally() %>% pull(n))


# Pull out fields from COHORT table
cohort_data <- sasdfs$cohort %>%
  select("projectid", "studyid", "subjectId", "StudySiteId", "Subject", "siteid", "Site", "COHORT", "COHORT_STD")

# Pull out fields from CONSENT table
consent_data <- sasdfs$ic %>%
  select("subjectId", "CNSTDATE") %>%
  distinct()

# Pull out fields from MHDIAGBX table
diagnosis_data <- sasdfs$mhdiagbx %>%
  select("subjectId", "DIAGBX_DATE") %>%
  distinct()

# merge three database files
# note - there are 21 patients who have been consented but have no other info entered yet
database <- cohort_data %>% 
  left_join(consent_data,by='subjectId') %>% 
  left_join(diagnosis_data,by='subjectId')

#####################################################

# read in excel file with country 
#fname <- paste0(dir,"EMPRO sub-study site list and stratification factors.xlsx")
#sheetname <- "Site List"
#sites <- read_xlsx(fname, sheet=sheetname, col_types = c("text","text","text","text"), range="A1:D73")
  
# read in site list
sites<-read_xlsx(path="H:\\GU\\IRONMAN\\EMPRO randomization\\EMPRO_Site List for randomization_April2021_clean.xlsx", 
                 sheet="Sheet1", 
                 col_types = c("text","text","text","numeric","date", "text", "numeric", "numeric", "text", "text", "text"))


# new variable for site that matches the name in the database
sort(unique(sasdfs$cohort$Site)) # list of all sites in the database (compare against Excel spreadsheet)
sites2 <- sites %>%
  mutate(sitenew=case_when(`Site Name`=="Cross Cancer Institute" ~ "Cross Cancer Institute (Alberta Health Services)",
                           `Site Name`=="Guys and St. Thomas NHS Foundation Trust" ~ "Guy’s and St Thomas’ NHS Foundation Trust",
                           `Site Name`=="Royal Brisbane & Women's Hospital" ~ "Royal Brisbane & Women’s Hospital",
                           `Site Name`=="St. Vincent's Hospital Sydney" ~ "St. Vincent’s Hospital Sydney"
                           )) %>%
  mutate(sitenew=ifelse(is.na(sitenew),`Site Name`,sitenew)) %>% 
# grouping for sites that have subsites
  mutate(sitegp=case_when(`Site Name`=="Princess Alexandra Hospital" ~ "Queensland University of Technology",
                          `Site Name`=="Royal Brisbane & Women's Hospital" ~ "Queensland University of Technology")) %>%
  mutate(sitegp=ifelse(is.na(sitegp),`Site Name`,sitegp))


# merge site info from Excel spreadsheet into data - only keep sites in Excel sheet 
database_elig <- database %>% 
  right_join(sites2, by=c("Site"="sitenew"), copy=TRUE)


# calculate time from diagnosis to enrollment in months
database_elig$CNSTDATE=as.Date(database_elig$CNSTDATE)
database_elig$DIAGBX_DATE=as.Date(database_elig$DIAGBX_DATE)
database_elig$timetodx = difftime(database_elig$CNSTDATE, database_elig$DIAGBX_DATE, unit="days")/365*12

    # What should we do about patients with missing diagnosis date??

# export data 
save(database_elig,file=paste0(dir,"IRONMAN_data_for_rand.RData"))


