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
dir<-'H:\\GU\\IRONMAN\\Data\\2020-12-14\\'

#TOP_LEVEL_DATA_DIRECTORY = "C:\\Users\\nn31\\u_data\\ironman_empro\\c16_170_IRONMAN_635_20201211_115908"
TOP_LEVEL_DATA_DIRECTORY = paste0(dir, "c16_170_IRONMAN_635_20201211_115908")

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
fname <- paste0(dir,"EMPRO sub-study site list and stratification factors.xlsx")
sheetname <- "Site List"
sites <- read_xlsx(fname, sheet=sheetname, col_types = c("text","text","text","text"), range="A1:D73")
  
# new variable for site that matches the name in the database
sort(unique(sasdfs$cohort$Site)) # list of all sites in the database (compare against Excel spreadsheet)
sites2 <- sites %>%
  mutate(sitenew=case_when(`Site Name`=="Chesapeake Urology Associates" ~ "Chesapeake Urology Research Associates",
                           `Site Name`=="CHU de Quebec" ~ "CHU de Québec-Université Laval",
                           `Site Name`=="Columbia University Medical Center" ~ "Columbia University", 
                           `Site Name`=="Cross Cancer Institute" ~ "Cross Cancer Institute (Alberta Health Services)",
                           `Site Name`=="Doylestown Hospital (TJU affiliate)" ~ "Doylestown Health",
                           `Site Name`=="Guys St. Thomas NHS Foundation Trust" ~ "Guy’s and St Thomas’ NHS Foundation Trust",
                           `Site Name`=="Brooklyn Methodist Hospital (Weill Cornell Sub-site)" ~ 
                              "NewYork–Presbyterian Brooklyn Methodist Hospital",
                           `Site Name`=="Oregon Health & Science University" ~ "Oregon Health and Sciences Cancer Center",
                           `Site Name`=="QUT - Princess Alexandra Hospital" ~ "Princess Alexandra Hospital",
                           `Site Name`=="Reading Health System (TJU sub-site)" ~ "Reading Health System",
                           `Site Name`=="QUT - Redland Hospital" ~ "Redland Hospital",
                           `Site Name`=="Robert H. Lurie Cancer Center at Northwestern University" ~ 
                              "Robert H. Lurie Comprehensive Cancer Center Northwestern University",
                           `Site Name`=="QUT - Royal Brisbane & Women's Hospital" ~ "Royal Brisbane & Women’s Hospital",
                           `Site Name`=="St. Vincent's Hospital Sydney" ~ "St. Vincent’s Hospital Sydney",
                           `Site Name`=="St. Vincent's University Hospital" ~ "St. Vincent’s University Hospital",
                           `Site Name`=="Tallaght University Hospital" ~ "Tallaght University Hospital`",
                           `Site Name`=="Sidney Kimmel Cancer Center at Johns Hopkins University" ~ 
                              "Sidney Kimmel Comprehensive Cancer Center",
                           `Site Name`=="University of California-Los Angeles" ~ "University of California Los Angeles",
                           `Site Name`=="University of California-San Diego" ~ "University of California San Diego",
                           `Site Name`=="Winship Cancer Institute Emory" ~ "Winship Cancer Institute Emory University")) %>%
  mutate(sitenew=ifelse(is.na(sitenew),`Site Name`,sitenew)) %>% 
# grouping for sites that have subsites
  mutate(sitegp=case_when(`Site Name`=="QUT - Princess Alexandra Hospital" ~ "Queensland University of Technology Sites",
                          `Site Name`=="QUT - Royal Brisbane & Women's Hospital" ~ "Queensland University of Technology Sites",
                          `Site Name`=="QUT - Redland Hospital" ~ "Queensland University of Technology Sites",
                          `Site Name`=="Delnor Cancer Center" ~ "Northwestern Medicine Regional Cancer Centers",
                          `Site Name`=="Kishwaukee Cancer Center" ~ "Northwestern Medicine Regional Cancer Centers",
                          `Site Name`=="Warrenville Cancer Center" ~ "Northwestern Medicine Regional Cancer Centers",
                          `Site Name`=="Doylestown Hospital (TJU affiliate)" ~ "Thomas Jefferson University",
                          `Site Name`=="Reading Health System (TJU sub-site)" ~ "Thomas Jefferson University",
                          `Site Name`=="Aria Health (TJU affiliate)" ~ "Thomas Jefferson University",
                          `Site Name`=="Easton Hospital (TJU affiliate)" ~ "Thomas Jefferson University"))%>%
  mutate(sitegp=ifelse(is.na(sitegp),`Site Name`,sitegp))


# merge site info from Excel spreadsheet into data
database2 <- database %>% 
  left_join(sites2, by=c("Site"="sitenew"), copy=TRUE)

# select only English speaking sites (sites in Excel file)
database_elig <- subset(database2,!is.na(sitegp))

# calculate time from diagnosis to enrollment in months
database_elig$CNSTDATE=as.Date(database_elig$CNSTDATE)
database_elig$DIAGBX_DATE=as.Date(database_elig$DIAGBX_DATE)
database_elig$timetodx = difftime(database_elig$CNSTDATE, database_elig$DIAGBX_DATE, unit="days")/365*12

    # What should we do about patients with missing diagnosis date??

# export data 
save(database_elig,file=paste0(dir,"IRONMAN_data_for_rand.RData"))


