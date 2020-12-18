source('ironman_github/utils/load_sas_medidata.R')

TOP_LEVEL_DATA_DIRECTORY = "C:\\Users\\nn31\\u_data\\ironman_empro\\c16_170_IRONMAN_635_20201211_115908"

# Load All SAS Medidata
sasdfs <- load_sas_medidata(
  TOP_LEVEL_DATA_DIRECTORY
)

# Enumerate number of patients
number_of_patients <- sum(sasdfs$cohort %>%
  group_by(subjectId) %>%
  tally() %>% pull(n))


### Why are there differences between cohort$Site and EMPRO sub-study site list and strat..xlsx


