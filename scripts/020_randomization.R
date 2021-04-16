##############################
# IRONMAN study
# clusterized randomization based on:
#   site
#   country (weigh more heavily)
#   time from diagnosis to enrollment
#   disease status (CRPC vs. mHSPC)
##############################

# load package
library(tidyverse)
library(cvcrand)

# Directory where data are stored
#dir<-'H:\\GU\\IRONMAN\\Data\\2020-12-14\\'
dir<-'H:\\GU\\IRONMAN\\Data\\2021-04-08\\'
#dir <- 'C:\\Users\\nn31\\u_data\\ironman_empro\\'

# load data
load(file=paste0(dir,"IRONMAN_data_for_rand.RData"))


# complete cases of data
database_complete <- database_elig %>%
  filter_at(vars(c(sitegp, Country, timetodx, COHORT)), all_vars(!is.na(.)))
# n=507

# show missing data
sapply(database_complete, function(x) sum(is.na(x)))


# number of site groups
length(unique(database_complete$sitegp))
# 15 sites

# collapse data to one row per site
  # note: COHORT_STD (1=mHSPC, 2=CRPC)
database_site <- database_complete %>%
  group_by(sitegp) %>%
  summarise(
    n=n(),
    Country = first(Country),
    timetodx = mean(timetodx),
    CRPC = sum(COHORT=="CRPC")/n())

# code country as numeric 
database_site$CountryN = factor(database_site$Country,
                                levels = c("Australia", "Canada", "U.K.", "U.S."),
                                labels = c(1, 2, 3, 4))

# randomly choose 2 out of 3 Canadian sites
set.seed(1016)
database_final <- database_site %>%
  arrange(Country) %>%
  group_by(Country) %>%
  nest() %>%            
  ungroup() %>% 
  mutate(numsites = c(4, 2, 4, 4)) %>% 
  mutate(samp = map2(data, numsites, sample_n)) %>% 
  select(-data) %>%
  unnest(samp)


####################################

# final randomization: stratified constrained randomization
# constrained on country

  # create data frame
  inputdf1 <- data.frame(model.matrix(~-1+Country+timetodx+CRPC, database_final))
  # run randomization 
  Design_result_strat1 <- cvrall(clustername = database_final$sitegp,
                                 balancemetric = "l2",
                                 x = inputdf1,
                                 ntotal_cluster = nrow(inputdf1),
                                 ntrt_cluster = ceiling(nrow(inputdf1)/2),
                                 categorical = c("CountryAustralia", "CountryCanada", "CountryU.K.", "CountryU.S."),
                                 cutoff = 0.1,
                                 stratify = c("CountryAustralia", "CountryCanada", "CountryU.K.", "CountryU.S."),
                                 bhist = TRUE,
                                 seed = 12345,
                                 check_validity = TRUE)
  
  ## merge allocation back into data set
  database_site_strat1 <- database_site %>% 
    left_join(Design_result_strat1$allocation,by=c("sitegp"="clustername"))
    
  # sum N by arm - extract column totals for patient count
  totaln <- database_site_strat1 %>%
    group_by(allocation) %>%
    summarise(total=sum(n)) %>%
    filter(!is.na(allocation)) 
  total <- pull(totaln, total)
  
  # summary of stratification
  summary <- rbind(Design_result_strat1$baseline_table, total)
  row.names(summary)[8]="# patients"
  
  # output summary to CSV
  write.csv(subset(summary), file=paste0(dir,paste0("randsummary_final.csv")))
  

  # export randomization scheme 
  write.csv(subset(database_site_strat1, select=-CountryN), file=paste0(dir,paste0("randscheme_final.csv")))
  

