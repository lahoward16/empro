##############################
# IRONMAN study
# clusterized randomization based on:
#   site
#   country (weigh more heavily)
#   time from diagnosis to enrollment
#   disease status (CRPC vs. mHSPC)
#
#   this code is for trying randomization with a small number of sites per country (2)
#   and uneven number of sites per country (3) to see how it works
##############################

# load package
library(tidyverse)
library(cvcrand)
library(purrr)

# Directory where data are stored
dir<-'H:\\GU\\IRONMAN\\Data\\2020-12-14\\'
#dir <- 'C:\\Users\\nn31\\u_data\\ironman_empro\\'

# load data
load(file=paste0(dir,"IRONMAN_data_for_rand.RData"))

# complete cases of data
database_complete <- database_elig %>%
  filter_at(vars(c(sitegp, Country, timetodx, COHORT)), all_vars(!is.na(.)))
# n=1082

#
sapply(database_complete, function(x) sum(is.na(x)))


# number of site groups
length(unique(database_complete$sitegp))
# 54 sites

# collapse data to one row per site
# note: COHORT_STD (1=mHSPC, 2=CRPC)
database_site <- database_complete %>%
  group_by(sitegp) %>%
  summarise(
    n=n(),
    Country = first(Country),
    timetodx = mean(timetodx),
    CRPC = 100*sum(COHORT=="CRPC")/n())

# code country as numeric 
database_site$CountryN = factor(database_site$Country,
                                levels = c("Australia", "Canada", "Ireland", "U.K.", "U.S."),
                                labels = c(1, 2, 3, 4, 5))

# number of sites in each country
table(database_site$Country)
# exclude Ireland - only 3 sites
database_site4 <- database_site %>%
  filter(Country != "Ireland")

# function for selecting sites given a seed and number of sites selected for Canada
makedata <- function(seed, nCanada){
  set.seed(seed)
  database_site4 %>%
    filter(n >= 7) %>%
    arrange(Country) %>%
    group_by(Country) %>%
    nest() %>%            
    ungroup() %>% 
    mutate(numsites = c(4, nCanada, 4, 4)) %>% 
    mutate(samp = map2(data, numsites, sample_n)) %>% 
    select(-data) %>%
    unnest(samp)
}

# subset sites that have at least 7 subjects enrolled 
# select 4 sites in UK, US, Australia and 3 sites in Canada
database_test1a <- makedata(123, 3)
database_test1b <- makedata(456, 3)
database_test1c <- makedata(789, 3)


# subset sites that have at least 7 subjects enrolled 
# select 4 sites in UK, US, Australia and 2 sites in Canada
database_test2a <- makedata(123, 2)
database_test2b <- makedata(456, 2)
database_test2c <- makedata(789, 2)

####################################

# stratified constrained randomization
# constrained on country - 3 sites in Canada

clustrand <- function(data, nCanada, trial){
  # create data frame
  inputdf1 <- data.frame(model.matrix(~-1+Country+timetodx+CRPC, data))
  # run randomization 
  Design_result_strat1 <- cvrall(clustername = data$sitegp,
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
  write.csv(subset(summary), file=paste0(dir,paste0("randsummary_", nCanada, "inCanada_", trial, ".csv")))
  
  ## merge allocation back into data set
  database_site_strat1 <- database_site %>% 
    left_join(Design_result_strat1$allocation,by=c("sitegp"="clustername"))
  
  # export randomization scheme 
  write.csv(subset(database_site_strat1, select=-CountryN), file=paste0(dir,paste0("randscheme_", nCanada, "inCanada_", trial, ".csv")))
  
  }

# 3 sites in Canada, 3 trials
clustrand(database_test1a, 3, 1)
clustrand(database_test1b, 3, 2)
clustrand(database_test1c, 3, 3)

# 2 sites in Canada, 3 trials
clustrand(database_test2a, 2, 1)
clustrand(database_test2b, 2, 2)
clustrand(database_test2c, 2, 3)



