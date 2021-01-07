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
dir <- 'C:\\Users\\nn31\\u_data\\ironman_empro\\'

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
    CRPC = sum(COHORT=="CRPC")/n())

# code country as numeric 
database_site$CountryN = factor(database_site$Country,
                                levels = c("Australia", "Canada", "Ireland", "U.K.", "U.S."),
                                labels = c(1, 2, 3, 4, 5))

# number of sites in each country
table(database_site$Country)
# exclude Ireland - only 3 sites
database_site4 <- database_site %>%
  filter(Country != "Ireland")

# test dataset
database_UK <- database_site4 %>%
  filter(Country == "U.K.")

####################################

# try constrained randomization
### There is some non-working code in cvrall that inadvertantly creates a character list
### This occurs on line 100 and is:  x <- as.matrix(x)
### This line should be changed to: x2 <- as.matrix(sapply(x, as.numeric))
### Because this code is not on github, I cannot make a pull request, therefore, the recommended path is to one hot
### encode the dataframe yourself, for example

# 4 countries, 51 sites (Excluding Ireland which only has 3 sites)
inputdf <- data.frame(model.matrix(~-1+Country+timetodx+CRPC, database_site4))

Design_result <- cvrall(clustername = database_site4$sitegp,
                        balancemetric = "l2",
                        #x = data.frame(database_site4[,c("Country", "timetodx", "CRPC")]),
                        x = inputdf,
                        ntotal_cluster = 51,
                        ntrt_cluster = 25,
                        categorical = c("CountryAustralia", "CountryCanada", "CountryU.K.", "CountryU.S."),
                       # savedata=paste0(dir,"site_rand.csv"),
                        cutoff = 0.1,
                        seed=12345,
                        check_validity = TRUE)

# 5 countries, 54 site (with Ireland)
inputdf5 <- data.frame(model.matrix(~-1+Country+timetodx+CRPC, database_site))

Design_result5 <- cvrall(clustername = database_site$sitegp,
                        balancemetric = "l2",
                        #x = data.frame(database_site4[,c("Country", "timetodx", "CRPC")]),
                        x = inputdf5,
                        ntotal_cluster = 54,
                        ntrt_cluster = 27,
                        categorical = c("CountryAustralia", "CountryCanada", "CountryIreland", "CountryU.K.", "CountryU.S."),
                        # savedata=paste0(dir,"site_rand.csv"),
                        cutoff = 0.1,
                        seed=12345,
                        check_validity = TRUE)
# look at output
Design_result5$baseline_table
Design_result5$allocation
# merge allocation back into data set
database_site_rand <- database_site %>% 
  left_join(Design_result5$allocation,by=c("sitegp"="clustername"))
# sum N by arm
database_site_rand %>%
  group_by(allocation) %>%
  summarise(total=sum(n))

# 1 country, 10 sites (UK only, for testing purposes)
inputdf_UK <- data.frame(model.matrix(~-1+timetodx+CRPC, database_UK))
Design_resultUK <- cvrall(clustername = database_UK$sitegp,
                        balancemetric = "l2",
                        #x = data.frame(database_UK[,c("timetodx", "CRPC")]),
                        x = inputdf_UK,
                        ntotal_cluster = 10,
                        ntrt_cluster = 5,
                        # savedata=paste0(dir,"site_rand.csv"),
                        cutoff = 0.1,
                        seed=12345,
                        check_validity = TRUE)



# stratified constrained randomization
# constrained on country
inputdf5 <- data.frame(model.matrix(~-1+Country+timetodx+CRPC, database_site))

Design_result_strat <- cvrall(clustername = database_site$sitegp,
                         balancemetric = "l2",
                         #x = data.frame(database_site4[,c("Country", "timetodx", "CRPC")]),
                         x = inputdf5,
                         ntotal_cluster = 54,
                         ntrt_cluster = 27,
                         categorical = c("CountryAustralia", "CountryCanada", "CountryIreland", "CountryU.K.", "CountryU.S."),
                         # savedata=paste0(dir,"site_rand.csv"),
                         cutoff = 0.1,
                         stratify = c("CountryAustralia", "CountryCanada", "CountryIreland", "CountryU.K.", "CountryU.S."),
                         seed=12345,
                         check_validity = TRUE)
# show output
Design_result_strat$baseline_table
# merge allocation back into data set
database_site_strat <- database_site %>% 
  left_join(Design_result_strat$allocation,by=c("sitegp"="clustername"))
# sum N by arm
database_site_strat %>%
  group_by(allocation) %>%
  summarise(total=sum(n))

# covariate constrained randomization
