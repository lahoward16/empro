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

# number of sites in each country
table(database_site$Country)
# exclude Ireland - only 3 sites
database_site4 <- database_site %>%
  filter(Country != "Ireland")

# test dataset
database_test <- database_site4 %>%
  filter(Country == "U.K.")
####################################

# try constrained randomization
### There is some non-working code in cvrall that inadvertantly creates a character list
### This occurs on line 100 and is:  x <- as.matrix(x)
### This line should be changed to: x2 <- as.matrix(sapply(x, as.numeric))
### Because this code is not on github, I cannot make a pull request, therefore, the recommended path is to one hot
### encode the dataframe yourself, for example

inputdf <- data.frame(model.matrix(~Country+timetodx+CRPC, database_site4))

Design_result <- cvrall(clustername = database_site4$sitegp,
                        balancemetric = "l2",
                        #x = data.frame(database_site4[,c("Country", "timetodx", "CRPC")]),
                        x = inputdf,
                        ntotal_cluster = 51,
                        ntrt_cluster = 25,
                        #categorical = c("Country"),
                       # savedata=paste0(dir,"site_rand.csv"),
                        cutoff = 0.1,
                        seed=12345,
                        check_validity = TRUE)

# test with subset of sites (UK sites only)
Design_result <- cvrall(clustername = database_test$sitegp,
                        balancemetric = "l2",
                        x = data.frame(database_test[,c("timetodx", "CRPC")]),
                        ntotal_cluster = 10,
                        ntrt_cluster = 5,
                        # savedata=paste0(dir,"site_rand.csv"),
                        cutoff = 0.1,
                        seed=12345,
                        check_validity = TRUE)



# stratified constrained randomization
# constrained on country
Design_stratified_result <- cvrall(clustername = database_site4$sitegp,
                        balancemetric = "l2",
                        x = data.frame(database_site4[,c("Country", "timetodx", "CRPC")]),
                        ntotal_cluster = 51,
                        ntrt_cluster = 25,
                        categorical = c("Country"),
                        # savedata=paste0(dir,"site_rand.csv"),
                        weights = c(1000, 1, 1),
                        cutoff = 0.1,
                        seed=12345)

# covariate constrained randomization
