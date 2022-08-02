#############################
# Test cvcrand package with example data
#############################

# load package
library(tidyverse)
library(cvcrand)


Design_result <- cvrall(clustername = Dickinson_design$county,
                        balancemetric = "l2",
                        x = data.frame(Dickinson_design[ , c("location", "inciis",
                                                             "uptodateonimmunizations", "hispanic", "incomecat")]),
                        ntotal_cluster = 16,
                        ntrt_cluster = 8,
                        categorical = c("location", "incomecat"),
                        ###### Option to save the constrained space #####
                        # savedata = "dickinson_constrained.csv",
                        bhist = TRUE,
                        cutoff = 0.1,
                        seed = 12345)