library(maptools)
library(foreign)
library(mdscore)
library(lmtest)
library(epicalc)
# example data pre processing
example_table <- read.dbf("77huc8/77huc8example.dbf")
example77_allinfo <- subset(data_info, OBJECTID %in% example_table$OBJECTID)
example77_distance <- subset(distance_single, INPUT_FID %in% example_table$OBJECTID & NEAR_FID %in% example_table$OBJECTID)
e77_paircov <- subset(pair_covs, HUC_i %in% example_table$OBJECTID & HUC_j %in% example_table$OBJECTID)

write.csv(example77_allinfo, "example77_allinfo.csv")
write.csv(example77_distance, "example77_distance.csv")
write.csv(e77_paircov, "e77_paircov.csv")

source("sim_function.R") 
drops <- c("HUC_i","HUC_j")
e77_pcov_noindex <- e77_paircov[ , !(names(e77_paircov) %in% drops)]

par_list <- c(0.3, 0.3, 0.1, 0.15, 0.3, 0.1, 0.15, -1)
alpha <- 0.5 

e77_poi_data <- generate_data_poisson(e77_pcov_noindex, par_list)
e77_nb_data <- generate_data_nb(e77_pcov_noindex, par_list, alpha)

# check validity
print(sum(e77_poi_data$n_ij))
# by survey method 1: mail survey
bin_p <- 0.02
ob1_poi_data <- e77_poi_data


