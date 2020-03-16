# prepare for presentation
# all code for analysis, seperate later
install.packages("foreign")
install.packages("epicalc")
library(foreign)
library(mdscore)
library(lmtest)
library(epicalc)
source("sim_function.R")                                                     #usage???

# read data
# extract used covariates from .dbf file
data_info <- read.dbf("test_from_July/Huc_info.dbf", as.is = FALSE)
data_name <- as.vector(c("TARGET_FID", "Join_Count", "Shape_Le_1", "Shape_Area", "drainage_d"))
data_extract <- data_info[,data_name]

# read distance file
# distance_info <- read.dbf("test_from_July/HUC_DIS.dbf", as.is = FALSE)
# delete the first column and sort the dataframe by huci and hucj numbers
# distance_info <- distance_info[order(distance_info$INPUT_FID, distance_info$NEAR_FID),-1]
distance_single <- read.csv('C:\\summer_project_final\\new_data\\distance.csv')

# initial the dataframe of covariates for all pairs
huc_pair_num <- nrow(distance_single)
huc_num <- nrow(data_extract)
cov_num <- ncol(data_extract) - 1
pair_covs <- data.frame(HUC_i=integer(), HUC_j=integer(), points_i=integer(), length_i=double(), area_i=double(),
                        points_j=integer(), length_j=double(), area_j=double(), distance_ij=double())
pair_covs_test <- data.frame(HUC_i=integer(), HUC_j=integer(), points_i=integer(), length_i=double(), area_i=double(), 
                        points_j=integer(), length_j=double(), area_j=double(), distance_ij=double())
# loop to get covariates for all pairs
# now using 50 for testing
for (k in 1:huc_pair_num) {
  print(k)
  i <- distance_single[k,1] 
  j <- distance_single[k,2]
  distance_ij <- distance_single[k,3]
  pair_covs[k,] <- c(i, j, data_extract[i+1, "Join_Count"]+1, data_extract[i+1, "Shape_Le_1"], data_extract[i+1, "Shape_Area"],
                           data_extract[j+1, "Join_Count"]+1, data_extract[j+1, "Shape_Le_1"], data_extract[j+1, "Shape_Area"], 
                           distance_ij)
}

write.csv(pair_covs_test,'C:\\summer_project_final\\new_data\\pair_covs_test.csv',row.names = FALSE)
write.csv(pair_covs,'C:\\summer_project_final\\new_data\\pair_covs.csv',row.names = FALSE)

pair_num_test <- nrow(pair_covs_test)

# Set a parameter
sim_parameter <- c(0.3, 0.3, 0.1, 0.15, 0.3, 0.1, 0.15, -1)

# leave only covariates
sim_covs <- pair_covs[,-(1:2)]

# generate true n_ij by poisson
poi_sim_data <- generate_data_poisson(sim_covs, sim_parameter)
nij_list <- poi_sim_data[,1]
total_ang_num <- sum(nij_list)

observe_nij <- poi_sim_data
observe_nij[,1] <- 0


# generate true n_ij by negBin
alpha <- 0.5
nb_sim_data <- generate_data_nb(sim_covs, sim_parameter, alpha)
# (nb_sim_data)
nb_nij_list <- nb_sim_data[,1]
nb_total <- sum(nb_nij_list)

nb_observe_nij <- nb_sim_data
nb_observe_nij[,1] <- 0

#### direct test
poi_res_from_poi <- glm_poisson(nb_observe_nij)
nb_res_from_poi <- glm_nb(nb_observe_nij)
fit1 <- glm.convert(nb_res_from_poi)
summary(poi_res_from_poi)
summary(nb_res_from_poi)

test_res <- lrtest(poi_res_from_poi,nb_res_from_poi)
test_res

conf <- confint(poi_res_from_poi)
conf


nb_res_from_nb <- glm_nb(nb_sim_data)
summary(nb_res_from_nb)
#### test end



# total_ang_seq <- c(1)
# for (ob in 2:total_ang_num) {
#   
#   total_ang_seq <- c(total_ang_seq,type)
# }
# 
# observe_seq <- sample(1:total_ang_num, total_ang_num, )

# total_ang_seq <- NULL
# for (ij in 1:length(nij_list)) {
#   for (ob in 1:nij_list[ij]) {
#     total_ang_seq <- c(total_ang_seq, ij)
#   }
# }

# total number of angler: 284000
for (k in 1:20000) {
  index <- sample(1:huc_pair_num, 1, prob = nij_list)
  
  nij_list[index] <- nij_list[index] - 1
  observe_nij[index,1] <- observe_nij[index,1] + 1
  nb_observe_nij[index,1] <- nb_observe_nij[index,1] + 1
}



for (n in 1:total_ang_num) {
  index <- sample(1:huc_pair_num, 1, prob = nij_list)
  
  nij_list[index] <- nij_list[index] - 1
  observe_nij[index,1] <- observe_nij[index,1] + 1
  if(n<20000){
    #print(n)
    next
    
  }
  
  total_ob <- sum(observe_nij[,1])
  p_value <- total_ob/total_ang_num
  
  fit_nij <- observe_nij
  fit_nij[,1] <- round(fit_nij[,1]/p_value)
  # generate the matrix of observed data with covariates, and use for poisson glm 
  poi_res <- try(glm_poisson(fit_nij))
  #summary(poi_res)
  
  conf <- confint(poi_res)  
  #summary(conf)
  increase_toggle <- 0
  conf_num <- nrow(conf)
  
  print(n)
  print(conf)
  
  for (i in 1:conf_num) {
    if ((conf[i,1]<0) && (conf[i,2]>0)) {
      increase_toggle <- 1
      break
    }
    if ((conf[i,1]>sim_parameter[i]) || (conf[i,2]<sim_parameter[i])) {
      increase_toggle <- 1
      break
    }
  }
  
  if (increase_toggle==0) {
    print(conf)
    print(n)
    break
  }
  
}




sury_pt_dist <- rep(0,huc_pair_num)
sury_eff_dist <- rep(0,huc_pair_num)
sury_pt_num <- 10
initial_sury_pt <- sample(1:length(nij_list), sury_pt_num)
for (p in 1:initial_sury_pt) {
  sury_pt_dist[initial_sury_pt[p]] = sury_pt_dist[initial_sury_pt[p]] + 1
}

while(1) {
  
  sury_eff_dist <- sury_pt_dist*40
  # the part of simulation for sampling process could be changed, now we set different prob for every pair of huc
  survey_prob <- rep(survey_prob, length(true_nij))

  # get the observed n_ij
  ob_nij <- sampling_process(true_nij, survey_prob, num_of_surveyor)

  # generate the matrix of observed data with covariates, and use for poisson glm
  ob_covs <- cbind(ob_nij,poi_sim_data[,-1])
  poi_res <- glm_poisson(ob_covs)

  # get the likelihood for
  # show poisson glm result
  summary(poi_res)

  # calculate the confidence interval for each parameter
  increase_toggle <- 0
  conf <- confint(poi_res)
  conf_num <- nrow(conf)
  for (i in 1:conf_num) {
    if ((conf[i][1]<sim_parameter[i])|(conf[i][2]>sim_parameter[i])){
      increase_toggle <- 1
    }
    if ((conf[i][1]>0)&(conf[i][2]<0)){
      increase_toggle <- 1
    }
    if (increase_toggle == 1)
      num_of_surveyo <- num_of_surveyo + 1
  }
}







