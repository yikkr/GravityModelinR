### functions for simulations  
## generate the true number of anglers travelling bewteen each pair of hucs  
# generate_data_poisson <- function(cov_data, parameter) {
#   stopifnot(ncol(cov_data) == length(parameter) - 1)
#   res_data <- cbind(n_ij = 0, cov_data)
#   res_data[,1] <- by(cov_data, 1:nrow(cov_data), function(row) rpois(1,exp(log(data.matrix(row)) %*% parameter[-1]+parameter[1])))
#   return(res_data)
# }
library(MASS)
generate_data_poisson <- function(cov_data, parameter) {
  stopifnot(ncol(cov_data) == length(parameter) - 1)

  res_data <- cbind(n_ij = 0, cov_data)
  for (i in 1:nrow(res_data)) {
    mu <- 0
    for (j in 1:ncol(cov_data)) {
      mu <- mu + parameter[j+1]*log(cov_data[i,j])
    }
    mu <- exp(mu +parameter[1])
    #print(mu)
    res_data[i,1] <- rpois(1, mu)
  }

  return(res_data)
}

## generate true number of anglers from
# input: 
generate_data_nb <- function(cov_data, parameter, alpha) {
  # check
  stopifnot(ncol(cov_data) == length(parameter) - 1)
  
  res_data <- cbind(n_ij = 0, cov_data)
  for (i in 1:nrow(res_data)) {
    mu <- 0
    for (j in 1:ncol(cov_data)) {
      mu <- mu + parameter[j+1]*log(cov_data[i,j])
    }
    mu <- exp(mu +parameter[1])
    #print(mu)
    res_data[i,1] <- rnegbin(1, mu, theta = alpha^(-1))
  }
  # res_data[,1] <- by(cov_data, 1:nrow(cov_data), function(row) rnegbin(data.matrix(row) %*% test_par[-1]+test_par[1]), theta = alpha^(-1))
  
  return(res_data)
}

## sampling based on binomial distribution
sampling_process <- function(true_n, prob, ob_num) {
  num_of_pairs <- length(true_n)
  stopifnot(length(prob) == num_of_pairs)
  ob_data <- rep(0, num_of_pairs)
  
  for (i in num_of_pairs) {
    ob_data[i] <- (sum(rbinom(ob_num, true_n[i], prob[i]))/ob_num)*prob[i]
  }
  return(ob_data)
  
}

glm_poisson <- function(all_pairs_info) {
  
  resp <- colnames(all_pairs_info)[1]
  cov <- colnames(all_pairs_info)[-1]
  
  all_pairs_info[,-1] <- log(all_pairs_info[,-1])
  # num_of_pairs <- nrow(all_pairs_info)
  # num_of_covariates <- length(cov)
  
  glm_fmla <- paste(paste(resp, "~"), paste(cov, collapse="+"))
  # print(glm_fmla)
  poi <- glm(glm_fmla, family = poisson(link = "log"), data = all_pairs_info)
  
  
  return(poi)
}

glm_nb <- function(all_pairs_info) {
  
  resp <- colnames(all_pairs_info)[1]
  cov <- colnames(all_pairs_info)[-1]
  
  all_pairs_info[,-1] <- log(all_pairs_info[,-1])
  # num_of_pairs <- nrow(all_pairs_info)
  # num_of_covariates <- length(cov)
  
  glm_fmla <- paste(paste(resp, "~"), paste(cov, collapse="+"))
  # print(glm_fmla)
  nb <- glm.nb(glm_fmla,  data = all_pairs_info)
  
  return(nb)
}

# first time 19186
 
# 
# # function to do the mle for poisson model
# # input: covar, matrix of covariates (row) of all pairs (column);
# #        n_ij, array of number of anglers travelling between pairs, with the same order as covar
# # return: array of optimized parameters and the value of maximum likelihood
# 
# 
# #
# # For test
# # n_ij <- c(54,49,45,46,44,41,51,53,43,46)
# test_cov <- matrix(1:30, nrow = 10, ncol = 3)
# test_cov[,1] = c(144,231,422,213,321,347,753,234,237,613)
# test_cov[,2] = c(332,765,435,534,533,554,421,675,342,654)
# test_cov[,3] = c(27,36,21,56,78,98,43,23,53,11)
# 
# # test_pre_data <- cbind(n_ij, test_cov)
# rownames(test_cov) <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10")
# colnames(test_cov) <- c( "z1", "z2", "z3")
# 
# # rownames(test_pre_data) <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10")
# # colnames(test_pre_data) <- c("n_ij", "z1", "z2", "z3")
# 
# test_parameter <- c(20, 0.8, -0.003, 1)
# 
# 
# test_cov_data <- data.frame(test_cov)
# 
# poisson_data <- generate_data_poisson(test_cov_data, test_parameter)
# 
# test_prob <- rep(0.3, 10)
# 
# test_ob <- sampling_process(poisson_data[,1], test_prob, 10)
# 
# k <- (glm_poisson(poisson_data))
# # rpois(1, data.matrix(test_cov[1,]) %*% test_parameter[-1]+test_parameter[1])
# 
# # Power analysis, in the analysis we have 
# # To decide how many surveyors we need



