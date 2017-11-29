#######################################
# Evaluation for dataset1 - Ranked scroing function
# Saaya Yasuda (sy2569)
#######################################

#rm(list=ls())
#setwd('~/Documents/Github/fall2017-project4-fall2017-proj4-grp6')

#######################################
# Ranked scroing function
# It returns a score up to 100
#######################################

# for testing:
#m <- 4
#n <- 3
#matrix = round(matrix(runif(m * n), m, n))

randked_scoring <- function(matrix){
  nrow = nrow(matrix)
  ncol = ncol(matrix)
  a_minus_one = nrow/2 - 1
  
  # rank/sort by vote value
  numerator_matrix = t(apply(matrix, 1, sort,decreasing=T))
  
  # denominator of r_a
  denom_vector = rep(NA, ncol)
  for (j in 1:ncol){
    denom_vector[j] <- 2^((j-1)/a_minus_one)
  }
  
  # get a vector of r_a
  utility_matrix = numerator_matrix %*% diag(1 / denom_vector)
  r_a_vector = rowSums(utility_matrix)
  
  # get a r_a_max value
  max_numerator_vec = rep(1, ncol)
  max_utility_vec = max_numerator_vec %*% diag(1/denom_vector)
  max_r_a = rowSums(max_utility_vec)
  
  # Get the r_a / r_a_max score
  r = 100 * sum(r_a_vector)/sum(max_r_a * ncol)
  
  return(r)
}

#randked_scoring(matrix)
