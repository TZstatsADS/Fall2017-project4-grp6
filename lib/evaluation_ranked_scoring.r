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

randked_scoring <- function(pred_matrix, observed_matrix){
  nrow = nrow(pred_matrix)
  ncol = ncol(pred_matrix)
  a_minus_one = nrow/2 - 1
  
  # rank/sort pred_matrix by vote value. Assuming d=0 here.
  pred_matrix[pred_matrix<0] = 0
  numerator_matrix = t(apply(pred_matrix, 1, sort,decreasing=T))
  
  # denominator of r_a
  denom_vec = 2^(0:(ncol-1)/a_minus_one)
  denom_mat = matrix(rep(denom_vec, nrow), nrow, ncol, byrow=T)

  # get a vector of r_a
  utility_matrix = numerator_matrix/denom_mat
  r_a_vector = rowSums(utility_matrix)
  
  # get a r_a_max value
  denom_mat2 = matrix(rep(denom_vec, nrow(observed_matrix)), 
                      nrow(observed_matrix), ncol, byrow=T)
  max_numerator_matrix = t(apply(observed_matrix, 1, sort,decreasing=T))
  max_utility_matrix = max_numerator_matrix/denom_mat2
  max_r_a_vector = rowSums(max_utility_matrix)
  
  # Get the r_a / r_a_max score
  r = 100 * sum(r_a_vector)/sum(max_r_a_vector)
  
  return(r)
}

### for testing:
m <- 4
n <- 3
observed_matrix = round(matrix(runif(m * n), m, n))
pred_matrix = matrix(runif(m * n), m, n)

nrow = nrow(pred_matrix)
ncol = ncol(pred_matrix)
a_minus_one = nrow/2 - 1


#randked_scoring(pred_matrix,observed_matrix)

########
# written by Shiqi:
randked_scoring_shiqi <- function(matrix){
  N = nrow(matrix)
  M = ncol(matrix)
  a_minus_one = nrow/2 - 1
  d = 0
  # rank/sort by vote value
  numerator_matrix = max(matrix-d,0)
  
  # denominator of r_a
  denom_vec = 2^(1:M/a_minus_one)
  denom_mat = matrix(rep(denom_vec, N), N, M, byrow=T)
  
  # get a vector of r_a
  utility_matrix = numerator_matrix*matrix/denom_mat
  r_a_vector = rowSums(utility_matrix)
  
  # get a r_a_max value
  max_utility_vec = numerator_matrix/denom_mat
  max_r_a = rowSums(max_utility_vec)
  
  # Get the r_a / r_a_max score
  r = 100 * sum(r_a_vector)/sum(max_r_a * ncol)
  
  return(r)
}