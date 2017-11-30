#######################################
# Evaluation for dataset1 - Ranked scroing functions
# Saaya Yasuda (sy2569)
#######################################

#rm(list=ls())
#setwd('~/Documents/Github/fall2017-project4-fall2017-proj4-grp6')

#######################################
# Rank matrix function (helper function)
# Input: predicted matrix & test set matrix.
#        These matrices need to have user names in rownames.
# Output: return the ranked test set matrix, based on predicted vote values.
#######################################

rank_matrix <- function(pred_matrix, observed_matrix){
  nrow = nrow(observed_matrix)
  ncol = ncol(observed_matrix)
  ranked_mat = matrix(NA, nrow, ncol)
  
  for (r in 1:nrow){
    # get username of the row
    user_name = rownames(observed_matrix)[r] 
    
    # sort pred values
    sorted_pred = sort(pred_matrix[user_name,], decreasing=TRUE) 
    
    # sort observed values based on pred values.
    sorted_obs = unlist( observed_matrix[user_name,][names(sorted_pred)] )
    
    # save the ranked row in the new matrix.
    ranked_mat[r,] = unname(sorted_obs)
  }
  rownames(ranked_mat) = rownames(observed_matrix)
  return(ranked_mat)
}

#######################################
# Ranked scroing function
# Input: predicted matrix & test set matrix.
#        These matrices need to have user names in rownames.
# Output: return the ranked score for the test set matrix
#######################################

randked_scoring <- function(pred_matrix, observed_matrix, alpha){
  # Get ranked version of the observed_matrix
  ranked_mat = rank_matrix(pred_matrix, observed_matrix)
  
  nrow = nrow(ranked_mat)
  ncol = ncol(ranked_mat)
  a_minus_one = alpha-1
  
  # Assuming d=0, this is the numerator for r_a. 
  # There's no negative value in dataset 1, but just in case.
  ranked_mat[ranked_mat<0] = 0
  
  # denominator of r_a & r_a_max
  denom_vec = 2^(0:(ncol-1)/a_minus_one)
  denom_mat = matrix(rep(denom_vec, nrow), nrow, ncol, byrow=T)
  
  # Get a vector of r_a
  utility_matrix = ranked_mat/denom_mat
  r_a_vector = rowSums(utility_matrix)
  
  # Rank the observed_matrix for r_a max in order to have the maximum achievable utility.
  max_numerator_matrix = t(apply(observed_matrix, 1, sort,decreasing=T))
  
  # Get a vector of r_a max
  max_utility_matrix = max_numerator_matrix/denom_mat
  max_r_a_vector = rowSums(max_utility_matrix)
  
  # Get the r_a / r_a_max score
  r = 100 * sum(r_a_vector)/sum(max_r_a_vector)
  
  return(r)
}

