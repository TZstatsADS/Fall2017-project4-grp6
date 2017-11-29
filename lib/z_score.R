### Author: Shiqi Duan
### Date: "November 28, 2017"

library(matrixStats)
z_score<-function(df, w, neighbor_matrix){
  #Input: Rating dataset with users in row and items in columns, Weight matrix between users
  #Output: Prediction matrix with users in row and items in columns
  N <- nrow(df)
  M <- ncol(df)
  
  df_mean <- matrix(rep(rowMeans(df), M), N, M)
  df_sd <- matrix(rep(rowSds(as.matrix(df)), M), N, M) 
  w_new <- neighbor_matrix * w
  w_sum <- matrix(rep(rowSums(w_new), M), N, M)
  df_scale <- (df - df_mean)/df_sd
  p <- df_sd * t((t(df_scale) %*% as.matrix(w_new)))/w_sum
  return(p + df_mean)
}


#pred_spearman <- z_score(train1, spearman_matrix1, neighbor_thresh_spearman)

