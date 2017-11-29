### Author: Shiqi Duan
### Date: "November 28, 2017"
library(matrixStats)
z_score<-function(df, w){
  #Input: Rating dataset with users in row and items in columns, Weight matrix between users
  #Output: Prediction matrix with users in row and items in columns
  N <- nrow(df)
  M <- ncol(df)
  
  df_mean <- matrix(rep(rowMeans(df), M), N, M)
  df_sd <- matrix(rep(rowSds(df), M), N, M) 
  w_sum <- matrix(rep(rowSums(w), M), N, M)
  df_scale <- (df - df_mean)/df_sd
  p <- df_sd * (t(df_scale) %*% w)/w_sum
  return(p + df_mean)
}