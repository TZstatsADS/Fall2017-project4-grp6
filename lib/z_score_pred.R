
####################################### Z-score prediction #######################################


# weight with neibors
# for user 1
#u <- bnnbors[[1]]
#u

#x<- rep(0,ncol(train1))
#for(i in u){
#  vec = var_rating[i,] * spearman_matrix1[1,i]
#  x <- x + vec
#}


#source("./lib/selecting_neighbors.R")

neighbor_threshold_spearman2 <- read.csv("./output/thresh2_spearman.csv", header = T)
neighbor_threshold_spearman2<-neighbor_threshold_spearman2[,-1]

mean_rating <- apply(matrix,1,mean)
sd_rating <- apply(matrix,1,sd)
var_rating <- (matrix - mean_rating)/sd_rating

#### predict with correlation-thresholding 

#nnn = neighbor_threshold_spearman2
#new = neighbor_threshold_spearman2[1:5,1:5]
#new



prediction_threshold <- function(dataset, weight, neighbor_matrix){
  
  mean_rating <- mean_rating(dataset)
  sd_rating <- sd_rating(dataset)
  var_rating <- var_rating(dataset)
  
  neighbor <- neighbor_matrix
  
  pred<-c()
  for(a in 1:nrow(dataset)){
    u <- as.vector(neighbor[a,])
    x<- rep(0,ncol(dataset))
    sum <- 0
    
    for(i in u){
      vec = var_rating[i,] * weight[1,i]
      x <- x + vec
      count <- weight[1,i]
      sum = sum + count
    }
    #print(a)
    prediction <- mean_rating[a] + sd_rating[a] * x / sum
    pred <- rbind(pred, prediction)
  }
}






c<-c(1,1,NA)
mean(c,na.rm = T)

neighbor

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


#### predict with best n neighbors

prediction_bnn <- function(dataset, weight){
  var_rating <- var_rating(dataset)
  mean_rating <- mean_rating(dataset)
  sd_rating <- sd_rating(dataset)
  
  neighbor <- bestnn(weight, 0.3) # with threshold = 0.3
  
  pred<-c()
  for(a in 1:nrow(dataset)){
    u <- neighbor[[a]]
    x<- rep(0,ncol(dataset))
    sum <- 0
    
    for(i in u){
      vec = var_rating[i,] * weight[1,i]
      x <- x + vec
      count <- weight[1,i]
      sum = sum + count
    }
    #print(a)
    prediction <- mean_rating[a] + sd_rating[a] * x / sum
    pred <- rbind(pred, prediction)
  }
}


#pred<-c()
#for(a in 1:nrow(train1)){
#  u <- bnnbors[[a]]
#  x<- rep(0,ncol(train1))
#  sum <- 0
#  for(i in u){
#    vec = var_rating[i,] * spearman_matrix1[1,i]
#    x <- x + vec
#    count <- spearman_matrix1[1,i]
#    sum = sum + count
#  }
#  print(a)
#  prediction <- mean_rating[a] + sd_rating[a] * x / sum
#  pred <- rbind(pred, prediction)
#}