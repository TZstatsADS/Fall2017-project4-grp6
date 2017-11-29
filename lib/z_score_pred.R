
####################################### Z-score prediction #######################################

# mean rating for each user
# <- apply(train1, 1, mean)
mean_rating<-function(matrix){
  mean_rating <- apply(matrix, 1, mean)
  return(mean_rating)
}

# sd for each user
#sd_rating <- apply(train1, 1, sd)

sd_rating <- function(matrix){
  sd_rating <- apply(matrix, 1, sd)
  return(sd_rating)
}

# z-score calculation part
#var_rating <- (train1 - mean_rating)/sd_rating

var_rating <- function(matrix){
  var_rating <- (matrix - mean_rating(matrix))/sd_rating(matrix)
  return(var_rating)
}


# weight with neibors
# for user 1
#u <- bnnbors[[1]]
#u

#x<- rep(0,ncol(train1))
#for(i in u){
#  vec = var_rating[i,] * spearman_matrix1[1,i]
#  x <- x + vec
#}


source("./lib/selecting_neighbors.R")



#### predict with correlation-thresholding 

prediction <- function(dataset, weight){
  var_rating <- var_rating(dataset)
  mean_rating <- mean_rating(dataset)
  sd_rating <- sd_rating(dataset)
  
  neighbor <- cor_threshold(weight, 0.3) # with threshold = 0.3
  
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



#### predict with best n neighbors

prediction <- function(dataset, weight){
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