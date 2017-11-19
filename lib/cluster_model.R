setwd("/Users/duanshiqi/Documents/GitHub/fall2017-project4-fall2017-proj4-grp6/data/")
train <- read.csv("../output/dataset2_train.csv",header=T)
test <- read.csv("../output/dataset2_test.csv",header=T)

rownames(train) <- train[,1]
train <- train[,-1]

M <- ncol(train)
N <- nrow(train)
movie <- colnames(train)
user <- rownames(train)

### function for cluster models
cluster_model <- function(df, C, tau){
  
  #Input: dataframe to train, number of classes, threshold to determine convergence
  #Output: parameters for cluster models: 
  #        mu: probability of belonging to class c
  #        gamma: probability of scores for a movie given the class 
  
  ## Step1: initialize the parameters
  mu <- rep(1/C, C)
  gamma <- list() #each list represents a class
                  #the ijTh element means the probability of rating jth movie with score i in that class 
  for(i in 1:C){
    gamma[[i]] <- data.frame(matrix(1/6, 6, M))
    colnames(gamma[[i]]) <- movie
    rownames(gamma[[i]]) <- 0:5
  }
  
  ## Step2: Expectation
  expect <- matrix(0, C, N) # expectation with rows meaning classes and columns meaning users
  for(i in 1:C){
    for(j in 1:N){
      rated <- (!is.na(df[j, ]))
      score <- df[j, rated] # find the movies that jth user has rated and the scores of those movies
      rated_gamma <- gamma[[i]][, rated] #find the gammas for the rated movies of jth user given class i
      phi <- 1
      for(k in 1:ncol(rated_gamma)){
        phi <- phi*rated_gamma[score[k]+1, k]
      }
      expect[i,j] <- mu[i]*phi
    }
  }
  expect <- expect/colSums(expect)
  
  ## Iterations based on the stop criterion
  threshold <- 9999
  while(threshold >= tau){
    
    expect <- expect_new  #update expectation
    
    ## Step3: Maximization
    mu <- rowSums(expect)/N  #update parameters
    for(k in 1:C){
      for(i in 1:6){
        for(j in 1:M){
          rated_user <- (!is.na(df[, j])) # user who has rated movie j
          rated_expect <- expect[k, rated_user] # expectation of rated_user in class k
          is_i <- (df[rated_user, j] == i-1) # indicator of whether rated_user's score for movie j is i-1
          gamma[[k]][i, j] <- sum(rated_expect[is_i])/sum(rated_expect) # updated gamma
        }
      }
    }
    
    ## Check convergence
    expect_new <- matrix(0, C, N) 
    for(i in 1:C){
      for(j in 1:N){
        rated <- (!is.na(df[j, ]))
        score <- df[j, rated] 
        rated_gamma <- gamma[[i]][, rated] 
        phi <- 1
        for(k in 1:ncol(rated_gamma)){
          phi <- phi*rated_gamma[score[k]+1, k]
        }
        expect_new[i,j] <- mu[i]*phi
      }
    }
    expect_new <- expect/colSums(expect)
    
    threshold <- norm(expect_new - expect, "O") # matrix 1-norm of the difference between updated and old expectation
  }
  return(list(mu = mu, gamma = gamma))
}