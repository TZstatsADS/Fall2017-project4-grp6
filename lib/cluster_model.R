### Author: Shiqi Duan
### Date: "November 19, 2017"

  
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
  
  mu_new <- mu
  gamma_new <- gamma
  expect <- matrix(0, C, N) # expectation with rows meaning classes and columns meaning users
  
  ## Iterations based on the stop criterion
  threshold1 <- 9999
  threshold2 <- 9999
  count <- 0
  while(!(threshold1 <= tau & threshold2 <= tau) ){
    count <- count + 1
    print(paste0("iteration = ", count))
    mu <- mu_new
    gamma <- gamma_new
    
    ## Step2: Expectation
    for(j in 1:N){
      for(i in 1:C){
        rated <- (!is.na(df[j, ]))
        score <- df[j, rated] # find the movies that jth user has rated and the scores of those movies
        rated_gamma <- gamma[[i]][, rated] #find the gammas for the rated movies of jth user given class i
        phi <- 6^300
        for(k in 1:ncol(rated_gamma)){
          phi <- phi*rated_gamma[score[1,k]+1, k]
        }
        expect[i,j] <- mu[i]*phi
      }
      col_sum <- sum(expect[,j])
      expect[,j] <- ifelse(col_sum == 0, rep(1/C, C), expect[,j]/col_sum)
    }
    
    ## Step3: Maximization
    mu_new <- rowSums(expect)/N  #update mu
    
    for(k in 1:C){
      for(i in 1:6){
        for(j in 1:M){
          rated_user <- (!is.na(df[, j])) # user who has rated movie j
          if(sum(rated_user) != 0){
            rated_expect <- expect[k, rated_user] # expectation of rated_user in class k
            is_i <- (df[rated_user, j] == i-1) # indicator of whether rated_user's score for movie j is i-1
            gamma_new[[k]][i, j] <- sum(rated_expect[is_i])/sum(rated_expect) # updated gamma
          }
        }
      }
    }
    
    ## Check convergence
    threshold1 <- mean(abs(mu_new - mu)) #mean absolute difference of mu
    threshold2 <- 0
    for(c in 1:C){
      threshold2 <- max(threshold2,norm(as.matrix(gamma_new[[c]] - gamma[[c]]), "O"))# matrix 1-norm of the difference between updated and old gamma
    }
    print(paste0("threshold1 = ", threshold1, " threshold2 = ", threshold2))
    
  }
  return(list(mu = mu, gamma = gamma))
}


## estimate scores based on the parameterd obtained by cluster models
score_estimation_CM <- function(df, ui, mb, par){
  
  ###Input: dataframe of training set, index of user, index of movie, parameter list
  ###Output: estimated score
  
  mu <- par$mu
  gamma <- par$gamma
  C <- length(gamma)
  
  ##probability by Naive Bayes formula
  prob <- rep(0, 5)
  
  obs_movie <- (!is.na(df[ui, ])) #movies rated by the user
  obs_score <- df[ui, obs_movie]  #scores of movies rated by the user
  
  for(k in 1:5){
    
    obs <- mu #probability of observed scores
    prob_all <- rep(1, C) 
    
    for(c in 1:C){
      
      obs_gamma <- gamma[[c]][, obs_movie] #gamma of rated movies
      
      for(j in 1:sum(obs_movie)){
        obs[c] <- obs[c] * obs_gamma[obs_score[j]+1, j]
      }
      
      prob_mb <- gamma[[c]][k+1, mb] #probability of bth movie is rated with score k given class c
      prob_all[c] <- obs[c] * prob_mb
    }
    prob[k] <- sum(prob_all)/sum(obs)
  }
  return(1*prob[1]+2*prob[2]+3*prob[3]+4*prob[4]+5*prob[5])
}


### 5-fold cross-validation to find best class number C
set.seed(2)
K <- 5
n <- ncol(train)
n.fold <- floor(n/K)
s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
cv_error <- rep(NA, K)
c_list <- c(2,3,6,12)
for(c in c_list){
  for(k in 1:K){
    train_df <- data.frame(matrix(NA, N, M))
    colnames(train_df) <- movie
    rownames(train_df) <- user
    train_df[, s!=k] <- train[, s!=k]
  
    test_df <- data.frame(matrix(NA, N, M))
    colnames(test_df) <- movie
    rownames(test_df) <- user
    test_df[,s == k] <- train[ ,s == k]
  
    estimate_df <- test_df
  
    cm_par <- cluster_model(df = train_df, C = c, tau = 0.1)
    
    for(i in 1:N){
      for(j in 1:M){
        if(!is.na(test_df[i,j])){
          estimate_df[i,j] <- score_estimation_CM(df = train_df, ui = i, mb = j, par = cm_par)
        }
      }
    }
    cv_error[k] <- mean(abs(estimate_df-test_df),na.rm = T)
  }
}
