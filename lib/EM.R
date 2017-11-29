### Author: Shiqi Duan
### Date: "November 19, 2017"


setwd("/Users/duanshiqi/Documents/GitHub/fall2017-project4-fall2017-proj4-grp6/data/")
train <- read.csv("../output/dataset2_train.csv",header=T)
test <- read.csv("../output/dataset2_test.csv",header=T)

rownames(train) <- train[,1]
train <- train[,-1]
rownames(test) <- test[,1]
test <- test[,-1]

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
  set.seed(2)
  mu <- runif(C)
  mu <- mu/sum(mu)
  gamma <- array(NA,c(M,C,6)) #each list represents a class
  #the ijTh element means the probability of rating jth movie with score i in that class 
  for(c in 1:C){
    for(m in 1:M){
      gamma[m,c,] <- runif(6)
      gamma[m,c,] <- gamma[m,c,]/sum(gamma[m,c,])
    }
  }
  
  w <- array(0, c(M,N,7))
  for(k in 1:6){
    w[,,k] <- ifelse(t(df)==k, 1, 0)
    w[,,k] <- ifelse(is.na(w[,,k]),0,w[,,k])
    w[,,7] <- w[,,7] + w[,,k]
  }
  
  mu_new <- mu
  gamma_new <- gamma
  #expect <- matrix(0, N, C) # expectation with rows meaning classes and columns meaning users
  
  ## Iterations based on the stop criterion
  threshold1 <- 9999
  threshold2 <- 9999
  threshold1_new <- 0
  threshold2_new <- 0
  count <- 0
  while((threshold1 > tau | threshold2 >tau)&(abs(threshold1-threshold1_new) > tau | abs(threshold2-threshold2_new) > tau)){
    count <- count + 1
    print(paste0("iteration = ", count))
    
    threshold1_new <- threshold1
    threshold2_new <- threshold2
    
    mu <- mu_new
    gamma <- gamma_new
    
    ## Step2: Expectation
    phi <- matrix(0, C, N)
    for(k in 1:6){
      phi <- phi + t(log(gamma[,,k]))%*%w[,,k]
    }
    phi <- phi-rep(colMeans(phi),each=C)
    
    for(c in 1:C){
      phi[c,] <- mu[c]*exp(phi)[c,]
    }
    phi <- ifelse(phi == rep(colSums(phi),each=C), 1, phi/rep(colSums(phi),each=C))

    ## Step3: Maximization
    mu_new <- rowSums(phi)/N  #update mu

    for(k in 1:6){
      gamma_new[,,k] <- w[,,k]%*%t(phi)/w[,,7]%*%t(phi) #update gamma
    }
    gamma_new[gamma_new == 0] <- 10^(-100)
    if(sum(is.na(gamma_new)) != 0){
      is_zero <- which(is.na(gamma_new))
      gamma_new[is_zero] <- rep(1/6, length(is_zero))
    }
    
    ## Check convergence
    threshold1 <- mean(abs(mu_new - mu)) #mean absolute difference of mu
    threshold2 <- 0
    for(c in 1:C){
      threshold2 <- max(threshold2,norm(as.matrix(gamma_new[,c,] - gamma[,c,]), "O"))# matrix 1-norm of the difference between updated and old gamma
    }
    print(paste0("threshold1 = ", threshold1, " threshold2 = ", threshold2))
  }
  return(list(mu = mu, gamma = gamma))
}


score_estimation_CM <- function(train_df, test_df, par){
  
  ###Input: dataframe of training set, test set, parameter list
  ###Output: estimated score
  set.seed(2)
  mu <- par$mu
  gamma <- par$gamma
  C <- length(mu)
  
  w <- array(0, c(M,N,7))
  for(k in 1:6){
    w[,,k] <- ifelse(t(train_df)==k, 1, 0)
    w[,,k] <- ifelse(is.na(w[,,k]),0,w[,,k])
  }
  w[,,7] <- ifelse(!is.na(t(test_df)), 1, 0)
  
  ##probability by Naive Bayes formula
  prob <- array(0,c(N,M,7))
  prob_mu <- matrix(mu, N, C, byrow = TRUE) 
  phi <- matrix(0, C, N)
  for(k in 1:6){
    phi <- phi + t(log(gamma[,,k]))%*%w[,,k]
  }
  
  phi <- exp(phi)
  
  den <- matrix(diag(prob_mu%*%phi), N, M, byrow=FALSE)   #denominater in equation (2) of cluster model notes
  
  
  for(k in 1:6){
    print(paste0("k = ", k))
    
    num <- (t(phi)*prob_mu)%*%t(gamma[,,k]) #numerator in equation (2) of cluster model notes
    prob[,,k] <- ifelse(num==den & num == 0, runif(1)/6, num/den)
    prob[,,7] <- prob[,,7] + k*prob[,,k]
  }
  return(prob[,,7]*t(w[,,7]))
}
    
    
      
### 5-fold cross validation to find best class number C
set.seed(2)
K <- 5
n <- ncol(train)
n1 <- nrow(train)
n.fold <- floor(n/K)
n1.fold <- floor(n1/K)
s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
s1 <- sample(rep(1:K, c(rep(n1.fold, K-1), n1-(K-1)*n1.fold)))  

c_list <- c(2,3,6,12)
validation_error <-  matrix(NA, K, length(c_list))

train_df <- data.frame(matrix(NA, N, M))
colnames(train_df) <- movie
rownames(train_df) <- user

test_df <- data.frame(matrix(NA, N, M))
colnames(test_df) <- movie
rownames(test_df) <- user
#i=5

for(i in 1:K){
  train_df[s1 != i, ] <- train[s1 != i, ]
  train_df[s1 == i, s != i] <- train[s1==i, s != i]
  test_df[s1 == i,s == i] <- train[s1 == i ,s == i]
  write.csv(train_df,paste0("../output/cluster_model_subtrain.csv"))
  write.csv(test_df,paste0("../output/cluster_model_validation.csv"))



  estimate_df <- test_df

  for(c in 1:length(c_list)){
  
    cm_par <- cluster_model(df = train_df, C = c_list[c], tau = 0.05)
    save(cm_par, file=paste0("../output/cm_par_",c_list[c],"class.RData"))
  
    estimate_df <- score_estimation_CM(train_df = train_df, test_df = test_df, par = cm_par)
      
    write.csv(estimate_df,paste0("../output/cluster_model_validation_class_",c_list[c],".csv"))
    validation_error[i,c] <- sum(abs(estimate_df-test_df),na.rm = T)/sum(!is.na(estimate_df-test_df))
  }
}

save(validation_error, file=paste0("../output/validation_err.RData"))

cv_error<-colMeans(validation_error)
plot(c_list,cv_error,xlab="number of class",ylab="cv-error",col="blue",type="l")
points(c_list,cv_error,col="red",type="o")


class = c_list[which.min(cv_error)]
print(paste("Best class number is", class))

# use best class number to find parameters in EM and predict the rates of movies in test set
best_par <- cluster_model(df = train, C = class, tau = 0.01)
estimate<-score_estimation_CM(train_df = train, test_df = test, par = best_par)
write.csv(estimate,paste0("../output/cluster_model_prediction.csv"))
# MAE of EM algorithm
error_em<- sum(abs(estimate-test),na.rm = T)/sum(!is.na(estimate-test))

# ROC of EM algorithm
threshold <- seq(1,6,by=0.02)
sensitivity <- rep(0, length(threshold))
specificity <- rep(0,length(threshold))
for(i in 1:length(threshold)){
  print(paste("threshold = ", threshold[i]))
  t <- threshold[i]
  test_t <- ifelse(test>=t,1,0)
  estimate_t <- ifelse(estimate>=t,1,0)
  sensitivity[i] <- sum(test_t == 1 & estimate_t == 1, na.rm=T)/sum(test_t == 1, na.rm=T)
  specificity[i] <- sum(test_t == 0 & estimate_t == 0, na.rm=T)/sum(test_t == 0, na.rm=T)
}
save(sensitivity,file=paste("../output/em_sensitivity.RData"))
save(specificity,file=paste("../output/em_specificity.RData"))

plot(1-specificity,sensitivity,xlab="1-Specificity",ylab="Sensitivity", type="l")
abline(0,1, col="red", lty=2)

height = (sens[-1]+sens[-length(sens)])/2
width = -diff(omspec) # = diff(rev(omspec))
sum(height[-1]*width[-1])


library(pROC)
test_roc <- ifelse(test>=4,1,0)
roc(as.factor(as.vector(test_roc)), as.vector(estimate), na.rm = T)
