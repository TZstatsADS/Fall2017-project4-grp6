#######################################
# Similarity weights - Spearman, Vector Similarity, MSD
# Saaya Yasuda (sy2569)
#######################################

rm(list=ls())
setwd('~/Documents/Github/fall2017-project4-fall2017-proj4-grp6')

#######################################
# install & load necessary packages
#######################################
packages.used=c("lsa")

packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(lsa)


#######################################
# Load data
#######################################
train1 <- read.csv("./output/dataset1_train.csv",header=T)
test1 <- read.csv("./output/dataset1_test.csv",header=T)
train2 <- read.csv("./output/dataset2_train.csv",header=T)
test2 <- read.csv("./output/dataset2_test.csv",header=T)

rownames(train1) <- train1[,1]
train1 <- train1[,-1]
rownames(train2) <- train2[,1]
train2 <- train2[,-1]

#######################################
# Spearman
#######################################

spearman_weight <- function(user_vec_a, user_vec_u){
  rank_a = rank(user_vec_a, ties.method = 'average')
  rank_u = rank(user_vec_u, ties.method = 'average')
  ranks = cbind(rank_a, rank_u)
  w <- cov(ranks) / (sd(rank_a) * sd(rank_u))
  return(w)
}

w1 = spearman_weight(train1[1,], train1[2,])
w2 = spearman_weight(train2[1,], train2[2,])
w1
w2
w1[1,2]
w2[1,2]
#cor.test(x=unlist(train1[1,]), y=unlist(train1[2,]), method = 'spearman')

#######################################
# Vector Similarity
#######################################

vector_similarity <- function(user_vec_a, user_vec_u){
  user_vec_a = unlist(user_vec_a,use.names=FALSE)
  user_vec_u = unlist(user_vec_u,use.names=FALSE)
  #w = crossprod(user_vec_a, user_vec_u)/sqrt(crossprod(user_vec_a) * crossprod(user_vec_u))
  library(lsa)
  w = cosine(user_vec_a, user_vec_u)
  return(w)
}

w1 = vector_similarity(train1[1,], train1[2,])
w2 = vector_similarity(train2[1,], train2[2,])
w1
w2
#0 and NA
w1[1,2]
w2[1,2]

#debug
user_vec_a = train1[1,]
user_vec_u = train1[2,]
typeof(user_vec_a)
user_vec_a = unlist(user_vec_a,use.names=FALSE)
user_vec_u = unlist(user_vec_u,use.names=FALSE)
cosine(user_vec_a, user_vec_u)


#######################################
# Mean Squared Difference
#######################################

mean_sq_diff <- function(user_vec_a, user_vec_u){
  msd = mean((user_vec_a - user_vec_u)^2)
  return(msd)
}

w1 = mean_sq_diff(train1[1,], train1[2,])
w2 = mean_sq_diff(train2[1,], train2[2,])
w1
w2

