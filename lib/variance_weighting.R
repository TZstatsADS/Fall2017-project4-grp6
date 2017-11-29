rm(list=ls())
setwd("D:/Github/fall2017-project4-fall2017-proj4-grp6")

# Load data

train1 <- read.csv("./output/dataset1_train.csv",header=T)
test1 <- read.csv("./output/dataset1_test.csv",header=T)
train2 <- read.csv("./output/dataset2_train.csv",header=T)
test2 <- read.csv("./output/dataset2_test.csv",header=T)

rownames(train1) <- train1[,1]
train1 <- train1[,-1]
rownames(train2) <- train2[,1]
train2 <- train2[,-1]
train2[is.na(train2)]<-0


##################################### Variance Weighting #####################################

#var <- apply(train1, 2, var)
#v <- (var-min(var))/max(var)
#train_new <- train1 * sqrt(v)
#spearman_matrix1_nn <- cor(t(train_new),method = "spearman")


# spearman

var_weighting_spearman <- function(matrix){
  var <- apply(matrix, 2, var)
  v <- (var-min(var))/max(var)
  matrix <- matrix * sqrt(v)
  spearman_vw <- cor(t(matrix), method = "spearman")
  return(spearman_vw)
}

spearman_vm_matrix1 <- var_weighting_spearman(train1)
spearman_vm_matrix2 <- var_weighting_spearman(train2)


# Vector Similarity

library(lsa)
var_weighting_vecsim <- function(matrix){
  var <- apply(matrix, 2, var)
  v <- (var-min(var))/max(var)
  matrix <- matrix * sqrt(v)
  vs_vw <- cosine(t(matrix))
  return(vs_vw)
}

vs_vw_matrix1 <- var_weighting_vecsim(train1)
vs_vw_matrix2 <- var_weighting_vecsim(train2)


# mean squared difference

var_weighting_msd <- function(matrix){
  var <- apply(matrix, 2, var)
  v <- (var-min(var))/max(var)
  matrix <- matrix * sqrt(v)
  msd_vw <- mean_sq_diff(matrix)
  return(msd_vw)
}

msd_vm_matrix1 <- var_weighting_msd(train1)
msd_vm_matrix2 <- var_weighting_msd(train2)