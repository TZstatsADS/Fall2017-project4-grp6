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

rownames(train1) = train1[,1]
train1 = train1[,-1]
rownames(test1) = test1[,1]
test1 = test1[,-1]

rownames(train2) = train2[,1]
train2 = train2[,-1]
rownames(test2) = test2[,1]
test2 = test2[,-1]

#######################################
# Spearman
#######################################

spearman_weight <- function(matrix){
  file=deparse(substitute(matrix))
  file_name= paste0("./output/spearman_",file,".csv")
  
  matrix[is.na(matrix)] = 0
  matrix = t(matrix)
  w = cor(matrix,use="everything",method="spearman")
  
  write.csv(w,file=file_name)
}

w1 = spearman_weight(train1)
w2 = spearman_weight(train2)

#######################################
# Vector Similarity
#######################################

vector_similarity <- function(matrix){
  library(lsa)
  
  file=deparse(substitute(matrix))
  file_name= paste0("./output/vectorsimilarity_",file,".csv")
  
  matrix[is.na(matrix)] = 0
  matrix = t(matrix)
  w = cosine(matrix)
  write.csv(w,file=file_name)
}

w1 = vector_similarity(train1)
w2 = vector_similarity(train2)


#######################################
# Mean Squared Difference
#######################################

# Reduced the computational time significantly by assigning arguments outside 
# the loops, initiating the output matrix r with NA first, and using apply 
# to mean() outside the loops.

# system.time(mean_sq_diff(train1)) #65.051 sec
# system.time(mean_sq_diff(train2)) #100.079 sec

mean_sq_diff <- function(matrix){

  file=deparse(substitute(matrix))
  file_name= paste0("./output/meansqdiff_",file,".csv")
  
  matrix[is.na(matrix)] = 0
  usermean = apply(matrix,1,mean)
  
  ncolrow = nrow(matrix)
  w <- matrix(rep(NA), ncolrow, ncolrow)
  rownames(w) = rownames(matrix)
  colnames(w) = rownames(matrix)
  
  for (r in 1:ncolrow){
    for (c in 1:ncolrow){
      if (r>c){
        w[r,c] = w[c,r]
      }
      else if(r==c){
        w[r,c] = 0
      }
      else {
        w[r,c] = (usermean[r]-usermean[c])^2
      }
    }
  }
  write.csv(w,file=file_name)
}

w1 = mean_sq_diff(train1)
w2 = mean_sq_diff(train2)
