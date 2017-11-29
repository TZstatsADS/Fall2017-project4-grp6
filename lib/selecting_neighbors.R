
#rm(list=ls())
#setwd("D:/Github/fall2017-project4-fall2017-proj4-grp6")

# Load data

spearman_matrix1<-read.csv("./output/spearman_train1.csv",header = T)
spearman_matrix2<-read.csv("./output/spearman_train2.csv",header = T)

rownames(spearman_matrix1)<-spearman_matrix1[,1]
spearman_matrix1<-spearman_matrix1[,-1]
rownames(spearman_matrix2)<-spearman_matrix2[,1]
spearman_matrix2<-spearman_matrix2[,-1]


################################# selecting neighbors #################################

#### correlation-thresholding

n<-seq(0.1,0.5,0.1) # set threshold number

cor_threshold <- function(matrix, n){
  matrix_for_cal <- matrix
  matrix_for_cal[abs(matrix_for_cal) < n] <- NA
  diag(matrix_for_cal) <- NA
  cor_thre <- list()
  for(i in 1:nrow(matrix_for_cal)){
    x <- matrix_for_cal[i,]
    cor_thre[[i]] <- which(is.na(x)==F)
    #print(i)
  }
  return(cor_thre)
}

cornbors <- cor_threshold(spearman_matrix1,0.3)

#cornbors[[1]]

#spearman_matrix1[abs(spearman_matrix1) < 0.3] <- NA
#diag(spearman_matrix1)<-NA
#x<-spearman_matrix1[1,]
#cor_th <- which(is.na(x)==F)
#cor_th


#### best-n-neighbors

nnbors<-c(20,40)

bestnn <- function(matrix, nnbors){
  best <- list()
  for(i in 1:nrow(matrix)){
    best[[i]] <- order(matrix[i,], decreasing = T)[2:(nnbors+1)] # since correlation with 
    #print(i)
  }
  return(best)
}

bnnbors <- bestnn(spearman_matrix1, 20) # nnbors = 20

#bestnn <- matrix(ncol=10, nrow=nrow(train1)) # nnbors=10
#for(i in 1:nrow(train1)){
#  bestnn[i,]<-order(spearman_matrix1_new[i,], decreasing = T)[2:11]
#}




#### combined (in process)
