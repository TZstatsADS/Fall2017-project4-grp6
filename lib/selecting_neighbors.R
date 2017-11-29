
#rm(list=ls())
setwd("D:/Github/fall2017-project4-fall2017-proj4-grp6")

########################################## Load data #########################################

rowname_change <- function(matrix){
  rownames(matrix) <- matrix[,1]
  matrix <- matrix[,-1]
  return(matrix)
}

# spearman
spearman_matrix1<-read.csv("./output/spearman_train1.csv",header = T)
spearman_matrix2<-read.csv("./output/spearman_train2.csv",header = T)

rownames(spearman_matrix1)<-spearman_matrix1[,1]
spearman_matrix1<-spearman_matrix1[,-1]
rownames(spearman_matrix2)<-spearman_matrix2[,1]
spearman_matrix2<-spearman_matrix2[,-1]

# spearman + variance weight
spearman_vw_matrix1 <- read.csv("./output/spearman_vm_train1.csv", header = T)
spearman_vw_matrix2 <- read.csv("./output/spearman_vm_train2.csv", header = T)

rownames(spearman_vw_matrix1)<-spearman_vw_matrix1[,1]
spearman_vw_matrix1<-spearman_vw_matrix1[,-1]
rownames(spearman_vw_matrix2)<-spearman_vw_matrix2[,1]
spearman_vw_matrix2<-spearman_vw_matrix2[,-1]


# vector similarity
vecsim_matrix1 <- read.csv("./output/vectorsimilarity_train1.csv", header = T)
vecsim_matrix2 <- read.csv("./output/vectorsimilarity_train2.csv", header = T)

rownames(vecsim_matrix1)<-vecsim_matrix1[,1]
vecsim_matrix1<-vecsim_matrix1[,-1]
rownames(vecsim_matrix2)<-vecsim_matrix2[,1]
vecsim_matrix2<-vecsim_matrix2[,-1]

# vector similarity + variance weight
vecsim_vw_matrix1 <- read.csv("./output/vectorsimilarity_vm_train1.csv", header = T)
vecsim_vw_matrix2 <- read.csv("./output/vectorsimilarity_vm_train2.csv", header = T)

# msd

# msd + variance weight




########################### get the neighbors' matrix (old ver.) ##############################

#list_to_matrix<-function(list){
#  length <- c()
#  for(i in 1:length(list)){
#    length[i] <- length(list[[i]])
#  }
#  max_len <- max(length)
#  matrix <- matrix(nrow = length(list), ncol = max_len)
#  for(i in 1:length(list)){
#    if(length(list[[i]])!=0){
#      matrix[i,1:length(list[[i]])] <- list[[i]]
#    }
#  }
#  return(matrix)
#}


######################### get the neighbor matrix with 0-1 (updated) ##########################

#### turning list to matrix
list_to_matrix <- function(list){
  neighbor_matrix <- matrix(0, nrow = length(list), ncol = length(list))
  for(i in 1:length(list)){
    for(u in list[[i]]){
      if(length(list[[i]]) != 0){
        neighbor_matrix[i,u] <- 1
      }
    }
    print(i) # To see the process
  }
  return(neighbor_matrix)
}


# test
#thresh_spearman <- read.csv("./output/thresh_spearman.csv",header = T)
#rownames(thresh_spearman)<-thresh_spearman[,1]
#thresh_spearman<-thresh_spearman[,-1]



#neighbor_matrix <- matrix(0,nrow = nrow(spearman_matrix1), ncol = nrow(spearman_matrix1))

#for(i in 1:length(thresh_spearman_list)){
#  for(u in thresh_spearman_list[[i]]){
#    neighbor_matrix[i,u] <- 1
#  }
#  print(i)
#}

neighbor_thresh_spearman <- list_to_matrix(thresh_spearman_list)

#which(neighbor_thresh_spearman[1,] == 1)
#thresh_spearman_list[[1]]


###################################### selecting neighbors #####################################

#### correlation-thresholding

#n<-seq(0.1,0.5,0.1) # set threshold number

cor_threshold <- function(matrix, n){
  matrix_for_cal <- matrix
  matrix_for_cal[abs(matrix_for_cal) < n] <- NA
  diag(matrix_for_cal) <- NA
  cor_thre <- list()
  for(i in 1:nrow(matrix_for_cal)){
    x <- matrix_for_cal[i,]
    cor_thre[[i]] <- which(is.na(x)==F)
    print(i) # To see the process
  }
  return(cor_thre)
}

# set threshold as 0.3

#### spearman
cornbors <- cor_threshold(spearman_matrix1,0.3)
cornbors2 <- cor_threshold(spearman_matrix2, 0.3)

thresh_spearman_list <- cor_threshold(spearman_matrix1,0.3)
neighbor_thresh_spearman <- list_to_matrix(thresh_spearman_list)



#cornbors_spearman <- list_to_matrix(cornbors)
#cornbors2_spearman <- list_to_matrix(cornbors2)

#write.csv(cornbors_spearman,file = "./output/thresh_spearman.csv")
#write.csv(cornbors2_spearman,file = "./output/thresh2_spearman.csv")

#### spearman + variance weight
cornbors3 <- cor_threshold(spearman_vw_matrix1, 0.3)
cornbors4 <- cor_threshold(spearman_vw_matrix2, 0.3)

cornbors_spearman_vw <- list_to_matrix(cornbors3)
cornbors2_spearman_vw <- list_to_matrix(cornbors4)

write.csv(cornbors_spearman_vw, file = "./output/thresh_spearman_vw.csv")
write.csv(cornbors2_spearman_vw, file = "./output/thresh2_spearman_vw.csv")



#### best-n-neighbors

#nnbors<-c(20,40)

bestnn <- function(matrix, nnbors){
  best <- list()
  for(i in 1:nrow(matrix)){
    best[[i]] <- order(matrix[i,], decreasing = T)[2:(nnbors+1)]
    #print(i)
  }
  return(best)
}

bnnbors <- bestnn(spearman_matrix1, 30) # nnbors = 30



#bestnn <- matrix(ncol=10, nrow=nrow(train1)) # nnbors=30
#for(i in 1:nrow(train1)){
#  bestnn[i,]<-order(spearman_matrix1_new[i,], decreasing = T)[2:11]
#}




#### combined
bnn = bestnn(matrix, nnbors)
thresh = cor_threshold(matrix, n)

combined <- function(matrix, n, nnbors){
  bnn = bnn
  thresh = thresh
  combine <- list()
  for(i in 1:nrow(matrix)){
    combine[[i]] <- unique(c(bnn[[i]],thresh[[i]])) 
  }
  return(combine)
}

#x = bnnbors[[1]]
#y = cornbors[[1]]
#combine = unique(c(x,y))
#combine_update = unique(combine)