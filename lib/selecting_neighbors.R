
#rm(list=ls())
#setwd("D:/Github/fall2017-project4-fall2017-proj4-grp6")

########################################## Load data #########################################

train1 <- read.csv("./output/dataset1_train.csv",header=T)
test1 <- read.csv("./output/dataset1_test.csv",header=T)
train2 <- read.csv("./output/dataset2_train.csv",header=T)
test2 <- read.csv("./output/dataset2_test.csv",header=T)

rownames(train1) <- train1[,1]
train1 <- train1[,-1]
rownames(train2) <- train2[,1]
train2 <- train2[,-1]
train2[is.na(train2)]<-0

rownames(test1) <- test1[,1]
test1 <- test1[,-1]
rownames(test2) <- test2[,1]
test2 <- test2[,-1]
test2[is.na(test2)]<-0

#### weight
sp_w1 = read.csv("./output/spearman_train1.csv",header=T)
sp_w2 = read.csv("./output/spearman_train2.csv",header=T)
sp_vm_w1 = read.csv("./output/spearman_vm_train1.csv",header=T)
sp_vm_w2 = read.csv("./output/spearman_vm_train1.csv",header=T)
vs_w1 = read.csv("./output/vectorsimilarity_train1.csv",header=T)
vs_w2 = read.csv("./output/vectorsimilarity_train1.csv",header=T)
vs_vm_w1 = read.csv("./output/vectorsimilarity_vm_train1.csv",header=T)
vs_vm_w2 = read.csv("./output/vectorsimilarity_vm_train2.csv",header=T)

msd_w1 = read.csv("./output/meansqdiff_train1.csv",header=T)
msd_w2 = read.csv("./output/meansqdiff_train2.csv",header=T)

rownames(sp_w1) <- sp_w1[,1]
sp_w1 <- sp_w1[,-1]

rownames(sp_w2) <- sp_w2[,1]
sp_w2 <- sp_w2[,-1]

rownames(sp_vm_w1) <- sp_vm_w1[,1]
sp_vm_w1 <- sp_vm_w1[,-1]

rownames(sp_vm_w2) <- sp_vm_w2[,1]
sp_vm_w2 <- sp_vm_w2[,-1]

rownames(vs_w1) <- vs_w1[,1]
vs_w1 <- vs_w1[,-1]

rownames(vs_w2) <- vs_w2[,1]
vs_w2 <- vs_w2[,-1]

rownames(vs_vm_w1) <- vs_vm_w1[,1]
vs_vm_w1 <- vs_vm_w1[,-1]

rownames(vs_vm_w2) <- vs_vm_w2[,1]
vs_vm_w2 <- vs_vm_w2[,-1]

rownames(msd_w1) <- msd_w1[,1]
msd_w1 <- msd_w1[,-1]

rownames(msd_w2) <- msd_w2[,1]
msd_w2 <- msd_w2[,-1]

################################### Get the neighbor matrix under different method ###################################

#### neighbor matrix

# threshold (cor_threshold)
thresh_spearman_list <- cor_threshold(sp_w1,0.3)
neighbor_thresh_spearman <- list_to_matrix(thresh_spearman_list)

write.csv(neighbor_thresh_spearman, file = "./output/neighbor_thresh_spearman.csv")



thresh_spearman_list2 <- cor_threshold(sp_w2,0.3)
neighbor_thresh_spearman2 <- list_to_matrix(thresh_spearman_list2)

write.csv(neighbor_thresh_spearman2, file = "./output/neighbor_thresh_spearman2.csv")


thresh_spearman_vw_list <- cor_threshold(sp_vm_w1,0.3)
neighbor_thresh_spearman_vw <- list_to_matrix(thresh_spearman_vw_list)

write.csv(neighbor_thresh_spearman_vw, file = "./output/neighbor_thresh_spearman_wv.csv")



thresh_spearman_vw_list2 <- cor_threshold(sp_vm_w2,0.3)
neighbor_thresh_spearman_vw2 <- list_to_matrix(thresh_spearman_vw_list2)


write.csv(neighbor_thresh_spearman_vw2, file = "./output/neighbor_thresh_spearman_wv2.csv")


thresh_vecsim_list <- cor_threshold(vs_w1,0.3)
neighbor_thresh_vecsim <- list_to_matrix(thresh_vecsim_list)


write.csv(neighbor_thresh_vecsim, file = "./output/neighbor_thresh_vecsim.csv")


thresh_vecsim_list2 <- cor_threshold(vs_w2,0.3)
neighbor_thresh_vecsim2 <- list_to_matrix(thresh_vecsim_list2)


write.csv(neighbor_thresh_vecsim2, file = "./output/neighbor_thresh_vecsim2.csv")



thresh_vecsim_vw_list <- cor_threshold(vs_vm_w1,0.3)
neighbor_thresh_vecsim_vm <- list_to_matrix(thresh_vecsim_vw_list)


write.csv(neighbor_thresh_vecsim_vm, file = "./output/neighbor_thresh_vecsim_vm.csv")



thresh_vecsim_vw_list2 <- cor_threshold(vs_vm_w2,0.3)
neighbor_thresh_vecsim_vm2 <- list_to_matrix(thresh_vecsim_vw_list2)

write.csv(neighbor_thresh_vecsim_vm2, file = "./output/neighbor_thresh_vecsim_vm2.csv")


### sth wrong with msd weight matrix
thresh_msd_list <- cor_threshold(msd_w1, 0.3)
neighbor_thresh_msd <- list_to_matrix(thresh_msd_list)

thresh_msd_list2 <- cor_threshold(msd_w2,0.3)
neighbor_thresh_msd2 <- list_to_matrix(thresh_msd_list2)


# bnn (bestnn)
bnn_spearman_list <- bestnn(sp_w1,30)
neighbor_bnn_spearman <- list_to_matrix(bnn_spearman_list)

write.csv(neighbor_bnn_spearman, file = "./output/neighbor_bnn_spearman.csv")



bnn_spearman_list2 <- bestnn(sp_w2,30)
neighbor_bnn_spearman2 <- list_to_matrix(bnn_spearman_list2)

write.csv(neighbor_bnn_spearman2, file = "./output/neighbor_bnn_spearman2.csv")



bnn_spearman_vw_list <- bestnn(sp_vm_w1,30)
neighbor_bnn_spearman_vw <- list_to_matrix(bnn_spearman_vw_list)

write.csv(neighbor_bnn_spearman_vw, file = "./output/neighbor_bnn_spearman_vw.csv")


bnn_spearman_vw_list2 <- bestnn(sp_vm_w2,30)
neighbor_bnn_spearman_vm2 <- list_to_matrix(bnn_spearman_vw_list2)


write.csv(neighbor_bnn_spearman_vm2, file = "./output/neighbor_bnn_spearman_vm2.csv")



bnn_vecsim_list <- bestnn(vs_w1,30)
neighbor_bnn_vecsim <- list_to_matrix(bnn_vecsim_list)

write.csv(neighbor_bnn_vecsim, file = "./output/neighbor_bnn_vecsim.csv")


bnn_vecsim_list2 <- bestnn(vs_w2,30)
neighbor_bnn_vecsim2 <- list_to_matrix(bnn_vecsim_list2)


write.csv(neighbor_bnn_vecsim2, file = "./output/neighbor_bnn_vecsim2.csv")


bnn_vecsim_vw_list <- bestnn(vs_vm_w1,30)
neighbor_bnn_vecsim_vm <- list_to_matrix(bnn_vecsim_vw_list)

write.csv(neighbor_bnn_vecsim_vm, file = "./output/neighbor_bnn_vecsim_vm.csv")


bnn_vecsim_vw_list2 <- bestnn(vs_vm_w2,30)
neighbor_bnn_vecsim_vm2 <- list_to_matrix(bnn_vecsim_vw_list2)

write.csv(neighbor_bnn_vecsim_vm2, file = "./output/neighbor_bnn_vecsim_vm2.csv")



# sth wrong with msd weight matrix
bnn_msd_list <- bestnn(msd_w1, 30)
neighbor_bnn_msd <- list_to_matrix(bnn_msd_list)


bnn_msd_list2 <- bestnn(msd_w2,30)
neighbor_bnn_msd2 <- list_to_matrix(thresh_bnn_list2)

# combined (combined)

list1 = thresh_spearman_list
list2 = bnn_spearman_list
comb_spearman_list <- combined(sp_w1)
neighbor_comb_spearman <- list_to_matrix(comb_spearman_list)

write.csv(neighbor_comb_spearman, file = "./output/neighbor_comb_spearman.csv")


list1 = thresh_spearman_list2
list2 = bnn_spearman_list2
comb_spearman_list2 <- combined(sp_w2)
neighbor_comb_spearman2 <- list_to_matrix(comb_spearman_list2)

write.csv(neighbor_comb_spearman2, file = "./output/neighbor_comb_spearman2.csv")



########################### get the neighbor matrix (old ver.) ##############################

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



#### best-n-neighbors

#nnbors<-c(20,40)

bestnn <- function(matrix, nnbors){
  best <- list()
  for(i in 1:nrow(matrix)){
    best[[i]] <- order(matrix[i,], decreasing = T)[2:(nnbors+1)]
    print(i)
  }
  return(best)
}

bnnbors <- bestnn(spearman_matrix1, 30) # nnbors = 30



#bestnn <- matrix(ncol=10, nrow=nrow(train1)) # nnbors=30
#for(i in 1:nrow(train1)){
#  bestnn[i,]<-order(spearman_matrix1_new[i,], decreasing = T)[2:11]
#}




#### combined
list1 = bestnn(matrix, nnbors)
list2 = cor_threshold(matrix, n)

combined <- function(matrix){
  bnn = list1
  thresh = list2
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