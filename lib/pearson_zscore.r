library("lsa")
setwd("~/Desktop/Fall2017-project4-grp6-master")
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

pearson_zcore_weight <- function(matrix){
  file=deparse(substitute(matrix))
  file_name= paste0("./output/pearson_zscore_",file,".csv")
  
  matrix[is.na(matrix)] = 0
  matrix = t(matrix)
  w = cor(scale(matrix),use="everything",method="pearson")
  
  write.csv(w,file=file_name)
}

w1 = pearson_zcore_weight(train1)
w2 = pearson_zcore_weight(train2)

system.time(pearson_zcore_weight(train1))
system.time(pearson_zcore_weight(train2))
