###### data processing function for dataset1
process_dataset1<-function(df){
  
  y<-df[df$V1=="C",]
  user<-unique(y$V2) # get all the users
  
  x<-df[df$V1=="V",]
  x<-x[order(x$V2),]
  attr<-unique(x$V2) # get all the attributes
  
  m<-length(user) # number of users
  n<-length(attr) # number of attributes
  
  process_matrix<-matrix(0,nrow = m,ncol = n)
  rownames(process_matrix)<-user
  colnames(process_matrix)<-attr
  
  # location of users in data_train
  locate<-c()
  for(i in 1:m){
    locate[i]<-which(df$V3==user[i])
  }
  
  # seperate each user's vote info
  list<-list()
  for(i in 1:(m-1)){
    list[[i]]<-df[(locate[i]+1):(locate[i+1]-1),]$V2
  }
  list[[m]] <- df[(locate[m]+1):nrow(df),]$V2
  # number of votes for each user
  num<-c()
  for(i in 1:length(list)){
    num[i]<-length(list[[i]])
  }
  
  # get the final transformed matrix
  for(i in 1:length(list)){
    for(x in 1:num[i]){
      process_matrix[i,which(attr==list[[i]][x])]<-1
    }
  }
  
  return(as.data.frame(process_matrix))
}

# data processing function for dataset2
process_dataset2 <- function(df){
  
  # Input: original dataset
  # Output: dataset whose columns reprsent movie ids, rows represent user ids, 
  #         and ijTh element represents the score of jth movie rated by ith user
  library(dplyr)
  
  user <- unique(df$User)
  r <- length(user)
  process_matrix <- matrix(NA, nrow = r, ncol = c)
  colnames(process_matrix) <- movie
  rownames(process_matrix) <- user
  
  for(i in 1:r){
    df_i <- df%>%filter(User == user[i])
    for(j in 1:c){
      if(movie[j] %in% df_i$Movie){
        process_matrix[i,j] = df_i$Score[df_i$Movie == movie[j]] - 1 #transform score from 1-6 to 0-5
      }
    }
  }
  
  return(data.frame(process_matrix))
  
}