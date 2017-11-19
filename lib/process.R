library(dplyr)
setwd("/Users/duanshiqi/Documents/GitHub/fall2017-project4-fall2017-proj4-grp6/data/")
train <- read.csv("./dataset2/data_train.csv",header=T)
test<-read.csv("./dataset2/data_test.csv",header=T)

movie <- unique(train$Movie)
c <-length(movie)

# data processing function for dataset2
process_dataset2 <- function(df){
  
  # Input: original dataset
  # Output: dataset whose columns reprsent movie ids, rows represent user ids, 
  #         and ijTh element represents the score of jth movie rated by ith user
  
  user <- unique(df$User)
  r <- length(user)
  process_matrix <- matrix(NA, nrow = r, ncol = c)
  colnames(process_matrix) <- movie
  rownames(process_matrix) <- user
  
  for(i in 1:r){
    df_i <- df%>%filter(User == user[i])
    for(j in 1:c){
      if(movie[j] %in% df_i$Movie){
        process_matrix[i,j] = df_i$Score[df_i$Movie == movie[j]]
      }
    }
  }
  
  return(data.frame(process_matrix))
  
}

train_matrix <- process_dataset2(df = train)
write.csv(train_matrix,"../output/dataset2_train.csv")

test_matrix <- process_dataset2(df = test)
write.csv(test_matrix,"../output/dataset2_test.csv")
