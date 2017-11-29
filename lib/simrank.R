
####################################### Data Processing #######################################

setwd("D:/Github/fall2017-project4-fall2017-proj4-grp6")

train1 <- read.csv("./output/dataset1_train.csv",header=T)
test1 <- read.csv("./output/dataset1_test.csv",header=T)
rownames(train1) <- train1[,1]
train1 <- train1[,-1]
rownames(test1) <- test1[,1]
test1 <- test1[,-1]

# convert web name
convert_colname <- function(dataset){
  names <- colnames(dataset)
  for(i in 1:ncol(dataset)){
    names[i] <- sub("X","",names[i])
  }
  return(names)
}

colnames(train1) <- convert_colname(train1)
colnames(test1) <- convert_colname(test1)

#names <- colnames(train1)

# convert web name
#for(i in 1:ncol(train1)){
#  names[i] <- sub("X","",names[i])
#}



#x = train1[1,]
#which(x==1)
#colnames(x)[which(x==1)]

linkgraph_bip <- list()
for(i in 1:nrow(train1)){
  linkgraph_bip[[i]] <- colnames(train1[i,])[which(train1[i,]==1)]
  #print(i)
}

n = length(linkgraph_bip)
n

t_train1 <- t(train1)
t_train1 <- as.data.frame(t_train1)
for(i in 1:nrow(t_train1)){
  linkgraph_bip[[n+i]] <- colnames(t_train1[i,])[which(t_train1[i,]==1)]
  #print(i)
}

length(linkgraph_bip)

nods_name <- as.numeric(c(rownames(train1),colnames(train1)))


# Get the linkgraph file

#for(i in 1:length(linkgraph_bip)){
#  x = c(nods_name[i], as.numeric(linkgraph_bip[[i]]))
#  write(x, file = "D:/Github/fall2017-project4-fall2017-proj4-grp6/output/linkgraph.txt", ncolumns = 10000, append = T)
  #print(i)
#}


#x = as.integer(linkgraph_bip[[1]])
#write(x, file = "D:/Github/fall2017-project4-fall2017-proj4-grp6/output/linkgraph.txt", ncolumns = 10000 )

length <- c()
for(i in 1:length(linkgraph_bip)){
  length[i] = length(linkgraph_bip[[i]])
}

max_len <- max(length)
max_len

# Get graph matrix
graph_matrix <- matrix(nrow = length(linkgraph_bip), ncol = max_len+1)
graph_matrix[,1] <- nods_name

for(i in 1:length(linkgraph_bip)){
  graph_matrix[i,2:(1+length(linkgraph_bip[[i]]))]<-as.numeric(linkgraph_bip[[i]])
  #print(i)
}

dim(graph_matrix)

####################################### Simrank Algorithm #######################################

graph <- graph_matrix

nods<-nods_name
nnods <- length(nods)

#sum(is.na(graph[1,-1])==F)

#which(nods==graph[1,2])


trans_matrix <- function(graph){
  trans_matrix <- matrix(0, nrow = nnods, ncol = nnods)
  rownames(trans_matrix) <- nods
  colnames(trans_matrix) <- nods
  for(i in 1:nnods){
    n = sum(is.na(graph[i,-1])==F)
    for(m in 1:n){
      loc <- which(nods == graph[i,m+1])
      trans_matrix[i,loc] <- 1/n
    }
  }
  return(trans_matrix)
}

trans_matrix <- trans_matrix(graph)
#write.csv(trans_matrix, file = "D:/Github/fall2017-project4-fall2017-proj4-grp6/output/trans.csv")

#### naive method

sim_matrix <- matrix(0, nrow = nnods, ncol = nnods)
rownames(sim_matrix)<-nods
colnames(sim_matrix)<-nods
sim_matrix_old <- sim_matrix
diag(sim_matrix) <- 1 # initial simrank matrix with diagnal = 1
In <- sim_matrix

c = 0.8
#sim0 <- sim_matrix
#sim1 <- t(trans_matrix) %*% sim0 %*% trans_matrix + (1-c)*sim_matrix

niter = 5
sim_list <- list()

for(i in 1:niter){
  print(c("start",i))
  ptm <- proc.time()[1]
  sim_matrix_old <- sim_matrix
  sim_matrix <- c * (t(trans_matrix) %*% sim_matrix_old %*% trans_matrix) + (1-c)*In
  sim_list[[i]] <- sim_matrix
  end <- proc.time()[1]
  print(c("end", i, end-ptm))
}


simrank <- sim_list[[5]]
simrank_matrix <- simrank[1:nrow(train1),1:nrow(train1)]
# for easier upcoming calculation
diag(simrank_matrix) <- 1

#relations <- linkgraph_bip
#for(i in 1:nnods){
#  relations[[i]] <- c(nods[i], as.numeric(relations[[i]]))
#}

# relations = linkgraph_bip
#sim_matrix <- matrix(0, nrow = nnods, ncol = nnods)
#rownames(sim_matrix)<-nods
#colnames(sim_matrix)<-nods
#sim_matrix_old <- sim_matrix
#diag(sim_matrix) <- 1 # initial simrank matrix with diagnal = 1



#niter <- 10
#similarity <- function(relations, sim_df, sim_df_old){ # sim_df = sim_matrix, sim_df_old = sim_matrix_old
  
#  n <- nnods
#  r <- 0.8 #(r = c)
#  s_uv <- 0 
#  nodes <- nods
  
  #for (node in 1:n){
  #  nodes <- c(nodes,relations[[node]][1])
  #}
  #browser()
  
#  for(iter in 1:niter){
#    print(iter)
#    if(converged(sim_df,sim_df_old)){break}
#    sim_df <- sim_df_old
#    for (u in 1:n){
#      for (v in 1:n){
#        node_relations_u <- relations[[u]]
#        node_relations_v <- relations[[v]]
#        node_u <- relations[[u]][1]
#        node_v <- relations[[v]][1]
#        length_relations_u <- length(relations[[u]])-1
#        length_relations_v <- length(relations[[v]])-1
#        node_related_u <- relations[[u]][-1]
#        node_related_v <- relations[[v]][-1]
#       
#        if (node_u == node_v) {next} else {s_uv=0.0}
#        
#        for (n_u in node_related_u){
#          
#          for (n_v in node_related_v){
#            s_uv = s_uv + sim_df_old[as.character(n_u),as.character(n_v)]
#          }
#          
#          sim_df[u,v] = r * s_uv / (length(node_related_u)*length(node_related_v))
#          
#        }
#        print(c(u,v))
#      }
#    }
    
#    iter <- iter + 1
#  }
  #browser()
#  return (sim_df)
#}


#converged <- function(sim, sim_old, eps = 10^-4){
#  if (any(abs(sim - sim_old) >= eps)) {return (FALSE)}
#  else {return (TRUE)}
#}


#x = similarity(relations,sim_matrix,sim_matrix_old)
