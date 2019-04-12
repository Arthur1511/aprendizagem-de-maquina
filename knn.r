# Algoritmo do KNN para K iguanl a 1


rm(list = ls())
set.seed(123)
library("MASS")
# library("rgl")
library('caret')
library('som')

baseDados <- read.csv("C:/Users/arthu/Dropbox/Triviais' Works (UFOP)/2019 1/Aprendizagem de maquina/breast-cancer-wisconsin.data", header=FALSE, na.strings="?")
baseDados2 <- na.omit(baseDados[,2:11])
labels_base <- baseDados2[,10]
baseDados2 <- baseDados2[,-10]
labels_base[labels_base == 2] <- -1
labels_base[labels_base == 4] <- 1
baseDados <- baseDados2
baseDados <- som::normalize(baseDados, byrow = FALSE)
labels_base <- as.matrix(labels_base)


# media1 <- rep(1, 2)
# sigma1 <- diag(2)*3
# N1 <- 10 # numero de elementos da classe 1
# classe1 <- mvrnorm(n = N1, media1, sigma1)
# 
# media2 <- rep(-2, 2)
# sigma2 <- diag(2)*3
# N2 <- 10 # numero de elementos da classe 2
# classe2 <- mvrnorm(n = N2, media2, sigma2)
# 
# plot(classe1[,1],classe1[,2],type = "p",col="red",xlim = c(-10,10), ylim = c(-10,10),pch=19)
# points(classe2[,1],classe2[,2],type = "p",col="blue",pch=19)
# 
# 
# # juntando as duas classes em uma unica base de dados
# baseDados <- rbind(classe1,classe2)
# 
# labels_base<- c(rep(1,N1),rep(-1,N2)) 
# labels_base<-as.matrix(labels_base)

decisaoKNN <- function(x, baseDados, k, labels_base){
  
  # classificando o novo ponto de acordo com o vizinho mais próximo
  distanciaVizinhos <- matrix(0,ncol = 1,nrow = nrow(baseDados));
  for (i in c(1:nrow(baseDados))){
    distanciaVizinhos[i] <- dist(rbind(x,baseDados[i,]))   
  }  
  soma = 0
  ordem <- rank(distanciaVizinhos)
  
  for (i in c(1:nrow(distanciaVizinhos))) {
    if(ordem[i] <= k){
      soma = (soma + labels_base[i])
    }
    
  }
  
  if (soma >= 0){
    return(1)
    
  }else{
    return(-1)
  }
}


# novo ponto
# x<-as.matrix(c(0,0))
# x<-t(x)
# 
# points(x,type = "p", col="black",pch=22)
# 
# Sys.sleep(2)
# 
# 
# 
# decisaoKNN(x= x, baseDados = baseDados, k = 3, labels_base = labels_base)

#vizinhoProximo <- which.min(distanciaVizinhos)

# n = 4
# soma = 0
# ordem <- rank(distanciaVizinhos)
# 
# for (i in c(1:nrow(distanciaVizinhos))) {
#   if(ordem[i] <= n){
#     soma = (soma + labels_base[i])
#   }
# 
# }
# 
# if (soma >= 0){
#   points(x,type = "p", col="red",pch=15)
# }else{
#   points(x,type = "p", col="blue",pch=15)
# }
#if (labels_base[vizinhoProximo] == 1){
#  points(x,type = "p", col="red",pch=15)
  
#}else{
#  points(x,type = "p", col="blue",pch=15)
#}


# grid <- seq(-5, 7, by = 0.1)
# Z <- matrix(0,ncol = length(grid), 0, nrow = length(grid))
# 
# for( i in c(1:length(grid))){
#   for( j in c(1:length(grid))){
#     Z[i,j] = decisaoKNN(c(grid[i],grid[j]), baseDados = baseDados, k = 3, labels_base = labels_base)
#   }
# }

# persp3d(grid, grid, Z, aspect = c(1, 1, 0.5), col = "lightblue",alpha=0.8 ,
#         labels=FALSE,xlab = "x",ylab = "y",zlab = "z")
# 
# points3d(classe1[,1], classe1[,2], rep(0,nrow(classe1)),col="red",size=8)
# points3d(classe2[,1], classe2[,2], rep(0,nrow(classe1)),col="blue",size=8)
# 


folds <- createFolds(labels_base, k = 10, list = TRUE, returnTrain = FALSE)


acerto <- c(rep(0,length(folds)))

for (i in (1:(length(folds)))) {
  
  indice <- c(1:length(folds))
  indice <- indice[-i]
  
  bd <- c()
  lb <- c()
  
  for (j in indice) {

    bd <- rbind(bd, baseDados[folds[[j]],]) 
    lb <- cbind(lb, t(labels_base[folds[[j]],]))

    
  }
  
  for (k in folds[[i]]) {
  
    resultado <- decisaoKNN(x= baseDados[k,], baseDados = bd, k = 13, labels_base = lb)  
    if (resultado == labels_base[k]) {
      acerto[i] <- acerto[i]+1
    }
    
  }
  
  acerto[i]<- acerto[i]/length(folds[[i]])
  
}

media <- mean(acerto)
desvio <- sd(acerto)

