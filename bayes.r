rm(list = ls())
set.seed(123)
library("MASS")
library("rgl")
library(mvtnorm)

media1 <- rep(1, 2)
sigma1 <- diag(2)*3
N1 <- 10 # numero de elementos da classe 1
classe1 <- mvrnorm(n = N1, media1, sigma1)

media2 <- rep(-2, 2)
sigma2 <- diag(2)*3
N2 <- 10 # numero de elementos da classe 2
classe2 <- mvrnorm(n = N2, media2, sigma2)

plot(classe1[,1],classe1[,2],type = "p",col="red",xlim = c(-10,10), ylim = c(-10,10),pch=19)
points(classe2[,1],classe2[,2],type = "p",col="blue",pch=19)


# juntando as duas classes em uma unica base de dados
baseDados <- rbind(classe1,classe2)

labels_base<- c(rep(1,N1),rep(-1,N2))
labels_base<-as.matrix(labels_base)


decisaoBayes <- function(x, media1, sigma1, media2, sigma2){
  
  if(dmvnorm(x, media1, sigma1) > dmvnorm(x, media2, sigma2))
    return(1)
  else
    return(-1)
}


grid <- seq(-5, 7, by = 0.1)
Z <- matrix(0,ncol = length(grid), 0, nrow = length(grid))

for( i in c(1:length(grid))){
  for( j in c(1:length(grid))){
    Z[i,j] = decisaoBayes(c(grid[i],grid[j]), media1, sigma1, media2, sigma2)
  }
}

persp3d(grid, grid, Z, aspect = c(1, 1, 0.5), col = "lightblue",alpha=0.8 ,
        labels=FALSE,xlab = "x",ylab = "y",zlab = "z")

points3d(classe1[,1], classe1[,2], rep(0,nrow(classe1)),col="red",size=8)
points3d(classe2[,1], classe2[,2], rep(0,nrow(classe1)),col="blue",size=8)