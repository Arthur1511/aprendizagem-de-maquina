
rm(list = ls())
# set.seed(123)
library("MASS")

media1 <- rep(4, 2)
sigma1 <- diag(2)*3
N1 <- 10 # numero de elementos da classe 1
classe1 <- mvrnorm(n = N1, media1, sigma1)

media2 <- rep(-2, 2)
sigma2 <- diag(2)*3
N2 <- 10 # numero de elementos da classe 2
classe2 <- mvrnorm(n = N2, media2, sigma2)

media3 <- rep(-8, 2)
sigma3 <- diag(2)*3
N3 <- 10 # numero de elementos da classe 1
classe3 <- mvrnorm(n = N3, media3, sigma3)

plot(classe1[,1],classe1[,2],type = "p",xlim = c(-10,10), ylim = c(-10,10),pch=19)
points(classe2[,1],classe2[,2],type = "p",pch=19)
points(classe3[,1],classe3[,2],type = "p",pch=19)

# juntando as duas classes em uma unica base de dados
baseDados <- rbind(classe1,classe2, classe3)

k <-3
grupos <- matrix(nrow = nrow(baseDados), ncol = 1)
medias <- matrix(nrow = k, ncol = ncol(baseDados))
mediasAtuais <- matrix(nrow = k, ncol = ncol(baseDados))
distancias <- matrix(nrow = nrow(baseDados), ncol = k)
# centros <- matrix(nrow = k, ncol = 2)
epocas <- 1

repeat {

    
  for (i in 1:k) {
    
    # plot(classe1[,1],classe1[,2],type = "p",xlim = c(-10,10), ylim = c(-10,10),pch=19)
    # points(classe2[,1],classe2[,2],type = "p",pch=19)  
    if (epocas == 1) {
      x <- baseDados[sample(nrow(baseDados), 1),]
      x <- t(as.matrix(x))
      points(x[1,1], x[1,2], type = "p", col= "red", pch=19)
    }
    else{
      x <- mediasAtuais[i,]
      x <- t(as.matrix(x))

      # points(x[1,1], x[1,2], type = "p", col= "blue", pch=19)
      Sys.sleep(2)
    }

    for (j in c(1:nrow(baseDados))){
      
      distancias[j,i] <- dist(rbind(x,baseDados[j,]))   
    } 
  }

  
  for (i in 1:nrow(distancias)) {
    grupos[i] <- which.min(distancias[i,])
    
  }
  
  
  for (i in 1:k) {
    aux <- baseDados[grupos == i,]
    mediasAtuais[i,] <- colMeans(aux)
  }
  
  plot(classe1[,1],classe1[,2],type = "p",xlim = c(-10,10), ylim = c(-10,10),pch=19)
  points(classe2[,1],classe2[,2],type = "p",pch=19)  
  points(classe3[,1],classe3[,2],type = "p",pch=19)
  points(mediasAtuais[,1], medias[,2], type = "p", col= "blue", pch=19)
  
  print(epocas)
  if (epocas != 1) {
   erro <- mean(colMeans(abs(mediasAtuais-medias)))
   print(erro)
   
   if (erro < 0.02) 
     break()
   
   if (epocas == 1000) {
     break()
   }
  }
  

  medias <- mediasAtuais
  epocas <- epocas +1
  
}

points(mediasAtuais[,1], medias[,2], type = "p", col= "yellow", pch=19)

