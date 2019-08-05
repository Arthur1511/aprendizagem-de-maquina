library(ggplot2)
library(corrplot)
source("C:/Users/arthu/Documents/Dev/fscore.r")

dados <- read.table("C:/Users/arthu/Documents/Dev/base_sintetica.txt")

labels <- dados[,8]

correlacao <- cor(dados)
dados <- dados[,1:7]

fscore <- f_score(baseDados = dados, base_negativa = dados[labels==-1,], base_positiva = dados[labels==1,])

plot(1:7, fscore, type= 'b' )

corrplot(correlacao, 'color')

pearson <- matrix(0,ncol(dados),ncol(dados))

for (i in 1:ncol(dados)) {
  for (j in 1:ncol(dados)) {
    pearson[i,j] <- cov(x=dados[,i], y=dados[,j], method = "pearson")
  }
}
image(pearson)
corrplot(data.Normalization(pearson, type = "n5"), 'color')

load("C:/Users/arthu/Documents/Dev/Sintetica2.RData")

correlacao2 <- cor(data$Xo)
corrplot(correlacao2, 'color')

fscore2 <- f_score(baseDados = data$Xo, base_negativa = data$Xo[data$Yo==-1,], base_positiva = data$Xo[data$Yo==1,])

for (i in 1:10) {
  for (j in 2:10) {
    distancia <- as.matrix(dist(x = data$Xo[1:100,c(1,2,9,10)], method = 'euclidian', upper = TRUE))
    
    image(distancia)
    Sys.sleep(2)
    
  }
}

pearson2 <- matrix(0,ncol(data$Xo),ncol(data$Xo))

for (i in 1:ncol(data$Xo)) {
  for (j in 1:ncol(data$Xo)) {
    pearson2[i,j] <- cov(x=data$Xo[,i], y=data$Xo[,j], method = "pearson")
  }
}
image(pearson2)

corrplot(data.Normalization(pearson2, type = "n5"), 'color')

