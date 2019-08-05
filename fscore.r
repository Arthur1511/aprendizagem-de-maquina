
# baseDados <- read.csv("C:/Users/arthu/Dropbox/Triviais' Works (UFOP)/2019 1/Aprendizagem de maquina/breast-cancer-wisconsin.data", header=FALSE, na.strings="?")
# baseDados2 <- na.omit(baseDados[,2:11])
# labels_base <- baseDados2[,10]
# baseDados2 <- baseDados2[,-10]
# labels_base[labels_base == 2] <- -1
# labels_base[labels_base == 4] <- 1
# baseDados <- baseDados2
# baseDados <- som::normalize(baseDados, byrow = FALSE)
# labels_base <- as.matrix(labels_base)
# 
# 
# 
# base_positiva <- baseDados[labels_base==1,]
# base_negativa <- baseDados[labels_base==-1,]

f_score <- function(baseDados, base_negativa, base_positiva){
  fscore <- c()
  
  for (i in 1:ncol(baseDados)){
  
    x_positivo <- 0
    x_negativo <- 0
    
    x <- (mean(base_positiva[,i]) - mean(baseDados[,i]))^2 + (mean(base_negativa[,i]) - mean(baseDados[,i]))^2
    
    for (j in 1:nrow(base_positiva)) {
      
      x_positivo <- x_positivo + (base_positiva[j,i] - mean(base_positiva[,i]))^2
    }
    
    for (k in 1:nrow(base_negativa)) {
      
      x_negativo <- x_negativo + (base_negativa[k,i] - mean(base_negativa[,i]))^2
    }
    
    fscore[i] <- x/((x_positivo/(nrow(base_positiva)-1)) + (x_negativo/(nrow(base_negativa)-1)))
  }
  
  print(fscore)
  print(rank(fscore))
  
  return(fscore)
}
