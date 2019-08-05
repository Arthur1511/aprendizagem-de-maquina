load("C:/Users/arthu/Documents/Dev/dados TP3.RData")
source("C:/Users/arthu/Documents/Dev/fscore.r")
library(corrplot)
baseDados <- rbind(data$DataTrain0, data$DataTrain1)

baseDados <- som::normalize(baseDados, byrow = FALSE)

base_positiva <- baseDados[c(1:nrow(data$DataTrain0)),]

base_negativa <- baseDados[-c(1:nrow(data$DataTrain0)),]

fscore <- f_score(baseDados = baseDados, base_negativa = base_negativa, base_positiva = base_positiva)

plot(1:ncol(baseDados), fscore, type = "l")

# boxplot(fscore,data=c(1:ncol(baseDados)))

sum(fscore > 0.4)

corr <- cor(baseDados[,1:100])

corrplot(corr, 'color')
