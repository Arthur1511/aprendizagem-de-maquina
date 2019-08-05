rm(list = ls())

library(caret)
library(ggplot2)
library(corrplot)
library(PredPsych)
library(doParallel)


baseDados <- read.csv("C:/Users/arthu/Documents/Dev/Dados_TP_Heart.csv")

baseDados <- baseDados[,c(-1)]

correlations = cor(baseDados)
# summary(correlations[upper.tri(correlations)])

# fscore <- fscore(Data = baseDados, classCol = 14, featureCol= c(1:13))
# fscore
# rank(fscore)


highlyCorrelated <- findCorrelation(correlations, cutoff = .6)

baseDados <- baseDados[,-highlyCorrelated]
# corrplot(correlations, method="color")

baseDados[,-13] <- som::normalize(baseDados[,-13], byrow = FALSE)
# baseDados2 <- baseDados[,c(3,9,10,12,13,14)]
baseDados$V14 <- as.factor(baseDados$V14)



set.seed(998)

# inTraining <- createDataPartition(baseDados$V14,times = 10, p = .9, list = FALSE)
fitControl <- trainControl( method = 'repeatedcv', number = 10, repeats = 10)

# acuracia_svm <- c()
# acuracia_svmP <- c()
# acuracia_svmL <- c()
# acuracia_gbm <- c()
# acuracia_knn <- c()
# acuracia_rf <- c()
# acuracia_ada <- c()

training <- baseDados

load("C:/Users/arthu/Documents/Dev/teste_Tp2.Rdata")
testing <- teste_TP2[,-highlyCorrelated]

testing <- som::normalize(testing[,], byrow = FALSE)

modelo_svm <- train(V14 ~., data = training, method = 'svmRadial', trControl = fitControl, verbose = TRUE)

results_svm <- predict(modelo_svm, newdata= testing)

write.table(results_svm, "C:/Users/arthu/Documents/Dev/resultados.txt")

# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)
# on.exit(stopCluster(cl))
# for (i in 1:10) {
# 
#   training <- baseDados[inTraining[,i],]
#   testing <- baseDados[-inTraining[,i],]
# 
# 
#   modelo_svm <- train(V14 ~., data = training, method = 'svmRadial', trControl = fitControl, verbose = TRUE)
#   
#   results_svm <- predict(modelo_svm, newdata= testing)
#   acuracia_svm[i] <- (sum(testing[,13] == results_svm)*100)/length(results_svm)
#   
  # modelo_svmP <- train(V14 ~., data = training, method = 'svmPoly', trControl = fitControl, verbose = TRUE)
  # 
  # results_svmP <- predict(modelo_svmP, newdata= testing)
  # acuracia_svmP[i] <- (sum(testing[,13] == results_svmP)*100)/length(results_svmP)
  # 
  # modelo_svmL <- train(V14 ~., data = training, method = 'svmLinear', trControl = fitControl, verbose = TRUE)
  # 
  # results_svmL <- predict(modelo_svmL, newdata= testing)
  # acuracia_svmL[i] <- (sum(testing[,13] == results_svmL)*100)/length(results_svmL)
  # 
  # modelo_gbm <- train(V14 ~., data = training, method = 'gbm', trControl = fitControl, verbose = TRUE)
  # 
  # results_gbm <- predict(modelo_gbm, newdata= testing)
  # acuracia_gbm[i] <- (sum(testing[,13] == results_gbm)*100)/length(results_gbm)
  
  
  # modelo_knn <- train(V14 ~., data = training, method = 'kknn', trControl = fitControl, verbose = TRUE)
  # 
  # results_knn <- predict(modelo_knn, newdata= testing)
  # acuracia_knn[i] <- (sum(testing[,13] == results_knn)*100)/length(results_knn)
  # 
  # 
  # modelo_rf <- train(V14 ~., data = training, method = 'rf', trControl = fitControl, verbose = TRUE)
  # 
  # results_rf <- predict(modelo_rf, newdata= testing)
  # acuracia_rf[i] <- (sum(testing[,13] == results_rf)*100)/length(results_rf)
  # 
  # 
  # modelo_ada <- train(V14 ~., data = training, method = 'ada', trControl = fitControl, verbose = TRUE)
  # 
  # results_ada <- predict(modelo_rf, newdata= testing)
  # acuracia_ada[i] <- (sum(testing[,13] == results_ada)*100)/length(results_ada)


  
# }

# stopCluster(cl)

# mean(acuracia_svm)
# sd(acuracia_svm)

# mean(acuracia_svmP)
# sd(acuracia_svmP)
# 
# mean(acuracia_svmL)
# sd(acuracia_svmL)
# 
# mean(acuracia_gbm)
# sd(acuracia_gbm)
# 
# mean(acuracia_knn)
# sd(acuracia_knn)
# 
# mean(acuracia_rf)
# sd(acuracia_rf)
# 
# mean(acuracia_ada)
# sd(acuracia_ada)
