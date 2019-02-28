require(e1071)
require(kernlab)
require(RColorBrewer)
require(caret)
require(NLP)
require(tm)
require(ggpubr)

n_series = 10^2

###############################################################################################################

original.series <- read.csv("EntropyComplexity_Normal.csv")
attacked.series <- read.csv("EntropyComplexity_Time_Ordered.csv")

Entropy.Complexity.train <- data.frame(Entropy = numeric(n_series*2), Complexity = numeric(n_series*2), Attacked = logical(n_series*2))

Entropy.Complexity.train$Entropy[1:n_series] = original.series[,2]
Entropy.Complexity.train$Complexity[1:n_series] = original.series[,3]
Entropy.Complexity.train$Attacked[1:n_series] = FALSE

Entropy.Complexity.train$Entropy[(n_series+1):(n_series*2)] = attacked.series[,4]
Entropy.Complexity.train$Complexity[(n_series+1):(n_series*2)] = attacked.series[,5]
Entropy.Complexity.train$Attacked[(n_series+1):(n_series*2)] = TRUE

#trainOriginal <- subset(Entropy.Complexity.train, Attacked == FALSE)
#testAttack <- subset(Entropy.Complexity.train, Attacked == TRUE)
#inTrain <- createDataPartition(1:nrow(trainOriginal), p = 0.6, list = FALSE)

#trainpredictors <- trainOriginal[inTrain, 1:2]
#trainLabels <- trainOriginal[inTrain, 3]

#testOriginal <- trainOriginal[-inTrain,]
#testPosNeg <- rbind(testOriginal,testAttack)

#testpredictors <- testPosNeg[,1:2]
#testLabels <- testPosNeg[,3]

###############################################################################################################

#svm.model <- svm(trainpredictors, y = NULL, type = 'one-classification', nu = 0.10, scale = TRUE, kernel = "radial")

#svm.predtrain <- predict(svm.model, trainpredictors, decision.values = TRUE, probability = TRUE)
#svm.predtest <- predict(svm.model, testpredictors)

#confTrain <- table(Predicted = svm.predtrain, Reference = trainLabels)
#confTest <- table(Predicted = svm.predtest, Reference = testLabels)

#confusionMatrix(confTest, positive = 'TRUE')

#print(confTrain)
#print(confTest)

###############################################################################################################

data <- read.csv("EntropyComplexity_Data_Driven.csv")
case <- read.csv("EntropyComplexity_Complete_Case.csv")
random <- read.csv("EntropyComplexity_Random_Imputation.csv")

Entropy.Complexity.test <- data.frame(Entropy = numeric(n_series*3), Complexity = numeric(n_series*3))

Entropy.Complexity.test$Entropy[1:n_series] = data[,8]
Entropy.Complexity.test$Complexity[1:n_series] = data[,9]

Entropy.Complexity.test$Entropy[(n_series+1):(n_series*2)] = case[,2]
Entropy.Complexity.test$Complexity[(n_series+1):(n_series*2)] = case[,3]

Entropy.Complexity.test$Entropy[((n_series*2)+1):(n_series*3)] = random[,6]
Entropy.Complexity.test$Complexity[((n_series*2)+1):(n_series*3)] = random[,7]

###############################################################################################################

Entropy.Imputations <- data.frame(NoAttack = numeric(n_series), Complete = numeric(n_series), TimeOrdered = numeric(n_series), Random = numeric(n_series), DDMI = numeric(n_series))

Entropy.Imputations$NoAttack = original.series[,2]
Entropy.Imputations$Complete = case[,2]
Entropy.Imputations$TimeOrdered = attacked.series[,4]
Entropy.Imputations$Random = random[,6]
Entropy.Imputations$DDMI = data[,8]

write.csv(Entropy.Imputations, file = "EntropyImputation.csv")

cor1 = cor(Entropy.Imputations$NoAttack, Entropy.Imputations$Complete, method = "pearson")
cor2 = cor(Entropy.Imputations$NoAttack, Entropy.Imputations$TimeOrdered, method = "pearson")
cor3 = cor(Entropy.Imputations$NoAttack, Entropy.Imputations$Random, method = "pearson")
cor4 = cor(Entropy.Imputations$NoAttack, Entropy.Imputations$DDMI, method = "pearson") 
cat("Complete Case: ", cor1, " Time Ordered ", cor2, " Random Imputation ", cor3, " DDMI ", cor4, "\n")

Complexity.Imputations <- data.frame(NoAttack = numeric(n_series), Complete = numeric(n_series), TimeOrdered = numeric(n_series), Random = numeric(n_series), DDMI = numeric(n_series))

Complexity.Imputations$NoAttack = original.series[,3]
Complexity.Imputations$Complete = case[,3]
Complexity.Imputations$TimeOrdered = attacked.series[,5]
Complexity.Imputations$Random = random[,7]
Complexity.Imputations$DDMI = data[,9]

write.csv(Complexity.Imputations, file = "ComplexityImputation.csv")

cor1 = cor(Complexity.Imputations$NoAttack, Complexity.Imputations$Complete, method = "pearson")
cor2 = cor(Complexity.Imputations$NoAttack, Complexity.Imputations$TimeOrdered, method = "pearson")
cor3 = cor(Complexity.Imputations$NoAttack, Complexity.Imputations$Random, method = "pearson")
cor4 = cor(Complexity.Imputations$NoAttack, Complexity.Imputations$DDMI, method = "pearson") 
cat("Complete Case: ", cor1, " Time Ordered ", cor2, " Random Imputation ", cor3, " DDMI ", cor4, "\n")



###############################################################################################################

#inTrain <- createDataPartition(1:nrow(Entropy.Complexity.train), p = 0.6, list = FALSE)

#svm.model <- svm(Entropy.Complexity.train[inTrain, 1:2], y = Entropy.Complexity.train[inTrain,3], type = 'C-classification', nu = 0.10, scale = TRUE, kernel = "radial")

#svm.predtest <- predict(svm.model, Entropy.Complexity.train[-inTrain, 1:2])
#confTest <- table(Predicted = svm.predtest, Reference = Entropy.Complexity.train[-inTrain, 3])
#confusionMatrix(confTest, positive = 'TRUE')
#print(confTest)

###############################################################################################################

svm.model <- svm(Entropy.Complexity.train[, 1:2], y = Entropy.Complexity.train[,3], type = 'C-classification', nu = 0.10, scale = TRUE, kernel = "radial", cross=10, probability=TRUE)

svm.predtest <- predict(svm.model, Entropy.Complexity.test, decision.values = TRUE, probability = TRUE)
att.pred <- attr(svm.predtest ,"probabilities")
media.dataDriven <- sum(att.pred[1:100,1])/100
media.completeCase <- sum(att.pred[101:200,1])/100
media.randomImputation <- sum(att.pred[201:300,1])/100
cat(media.dataDriven, " ", media.completeCase, " ", media.randomImputation, "\n")

