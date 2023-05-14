rm(list = ls())

library(ggplot2)
library(lattice)
library(caret)
library(kernlab)
library(utils)
library(dplyr)

data1 <- read.csv('data/winequality-red.csv',sep=';',header=TRUE)
data2 <- read.csv('data/winequality-white.csv',sep=';',header=TRUE)
data <- rbind (data1,data2)

label <- unique(data$quality)
quality_label <- data.frame(quality = label,quality_names = c('quality 5','quality 6','quality 7','quality 4','quality 8','quality 3','quality 9'))
data <- merge(data,quality_label,by.x = 'quality', by.y = 'quality')
data <- select(data,-quality)
summary(data)

percentage <- prop.table(table(data$quality_names)) * 100
cbind(freq=table(data$quality_names), percentage=percentage)

validation_index <- createDataPartition(data$quality_names,p=0.80, list=FALSE)
validation <- data[-validation_index,]
training <- data[validation_index,]

control <- trainControl(method="cv", number=10,savePredictions = TRUE)

metric <- "Accuracy" # Do do danh gia


# kNN
library(class)
set.seed(7)
fit.knn <- train(quality_names~., data=training, method="knn", metric=metric, trControl=control)

# SVM
set.seed(7)
fit.svm <- train(quality_names~., data=training,method="svmRadial", metric=metric, trControl=control)


plotConfusionMatrix <- function(cfm)
{
  plt <- as.data.frame(cfm$table)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
  
  ggplot(plt, aes(x = Reference,y= Prediction, fill= Freq)) +
    geom_tile() + geom_text(aes(label=Freq)) +
    scale_fill_gradient(low="white", high="#009194") +
    labs(x = "Actual",y = "Prediction") +
    scale_x_discrete(labels=label, position = "top") +
    scale_y_discrete(labels=rev(label))
}

predictions <- predict(fit.knn, validation)
cfm_knn <- confusionMatrix(factor(predictions), factor(validation$quality_names))
cfm_knn$overall['Accuracy']
plotConfusionMatrix(cfm_knn)


predictions <- predict(fit.svm, validation)
cfm_svm <- confusionMatrix(predictions, as.factor(validation$quality_names))
cfm_svm$overall['Accuracy']
plotConfusionMatrix(cfm_svm)


#Linear Discriminant Analysis (LDA)
set.seed(7)
fit.lda <- train(quality_names~., data=training, method="lda", metric=metric, trControl=control)
predictions <- predict(fit.lda, validation)
cfm_lda <- confusionMatrix(predictions, as.factor(validation$quality_names))
cfm_lda$overall['Accuracy']
plotConfusionMatrix(cfm_lda)

#Random Forest (RF)
set.seed(7)
fit.rf <- train(quality_names~., data=training, method="rf",metric=metric, trControl=control)
predictions <- predict(fit.rf, validation)
cfm_rf <- confusionMatrix(predictions, as.factor(validation$quality_names))
cfm_rf$overall['Accuracy']
plotConfusionMatrix(cfm_rf)

results <- resamples(list(knn=fit.knn, svm=fit.svm, lda=fit.lda, rf=fit.rf))
summary(results)

Wine = data.frame()

for (i in 1:5) {
  set.seed(i)
  fit.knn <- train(quality_names~., data=training, method="knn",metric=metric, trControl=control)
  predictions_1 <- predict(fit.knn, validation)
  cfm_knn <- confusionMatrix(predictions_1, as.factor(validation$quality_names))
  Wine[i,'KNN'] <- cfm_knn$overall['Accuracy']
  
  set.seed(i)
  fit.svm <- train(quality_names~., data=training,method="svmRadial", metric=metric, trControl=control)
  predictions_2 <- predict(fit.svm, validation)
  cfm_svm <- confusionMatrix(predictions_2, as.factor(validation$quality_names))
  Wine[i,'SVM'] <- cfm_svm$overall['Accuracy']
  
  set.seed(i)
  fit.lda <- train(quality_names~., data=training, method="lda",metric=metric, trControl=control)
  predictions_3 <- predict(fit.lda, validation)
  cfm_lda <- confusionMatrix(predictions_3, as.factor(validation$quality_names))
  Wine[i,'LDA'] <- cfm_lda$overall['Accuracy']
  
  set.seed(i)
  fit.rf <- train(quality_names~., data=training, method="rf",metric=metric, trControl=control)
  predictions_4 <- predict(fit.rf, validation)
  cfm_rf <- confusionMatrix(predictions_4, as.factor(validation$quality_names))
  Wine[i,'RF'] <- cfm_rf$overall['Accuracy']
}

Wine
mean_Wine <- summarise_if(Wine, is.numeric, mean)
mean_Wine
