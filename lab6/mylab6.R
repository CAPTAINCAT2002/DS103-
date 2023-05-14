rm(list=ls())
library('caret')
library('kernlab')
data <- read.csv("data/tidy_data.csv")

label <- unique(data$Activity)
summary(data)
percentage <- prop.table(table(data$Activity)) * 100
cbind(freq=table(data$Activity), percentage=percentage)

validation_index <- createDataPartition(data$Activity,p=0.80, list=FALSE)
validation <- data[-validation_index,]
training <- data[validation_index,]

control <- trainControl(method="cv", number=10,savePredictions = TRUE)
metric <- "Accuracy" # Do do danh gia

# kNN
set.seed(7)
fit.knn <- train(Activity.1~., data=training, method="knn",metric=metric, trControl=control)

# SVM
set.seed(7)
fit.svm <- train(Activity.1~., data=training, method="svmRadial", metric=metric, trControl=control)

results <- resamples(list(knn=fit.knn, svm=fit.svm))
summary(results)
fit.knn$pred
predictions <- predict(fit.knn, validation)
confusionMatrix(predictions, as.factor(validation$Activity.1))

