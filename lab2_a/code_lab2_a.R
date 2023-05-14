rm(list=ls())

mydata <- read.csv("dataset/iris.data", header = FALSE)

iris <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Class")

colnames(mydata) <- iris

write.csv(mydata,file="Iris.csv",row.names = FALSE)