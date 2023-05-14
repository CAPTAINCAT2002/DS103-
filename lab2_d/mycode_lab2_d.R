rm(list=ls())


mydata <- read.csv('dataset/wine.data', header = FALSE)


vab=c('Alcohol','Malic acid','Ash','Alcalinity of ash','Magnesium','Total phenols','Flavanoids','Nonflavanoid phenols','Proanthocyanins','Color intensity','Hue','OD280/OD315 of diluted wines','Proline','Class')



colnames(mydata) <- vab


write.csv(mydata,'tidy_wine.csv', row.names = FALSE)
