rm(list=ls())

mydata <-read.csv('dataset/car.data', header = FALSE)

vab<-c('buying', 'maint', 'doors', 'person', 'lug_boot', 'safety', 'class')

colnames(mydata)<-vab

write.csv(mydata,"tidy_car.csv",row.names = FALSE)
