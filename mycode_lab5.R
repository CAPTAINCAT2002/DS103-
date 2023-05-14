rm(list=ls())
library('dplyr')

if (!file.exists('data')){
  dir.create('data')
}

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip"
download.file(fileURL,destfile = 'data/human_activity.zip')
unzip('data/human_activity.zip', exdir='data')
date_of_download <-date()

features <- read.table("data/UCI HAR Dataset/features.txt",col.names = c('ID','Features'))
activity_label <- read.table('data/UCI HAR Dataset/activity_labels.txt',col.names = c('ID','Features'))

x_train <- read.table('data/UCI HAR Dataset/train/x_train.txt')
names(x_train) <- features$Features
y_train <- read.table('data/UCI HAR Dataset/train/y_train.txt',col.names = c('ID'))
subject_train <- read.table('data/UCI HAR Dataset/train/subject_train.txt',col.names = c("Subject"))

x_test <- read.table('data/UCI HAR Dataset/test/x_test.txt')
names(x_test) <- features$Features
y_test <- read.table('data/UCI HAR Dataset/test/y_test.txt',col.names = c('ID'))
subject_test <- read.table('data/UCI HAR Dataset/test/subject_test.txt',col.names = c("Subject"))

#gop cac bang train va test lai voi nhau tao thanh 1 bang lon gom 563 cot va 7352 dong

fulltrain <- cbind(y_train,subject_train,x_train)
fulltest  <- cbind(y_test,subject_test,x_test)

#cbind la gop cot
#rbind la gop dong

full <- rbind(fulltrain,fulltest)

name1 <- grep('mean\\(\\)',names(full))
mean_features_name <- names(full)[name1]
mean_features <- full %>% select(ID,Subject,mean_features_name)

name2 <- grep("std\\(\\)",names(full))
std_features_names <- names(full)[name2]
std_features <-full %>% select(std_features_names)

newdata <- cbind(mean_features,std_features)

newdata <- merge(newdata,activity_label,by.x = 'ID',by.y = 'ID')

colnames(newdata) <- sub(x=colnames(newdata),pattern = '^t',replacement = 'Time domain:')
colnames(newdata) <- sub(x=colnames(newdata),pattern = '^f',replacement = 'Frequency domain:')
colnames(newdata) <- sub(x=colnames(newdata),pattern = '-',replacement = ',')
colnames(newdata) <- sub(x=colnames(newdata),pattern = 'mean\\(\\)',replacement = 'mean value')
colnames(newdata) <- sub(x=colnames(newdata),pattern = 'std\\(\\)',replacement = 'standard deviation value')
colnames(newdata) <- sub(x=colnames(newdata),pattern = '-X',replacement = 'on X axis')
colnames(newdata) <- sub(x=colnames(newdata),pattern = '-Y',replacement = 'on Y axis')
colnames(newdata) <- sub(x=colnames(newdata),pattern = '-Z',replacement = 'on Z axis')
colnames(newdata) <- sub(x=colnames(newdata),pattern = 'AccJerk',replacement = 'acceleration jerk')
colnames(newdata) <- sub(x=colnames(newdata),pattern = 'Acc',replacement = 'acceleration')
colnames(newdata) <- sub(x=colnames(newdata),pattern = 'GyroJerk',replacement = 'angular velocity jerk')
colnames(newdata) <- sub(x=colnames(newdata),pattern = 'Mag',replacement = 'magnitude')
colnames(newdata) <- sub(x=colnames(newdata),pattern = 'Gyro',replacement = 'angular velocity')

tidy <- aggregate(newdata[,3:68],list(newdata$ID,newdata$Subject),FUN=mean)

write.csv(tidy,file='tidyyyy.csv')