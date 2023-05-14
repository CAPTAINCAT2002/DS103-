rm(list=ls())
library(gsheet)
link_patien <- "https://docs.google.com/spreadsheets/d/1XEFg047aSbg3OsEVx9PzmgSxGbCvCidfLiHfsgRS3R0/edit#gid=0"
data <- gsheet2tbl(link_patien)
# số ca nhiễm theo ngày
databydate <- table(data$Date.Announced)
barplot(databydate)

#cau3a Liet ke so ca nhiem theo tung thanh pho
newdata = subset(data, Detected.City!="")

#ham subset gom theo yeu cau thanh cac dong gan nhau
newdata$Prefecture.Patient.Number <- as.numeric(newdata$Prefecture.Patient.Number)
Patient_of_Detected.city <- aggregate(newdata$Prefecture.Patient.Number, by= list(newdata$Detected.City), FUN=sum)


# Tong hop lai tong cac gia tri cac hang cua thuoc tinh per
variables <- c("Detected.city","Prefecture.Patient.Number")

# Dat ten cho cot trong bo du lieu
colnames(Patient_of_Detected.city) <- variables

#cau3b liet ke so ca nhiem theo do tuoi
list(unique(data3$Age.Bracket))
data$Age.Bracket <- gsub("under10", "Under10", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("Under10", "Under 10", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("Under10", "Under 10", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("above 90", "Over 90", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("0ver 90", "Over 90", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("elderly", "Over 90", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("", "Unspecified", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("-", "Unspecified", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("M", "Unspecified", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("F", "Unspecified", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("Unspecifed", "Unspecified", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("unspecified", "Unspecified", data$Age.Bracket, fixed=TRUE)
data$Age.Bracket <- gsub("Undisclosed", "Not public", data$Age.Bracket, fixed=TRUE)

data$Prefecture.Patient.Number <- as.numeric(data$Prefecture.Patient.Number)
typeof(data$Age.Bracket)
Age.Bracket <- aggregate(data$Prefecture.Patient.Number, by= list(data$Age.Bracket), FUN=sum)
variables <- c("Age.Bracket","Prefecture.Patient.Number")
# Dat ten cho cot trong bo du lieu
colnames(Age.Bracket) <- variables
plot(Age.Bracket$Age.Bracket,Age.Bracket$Prefecture.Patient.Number,type = "l",col="red")

#cau3c liet ke so ca nhiem tai Hokkaido theo tung ngay

Hokkaido<- newdata[which(newdata$Detected.Prefecture=="Hokkaido"),][c("Date.Announced","Prefecture.Patient.Number")]
Hokkaido$Date.Announced <- as_datetime(anytime(Hokkaido$Date.Announced))

plot(Hokkaido$Date.Announced, Hokkaido$Prefecture.Patient.Number, type='l', main="Hokkaido", labels=FALSE)
axis.POSIXct(1, at=anytime(Hokkaido$Date.Announced), format="%y-%m-%d", origin="1970-01-01", labels=TRUE)
axis(2,at=Hokkaido$Date.Announced)