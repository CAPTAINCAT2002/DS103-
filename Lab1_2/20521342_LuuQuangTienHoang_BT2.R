rm(list=ls())
crData <- read.csv("dataset/covid_19_data.csv")
crData$ObservationDate <- as.Date(crData$ObservationDate, "%m/%d/%Y")

#Tim du lieu ve so ca lay nhiem tai VIETNAM
coronaVietnam <- crData[which(crData$Country.Region=='Vietnam'),]

#In ra so ca lay nhiem nhieu nhat tai VIETNAM
maxConfirmedvietnam <- max(coronaVietnam['Confirmed'])
print(maxConfirmedvietnam)

#Tim du lieu ve so ca lay nhiem tai VIETNAM trong thang 2
data_vn <- crData[which(crData$ObservationDate>="2021-01-01"& crData$ObservationDate<="2021-02-28"& crData$Country.Region=='Vietnam'), ]

#In ra so du lieu ve ca lay nhiem nhieu nhat trong thang 01 va 02 tai VIETNAM
max_vn <- max(data_vn['Confirmed'])
print(max_vn)


#In ra so du lieu ve ca lay nhiem nhieu nhat trong thang 01 va 02 tai Indonesia
data_id <- crData[which(crData$ObservationDate>="2021-01-01"& crData$ObservationDate<="2021-02-28"& crData$Country.Region=='India'), ]
max_id <- max(data_id['Confirmed'])
print(max_id)

#In ra so du lieu ve ca lay nhiem nhieu nhat trong thang 01 va 02 tai Singapore
data_sg <- crData[which(crData$ObservationDate>="2021-01-01"& crData$ObservationDate<="2021-02-28"& crData$Country.Region=='Singapore'), ]
max_sg <- max(data_sg['Confirmed'])
print(max_sg)


#In ra du lieu ve ca tu vong cua TQ trong thoi gian tu 1/2/2021 den 15/2/2021
tq <- crData[which(crData$Country.Region=='Mainland China'),]
deaths_tq <- crData[which(crData$ObservationDate>="2021-02-01"& crData$ObservationDate<="2021-02-15"& crData$Country.Region=='Mainland China'), ]
print(deaths_tq)

#So ca nhiem moi tai VN giua thang 5/2020 và thang 5/2021
data_may20_vn <- crData[which(crData$ObservationDate>="2020-05-01"& crData$ObservationDate<="2020-05-31"& crData$Country.Region=='Vietnam'), ]
data_may21_vn <- crData[which(crData$ObservationDate>="2021-05-01"& crData$ObservationDate<="2021-05-31"& crData$Country.Region=='Vietnam'), ]
plot(data_may20_vn,data_may21_vn)


