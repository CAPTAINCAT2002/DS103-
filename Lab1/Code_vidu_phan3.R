rm(list=ls())
data <- read.csv('dataset/covid_19_data.csv')

data$ObservationDate <- as.Date(data$ObservationDate, "%m/%d/%Y")

data_2021 <- data[which(data$ObservationDate>="2020-01-01"&data$ObservationDate<="2020-01-31"), ]

ncol(data)

nrow(data)

head(data,15)

country <- data['Country.Region']
country <- unique(data['Country.Region'])

china <- data[which(data$Country.Region=='Mainland China' & data$Province.State=="Hubei"),]
vietnam <- data[which(data$Country.Region=='Vietnam'),]



new_data <- data[c('Country.Region','Confirmed')]

# lay ra tinh co so ca nhiem nhieu nhat cua TQ

maxConfirmed <- max(china['Confirmed'])

china[which(china['Confirmed']==maxConfirmed),]['Province.State']

max_province_confirmed <- unique(china[which(china['Confirmed']==maxConfirmed),]['Province.State'])

# Lay ra bang co so ca nhiem nhieu nhat cua Hoa Ky 

us <- data[which(data$Country.Region=='US'),]

max_conf <- max(us['Confirmed'])

max_province_conf <- unique(us[which(us['Confirmed']==max_conf),]['Province.State'])


# Lay ra bang co so ca tu vong nhieu nhat HoaKy

max_deaths <- max(us['Deaths'])

max_province_deaths <- unique(us[which(us['Deaths']==max_deaths),][c('Province.State','Deaths')])


data$ObservationDate <-as.Date(coronaData$ObservationDate, "%m/%d/%Y")



