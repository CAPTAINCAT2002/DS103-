rm(list=ls())
library('dplyr')
library('lubridate')
library('tidyr')
library('magrittr')
library('tidyverse')

#Bai1 Thuc hien lai vi du bai mau
dataset <- read.csv("dataset/covid_19_data.csv")
dataset$ObservationDate <- as.Date(dataset$ObservationDate,"%m/%d/%Y")
dataset %>% select(ObservationDate, Country.Region, Confirmed, Deaths)
head(dataset %>% select(ObservationDate, Country.Region,Confirmed, Deaths), 10)
dataset %>% filter(Country.Region == "Mainland China")
dataset %>% filter(Country.Region == "Mainland China" &ObservationDate >= "2020-03-01" & ObservationDate <="2020-04-30")
dataset %>% filter(Country.Region == "Mainland China") %>%summarise(Mean=mean(Confirmed, na.rm = TRUE),
    Median = median(Confirmed, na.rm = TRUE),
    Variance = var(Confirmed, na.rm = TRUE),
    SD = sd(Confirmed, na.rm = TRUE))

dataset %>% filter(
  Country.Region == "Vietnam" &
    ObservationDate >= "2020-03-01" &
    ObservationDate <= "2020-04-30") %>%
  group_by(ObservationDate)
dataset %>% filter(
  Country.Region == "Vietnam" &
    ObservationDate >= "2020-03-01" &
    ObservationDate <= "2020-04-30") %>%
  group_by(ObservationDate) %>%
  arrange(Confirmed)


print(dataset[1,])
leap_year(dataset$ObservationDate[1])

year(dataset$ObservationDate[1])
month(dataset$ObservationDate[1])
day(dataset$ObservationDate[1])
now()
make_difftime(day=5)
dataset %>% filter(Country.Region == "Vietnam" &
                  month(ObservationDate) %in% c(1,3))
dataset %>% filter(Country.Region == "Vietnam" &
                  wday(ObservationDate, label=TRUE) == "Wed")

#1b
dataset['NewCase'] <-0
japan <- dataset %>% filter(
  Country.Region=='Japan'&
  ObservationDate >='2021-03-02'&
  ObservationDate <= '2021-03-15') %>%
  group_by(Province.State) %>%
  summarise(NewCase = Confirmed - lag(Confirmed, default = first(Confirmed)), Date=ObservationDate)
japan_date <- japan %>% group_by(Date) %>% summarise(sum(NewCase))
par(mar= c(1,1))
plot(
  x=japan_date$Date,
  y=japan_date$`sum(NewCase)`,
  type='l',
  main='Total Confirmed Japan by Date'
)

#1c
us <- dataset %>% filter(
  Country.Region=='US'&
  ObservationDate >='2021-03-15'&
  ObservationDate <= '2021-04-15') %>%
  group_by(Province.State) %>%
  summarise(NewCase = Confirmed - lag(Confirmed,default = first(Confirmed)),Date=ObservationDate)
us_date <- us %>% group_by(Date) %>% summarise(sum(NewCase))
us_province <- us %>% group_by(Province.State) %>% summarise(sum(NewCase)) 
matrix(unlist(us_province),ncol=58,nrow=2)
province_us = us_province$Province.State

plot(x=us_date$Date,
     y=us_date$`sum(NewCase)`,
     type='l',main='Total Confirmed US by province')

barplot(us_province$`sum(NewCase)`,
        main= 'Total Confirmed US by province',
        names.arg = province_us,
        xlab='Province',
        ylab='Confirmed',
        horiz = FALSE)
#Bai2
world <- dataset %>% filter(ObservationDate >= '2020-02-01'& ObservationDate <= '2020-04-30') %>%
                              group_by(ObservationDate) %>% 
                              summarise(TotalConfirmed =sum(Confirmed))%>%
                              mutate(NewCase=TotalConfirmed -lag(TotalConfirmed, default = first(TotalConfirmed)))

ggplot(dataset=world, aes(x=ObservationDate,y=NewCase,group=1))+geom_line(color='green') + geom_point()
plot(world$ObservationDate, world$NewCase, type='l')

#bai3
    vn_2021 <- dataset %>% filter (
    Country.Region=='Vietnam' &
    ObservationDate >= '2021-01-01'&
    ObservationDate <= '2021-12-31') %>%
    summarise(NewCase = Confirmed - lag(Confirmed,default = first(Confirmed)),date=ObservationDate)


 vn_2021 <- vn_2021 %>% mutate(year=year(date),month= month(date),day=day(date))
covidvn_month <- vn_2021 %>% group_by(month) %>% summarise(sum(NewCase))
par(mar=c(1,1,1,1))
plot (x= covidvn_month$month,
      y=covidvn_month$`sum(NewCase)`,
      ylab='NewCase',
      type='l',main='Total NewCase VN in 2021 grouped by month',xaxt='n')
axis(1,at=covidvn_month$month,las=2)

library(ggplot2)
ggplot(data=covidvn_month,aes( x=month, y=`sum(NewCase)`, group=1)) + geom_line(color='red')+geom_point()

#bai4
  
vn_2020 <- dataset %>% filter (
  Country.Region=='Vietnam'&
    ObservationDate >= '2020-01-01'&
    ObservationDate <= '2020-12-31') %>%
  summarise(NewCase=Confirmed - lag(Confirmed, default = first(Confirmed)),date=ObservationDate)

vn_2020_4 <- vn_2020 %>% filter(month(date)==04) %>% 
  mutate (days=wday(date,label=TRUE))%>% 
  group_by(days) %>% 
  summarise(sum(NewCase))
ggplot(data=vn_2020_4, aes(x=days, y=`sum(NewCase)`,group=1))+geom_line(color='red')+geom_point()

#bai5
vn_month_1_3_2020 <- vn_2020 %>% 
                    filter(date >='2020-01-01'&date<='2020-03-31')
vn_month_1_3_2021 <- vn_2021 %>% 
  filter(date >='2021-01-01'&date<='2021-03-31') %>%
  subset(select= -c(year,month,day))
t1 <- ggplot(vn_month_1_3_2020,aes(date,NewCase,color='blue'))+
  labs(title='Confirmed in VN from JP to March in 2020')+
  geom_point(size=1)+
  geom_quantile(quantiles=0.5)

t2 <- ggplot(vn_month_1_3_2021,aes(date,NewCase,color='blue'))+
  labs(title='Confirmed in VN from JP to March in 2021')+
  geom_point(size=1)+
  geom_quantile(quantiles=0.5)

  
library(gridExtra)
  grid.arrange(t1,t2,nrow=1)

#Bai6
  
vn_4_2019 <-dataset %>% filter(Country.Region=='Vietnam' &month(ObservationDate)==4 & year(ObservationDate)==2019) %>%
            summarise(NewCase=Confirmed - lag(Confirmed, default = first(Confirmed)))
  
vn_4_2020 <-dataset %>% filter(Country.Region=='Vietnam' &month(ObservationDate)==4 & year(ObservationDate)==2020) %>%
  summarise(NewCase=Confirmed - lag(Confirmed, default = first(Confirmed)))  

vn_4_2021 <-dataset %>% filter(Country.Region=='Vietnam' &month(ObservationDate)==4 & year(ObservationDate)==2021) %>%
  summarise(NewCase=Confirmed - lag(Confirmed, default = first(Confirmed)))

vn_compare_04 <- c(sum(vn_4_2019$NewCase),sum(vn_4_2020$NewCase),sum(vn_4_2021$NewCase))

par(mar=c(1,1,1,1))
Year <- c("2019","2020","2021")
barplot(vn_compare_04,main='Compare NewCase between 4/2019, 4/2020, 4/2021',
        names.arg=Year, horiz = FALSE)
  
dt <- data.frame(Year,vn_compare_04)
ggplot(dt,aes(x=Year,y=vn_compare_04))+ labs(title = 'Compare NewCase between 4/2019, 4/2020, 4/2021') + geom_bar(stat='identity')

#Bai7
par(mfrow=c(1,2))
boxplot(vn_month_1_3_2020$NewCase,
        main='NewCase from Jan to March 2020',
        ylab='counts',
        xlab='date',
        horizontal = FALSE,
        col='blue',
        border = 'brown')  
boxplot(vn_month_1_3_2021$NewCase,
        main='NewCase from Jan to March 2021',
        ylab='counts',
        xlab='date',
        horizontal = FALSE,
        col='red',
        border = 'brown')  
b1 <- ggplot(vn_month_1_3_2020,aes(y=NewCase))+geom_boxplot(outlier.colour = 'blue')+labs(title = 'NewCase from Jan to March 2020')
b2 <- ggplot(vn_month_1_3_2021,aes(y=NewCase))+geom_boxplot(outlier.colour = 'red')+labs(title = 'NewCase from Jan to March 2021')



#lam tren lop @@@@
  
#a <- filter(dataset, Country.Region=="Mainland China")
#b <- filter(a, ObservationDate >="2021-02-01"&ObservationDate <= "2021-02-28")
#c <- select(b,c(ObservationDate,Confirmed,Country.Region))

#c <- dataset %>% filter(Country.Region=="Mainland China") %>% 
                # filter(ObservationDate >= "2021-02-01" & ObservationDate <="2021-02-28") %>% 
                # select(c(ObservationDate, Confirmed, Country.Region))

c <- dataset %>% filter(Country.Region=="Mainland China") %>% 
     filter(month(ObservationDate) == 02 & year(ObservationDate) ==2021) %>% 
      select(c(ObservationDate, Confirmed, Country.Region))

vn_month_04_2020 <- dataset %>% filter(Country.Region=='Vietnam') %>% 
               filter(month(ObservationDate)==3 & year(ObservationDate)==2020) %>%
               mutate(NewConfirmed=Confirmed - lag (Confirmed, default =first(Confirmed))) %>%
               separate(ObservationDate,c('Year','Month','Date'),sep='-')

vn_month_04_2021 <- dataset %>% filter(Country.Region=='Vietnam') %>% 
  filter(month(ObservationDate)==4 & year(ObservationDate)==2021) %>%
  mutate(NewConfirmed=Confirmed - lag (Confirmed, default =first(Confirmed))) %>%
  separate(ObservationDate,c('Year','Month','Date'),sep='-')


 plot(vn_month_04_2020$Date,vn_month_04_2020$NewConfirmed,type='l',xaxt='n',yatx='n')
 axis(side=1,at=vn_month_04_2020$Date)
 axis(side=2,at=vn_month_04_2020$NewConfirmed)
 
 
 plot(vn_month_04_2021$Date,vn_month_04_2021$NewConfirmed,type='l',axes=FALSE)
 axis(side=1,at=vn_month_04_2021$Date)
 axis(side=2,at=vn_month_04_2021$NewConfirmed)
 
 world_covid <- dataset %>% filter(month(ObservationDate)%in%c(2:4)&year(ObservationDate)==2020) %>%
                            group_by(ObservationDate) %>%
                            summarise(TotalConfimed=sum(Confirmed)) %>%
                            mutate(New_confirmed = TotalConfimed,default=first(TotalConfimed))
                            
plot(world_covid$ObservationDate,world_covid$New_confirmed,type='l')




                            