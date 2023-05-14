rm(list=ls())

library('xml2')
library('httr')
library('dplyr')

data <- GET("https://www.worldometers.info/coronavirus/#countries")

content <- content(data, as="text")
html_data <- read_xml(content, as_html = TRUE)

# Lay cac thuoc tinh trong tbody - du lieu cua bang
table_data <- xml_text(xml_find_all(html_data,"//table[@id='main_table_countries_today']//tbody//tr//td"))

# Lay cac thuoc tinh trong thead - tieu de cua bang
table_head <- xml_text(xml_find_all(html_data,"//table[@id='main_table_countries_today']//thead//tr//th"))

# So cot o day tuong ung so luong table_head
# So dong o day tuong ung so luong table_data
dataset <- matrix(ncol=length(table_head),nrow =(length(table_data)/ length(table_head) ))

# So luong cot trong bang
data_col = length(table_head)
# So luong dong trong bang
data_row = length(table_data)
# Lan luot lay tung dong trong table_data bo vao dataset
count = 1
i = 1
while(i<=length(table_data) - data_col) {
  dataset[count, ] <- c(table_data[i:(i -1 + data_col)])
  i = i + data_col;
  count = count + 1
}
# Chuyen ma tran dataset thanh dang dataframe
dataset <- as.data.frame(dataset)
dataset <- na.omit(dataset)
# gan tieu de cho dataframe
names(dataset) <- c(table_head)
# luu du lieu thanh csv
write.csv(dataset, "corona_data.csv")
#xoa du lieu 
data <- dataset[-c(1:8),]
data <- dataset[-c(237:243),]
View(data)

data$TotalCases <- as.numeric(gsub(",","",data$TotalCases,fixed=TRUE))
data$TotalDeaths <- as.numeric(gsub(",","",data$TotalDeaths,fixed=TRUE))
data$TotalRecovered <- as.numeric(gsub(",","",data$TotalRecovered,fixed=TRUE))
data$NewRecovered <- as.numeric(gsub(",","",data$NewRecovered,fixed=TRUE))
data$ActiveCases <- as.numeric(gsub(",","",data$ActiveCases,fixed=TRUE))
data$NewCases <- as.numeric(gsub(",","",data$NewCases,fixed=TRUE))
data$NewDeaths <- as.numeric(gsub(",","",data$NewDeaths,fixed=TRUE))
data$`Serious,Critical` <- as.numeric(gsub(",","",data$`Serious,Critical`,fixed=TRUE))
data$`Tot Cases/1M pop` <- as.numeric(gsub(",","",data$`Tot Cases/1M pop`,fixed=TRUE))
data$`Deaths/1M pop` <- as.numeric(gsub(",","",data$`Deaths/1M pop`,fixed=TRUE))
data$TotalTests <- as.numeric(gsub(",","",data$TotalTests,fixed=TRUE))
data$`Tests/1M pop` <- as.numeric(gsub(",","",data$`Tests/1M pop`,fixed=TRUE))
data$Population <- as.numeric(gsub(",","",data$Population,fixed=TRUE))
data$`1 Caseevery X ppl` <- as.numeric(gsub(",","",data$`1 Caseevery X ppl`,fixed=TRUE))
data$`1 Deathevery X ppl` <- as.numeric(gsub(",","",data$`1 Deathevery X ppl`,fixed=TRUE))
data$`New Cases/1M pop` <- as.numeric(gsub(",","",data$`New Cases/1M pop`,fixed=TRUE))
data$`Active Cases/1M pop` <- as.numeric(gsub(",","",data$`Active Cases/1M pop`,fixed=TRUE))
#cau1a
data$TotalCases <- sort(data$TotalCases, decreasing = TRUE)
five_highest_totalcases_country <- head(data, 5)
#cau1b
num[is.na(num)] <- 0
Max_NewCases <- data[which(data$NewCases>= max(num)),]
#cau1c
data$rateRecovered[is.na(data$rateRecovered)] <- 0

data['rateRecovered'] <- data$TotalRecovered/data$TotalCases
data <- data %>% arrange(desc(rateRecovered))
data$rateRecovered[is.na(data$rateRecovered)] <- 0
Top3_rate <- head(data,3)