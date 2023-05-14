rm(list=ls())

data=read.csv('dataset/bank-full.csv', sep=';')

write.csv(data, 'tidy_bank-full.csv', row.names = FALSE)
