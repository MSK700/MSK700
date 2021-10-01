library(tidyverse)
library(corrplot)
library(data.table)
library(readr)
library(dplyr)
library(caret)
k = read.csv("k.csv" , header = T)
str(k)
summary(k) 
k %>% select_if(is.numeric) %>% cor %>% corrplot()
kw = k %>% 
  group_by(as.factor(product_id)) %>% 
  summarise(customer_id,list_price, standard_cost,Profit, frequency = n_distinct(transaction_id), transaction_date, wealth_segment, RFM)
kw = data.table(kw)
#k %>% 
  #group_by(as.factor(transaction_id),as.factor(product_id)) %>% 
  #summarise(frequency =n_distinct(SO.Number), monetary= sum(Gross.Profit))
View(kw)
kw %>% select_if(is.numeric) %>% cor %>% corrplot()
str(kw)
summary(k$product_id)
k$product_id = as.factor(k$product_id)
