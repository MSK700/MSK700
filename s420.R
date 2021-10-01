s33 = read.csv("pr21.csv", header = T)
str(SALE21)
summary(SALE21)
getwd()
str(s33)
library(ggplot2)
library(ggplot)
library(corrplot)
library(data.table)
library(readr)
library(dplyr)
library(stringr)
library(stringi)
sRFM <- s33 %>% 
  group_by(Customer.Name) %>% 
  summarise(recency=as.numeric(analysis_date-max(Date)),
            frequency =n_distinct(SO.Number), monetary= mean(Gross.Profit), sum(Gross.Profit),mean(Quantity..MT.))
#s33 %>% group_by(Date) %>% summarise(SO.Number,Invoice.Number,Customer.Name,Sales.Segment,Oder.Type,)
sall <- s33 %>% 
  group_by(SO.Number) %>% 
  summarise(Date, Customer.Name, Sales.Person,
            (Gross.Profit), (Quantity..MT.),SALE.RATE, COST.RATE, gpt = (Gross.Profit/Quantity..MT.),
            gpt2 = SALE.RATE - COST.RATE,gpm =gpt2/SALE.RATE,  Description,Oder.Type,Sales.Segment) %>% distinct() 
sall$SO.Number = as.factor(sall$SO.Number)



View (sall %>% quantile(sall$gpm, probs=c(.05, .75), na.rm = FALSE))
sall$gppm = quantile(sall$gpm, probs=c(.05, .75), na.rm = FALSE)
View(sall)
str(s33)
View(s33)
ggplot(sall, aes(x = Oder.Type, y = gpt, fill = Sales.Person)) + geom_boxplot()
View(sall %>% filter(gpt > 14023.1))
View(sall %>% filter(gpt < 0))
summary(sall$gpm)
Q <- quantile(sall$gpm, probs=c(.05, .75), na.rm = FALSE)
boxplot(Q)
boxplot(sall$gpm)