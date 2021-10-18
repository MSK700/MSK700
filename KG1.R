#Basic data structure on data

transactions = read.csv('E:/Saad/Parhai/Go remote/kpmg/transaction.csv', header = T)
str(transactions)
summary(transactions)
summary(complete.cases(transactions))
19445/(19445+555)

CustomerAddress = read.csv('E:/Saad/Parhai/Go remote/kpmg/CustomerAddress.csv', header = T)
str(CustomerAddress)
summary(CustomerAddress)
summary(complete.cases(CustomerAddress))


CustomerDemogrphics = read.csv('E:/Saad/Parhai/Go remote/kpmg/CustomerDemogrpahic.csv', header = T)
str(CustomerDemogrphics)
summary(CustomerDemogrphics)
summary(complete.cases(CustomerDemogrphics))
3913/4000

#=======#
lal = read.csv('E:/Saad/Parhai/Go remote/kpmgr.csv', header = T)

install.packages('corrplot')
install.packages('ggplot')
library(ggplot2)
library(ggplot)
library(corrplot)
library(dplyr)

lal %>% select_if(is.numeric) %>% select(-transaction_id) %>% cor() %>% corrplot()

ggplot(lal)+geom_boxplot(aes(x= wealth_segment,y = profit))
ggplot(lal)+geom_boxplot(aes(x= wealth_segment,y = past_3_years_bike_related_purchases))
ggplot(lal)+geom_boxplot(aes(x= customer_id ,y = profit))
ggplot(lal) + geom_bar(aes(x = wealth_segment , fill = age))
ggplot(lal) + geom_bar(aes(x = job_industry_category , fill = profit))
ggplot(lal) + geom_bar(aes(x = product_id , fill = gender))
ggplot(lal) + geom_bar(aes(x = wealth_segment , fill = gender))
plot(lal$transaction_date, lal$profit)

m2 = lal %>% select_if(is.numeric)
m3 = as.matrix(m2)
heatmap(m3)

df <- data.frame(lal$customer_id, lal$profit, lal$transaction_date)
df <- df[complete.cases(df),]

lal$transaction_date <- as.Date(as.character(lal$transaction_date),'%Y-%m-%d')
lal$transaction_date = as.Date(lal$transaction_date)

colnames(df)[colnames(df)=="lal.customer_id"] <- "customerID"
colnames(df)[colnames(df)=="lal.profit"] <- "amount"
colnames(df)[colnames(df)=="lal.transaction_date"] <- "date"
rfm_table_customer(df, customer_id = customer_id, )

df_RFM <- lal %>% 
  group_by(customer_id) %>% 
  summarise(recency=as.numeric(analysis_date-max(tds)),
            frequency =n_distinct(transaction_id), monetary= sum(profit))
write.csv(df_RFM, file = "kpmgsumary.csv")

#RECENCY TO BE CALCULATED MANUALLY BY MIN AMOUNT OF (31/12/2017 - TRANSACTION DATE)

#=================================================================#

pop = read.csv('E:/Saad/Parhai/Go remote/kpmgs.csv', header = T)
summary(pop)
str(pop)
pop$recency = is.integer(pop$recency)
pop$customer_id =is.factor(pop$customer_id)


#Scoring
#R_score
pop$R_Score[pop$recency>88.0]<-1
pop$R_Score[pop$recency>45 & pop$recency<=88.0 ]<-2
pop$R_Score[pop$recency>18 & pop$recency<=45 ]<-3
pop$R_Score[pop$recency<=18]<-4
#F_score
pop$F_Score[pop$frequency<4]<-1
pop$F_Score[pop$frequency>=4 & pop$frequency<5]<-2
pop$F_Score[pop$frequency>=5 & pop$frequency<7 ]<-3
pop$F_Score[pop$frequency>=7]<-4
#M_score
pop$M_Score[pop$monetary<= 1796.22]<-1
pop$M_Score[pop$monetary>=1796.22 & pop$monetary<2792.80]<-2
pop$M_Score[pop$monetary>=2792.80 & pop$monetary<4102.99 ]<-3
pop$M_Score[pop$monetary>=4102.99]<-4
#RFM_score
pop<- pop %>% mutate(RFM_Score = 100*R_Score + 10*F_Score+M_Score)

str(pop)
write.csv(pop, file = "listKPMG-RFMscore.csv")

ggplot(lal) + geom_bar(aes(x = wealth_segment , fill = gender))
plot(pop$recency, pop$monetary)
ggplot(pop, aes(x=pop$recency, y=pop$monetary)) + geom_point() 
ggplot(pop, aes(x=pop$monetary, y=pop$frequency)) + geom_point() 
ggplot(pop, aes(x=pop$frequency, y=pop$recency)) + geom_point() 
ggplot(data = pop, aes(x = CYLINDERS, fill = OIGIN)) + geom_bar()
ez %>% ggplot(mtcars, aes(x=recency, y=monetary, color=Segment)) + geom_point() + geom_rug()

ez = read.csv('E:/Saad/Parhai/Go remote/kpmgsumary.csv', header = T)
str(ez)
ggplot(ez, aes(x=ez$recency, y=ez$monetary, color=ez$Segment)) + geom_point() + geom_density_2d()
ggplot(ez) + geom_bar(aes(x = RFM_Score , fill = Segment))
ggplot(ez)+geom_boxplot(aes(x= Segment,y = Profit))

ggplot(ez, aes(x=ez$recency, y=ez$monetary, color=ez$Segment)) + geom_point() 
ggplot(ez, aes(x=ez$monetary, y=ez$frequency, color=ez$Segment)) + geom_point() 
ggplot(ez, aes(x=ez$frequency, y=ez$recency, color=ez$Segment)) + geom_point()

