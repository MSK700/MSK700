library(ggplot2)
library(ggplot)
library(corrplot)
library(data.table)
library(readr)
library(dplyr)
SALE = read.csv("SALE.csv", header = T)
str(SALE)
SALE$SO.Date = as.Date(SALE$SO.Date, "%d-%m-%y")
summary(SALE)
SALE[sapply(SALE, is.character)] <- lapply(SALE[sapply(SALE, is.character)], 
                                         as.factor)
SALE$SO.MONTH = as.factor(months.Date(SALE$SO.Date))
summary(SALE$SO.MONTH)
ggplot(SALE, aes(x = SO.MONTH, y = Gross.Profit, fill = Oder.Type)) + geom_boxplot()
ggplot(SALE, aes(x = `Sales.Person`, y = `Gross.Profit`, fill=`Sales.Person`)) + 
  geom_bar(stat="identity") + theme_minimal()
ggplot(SALE, aes(x = `Sales.Segment`, y = `Gross.Profit`, fill=`Sales.Segment`)) + 
  geom_bar(stat="identity") + theme_minimal()
ggplot(SALE, aes(x = `Transaction.Type`, y = `Gross.Profit`, fill=`Transaction.Type`)) + 
  geom_bar(stat="identity") + theme_minimal()
ggplot(SALE, aes(x = Sales.Segment, y = Gross.Profit, fill = Sales.Segment)) + geom_boxplot()
ggplot(SALE, aes(x = Sales.Person, y = Gross.Profit, fill = Sales.Person)) + geom_boxplot()
ggplot(SALE, aes(x = Oder.Type, y = Gross.Profit, fill = Oder.Type)) + geom_boxplot()


View(SALE)
summary(SALE$Oder.Type)

SALE %>% group_by(Sales.Person) %>% 
  summarise(Min_Value = min(Gross.Profit), Max_Value = max(Gross.Profit), Sum_Value = sum(Gross.Profit), Avg_Value =mean(Gross.Profit), Median =median(Gross.Profit))
SALE %>% group_by(Quantity..MT.) %>% 
  summarise(Min_Value = min(Gross.Profit), Max_Value = max(Gross.Profit), Sum_Value = sum(Gross.Profit), Avg_Value =mean(Gross.Profit), Median =median(Gross.Profit))
SALE %>% group_by(SALE.RATE) %>% 
  summarise(Min_Value = min(Gross.Profit), Max_Value = max(Gross.Profit), Sum_Value = sum(Gross.Profit), Avg_Value =mean(Gross.Profit), Median =median(Gross.Profit))
SALE %>% group_by(Sales.Segment) %>% 
  summarise(Min_Value = min(Gross.Profit), Max_Value = max(Gross.Profit), Sum_Value = sum(Gross.Profit), Avg_Value =mean(Gross.Profit), Median =median(Gross.Profit))

SALE$SO.Number = as.factor(SALE$SO.Number)

analysis_date = max(KOP$DATE)
df_RFM <- KOP %>% 
  group_by(LYLTY_CARD_NBR) %>% 
  summarise(recency=as.numeric(analysis_date-max(DATE)),
            frequency =n_distinct(TXN_ID), monetary= sum(TOT_SALES))
summary(df_RFM)

analysis_date = max(SALE$SO.Date)
CRFM <- SALE %>% 
  group_by(Customer.Name) %>% 
  summarise(recency=as.numeric(analysis_date-max(SO.Date)),
            frequency =n_distinct(SO.Number), monetary= sum(Gross.Profit))
summary(CRFM)
str(CRFM)
View(CRFM)
#Scoring
#R_score
CRFM$R_Score[CRFM$recency>111.00]<-1
CRFM$R_Score[CRFM$recency>43.00 & CRFM$recency<=111.00 ]<-2
CRFM$R_Score[CRFM$recency>43.00 & CRFM$recency<=43.00 ]<-3
CRFM$R_Score[CRFM$recency<=43.00]<-4
#F_score
CRFM$F_Score[CRFM$frequency<1.000]<-1
CRFM$F_Score[CRFM$frequency>=1.000 & CRFM$frequency<3.000]<-2
CRFM$F_Score[CRFM$frequency>=3.000 & CRFM$frequency<8.000 ]<-3
CRFM$F_Score[CRFM$frequency>=8.000]<-4
#M_score
CRFM$M_Score[CRFM$monetary<= 74349]<-1
CRFM$M_Score[CRFM$monetary>=74349 & CRFM$monetary<367091]<-2
CRFM$M_Score[CRFM$monetary>=367091 & CRFM$monetary<1187212 ]<-3
CRFM$M_Score[CRFM$monetary>=1187212]<-4
#RFM_score
CRFM<- CRFM %>% mutate(CRFM_Score = 100*R_Score + 10*F_Score+M_Score)
CFRM$CATEGORY = as.data.frame(CFRM$CATEGORY)
CRFM$CATEGORY[CRFM$CRFM_Score == c(444) ] <-"Champion"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(334,342,343,344,433,434,443) ] <-"Loyal Customer"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(341,332,333,412,413,414,431,432,441,442,421,422,423,424) ] <-"Potential Loyalist"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(411) ] <-"Recent Customer"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(311,312,313,331) ] <-"Promising"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(212,213,214,231,232,233,241,314,321,322,323,324) ] <-"Needing Attention"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(211) ] <-"About to Sleep"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221) ] <-"At Risk"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(134,143,144,234,242,243,244) ] <-"Cant Loose"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(141) ] <-"Hibernating"
CRFM$CATEGORY[CRFM$CRFM_Score %in% c(111) ] <-"Lost"

CRFM[sapply(CRFM, is.character)] <- lapply(CRFM[sapply(CRFM, is.character)], 
                                           as.factor)
CRFM %>% filter(CATEGORY == "Champion")
str(CRFM$CRFM_Score)
CRFM = CRFM[,c(-9,-10)]
CRFM

analysis_date

#CRFM %>% group_by(CATEGORY) %>% 
  #summarise(Min_Value = min(monetary), Max_Value = max(monetary), Sum_Value = sum(monetary), Avg_Value =mean(monetary), Median =median(monetary))
ggplot(CRFM, aes(x = CATEGORY, y = monetary, fill = CATEGORY)) + geom_boxplot()
ggplot(sl, aes(x = CATEGORY, y = Gross.Profit, fill = BRAND)) + geom_boxplot()
ggplot(CRFM, aes(x = CATEGORY, y = recency, fill = CATEGORY)) + geom_boxplot()
ggplot(CRFM, aes(x = CATEGORY, y = frequency, fill = CATEGORY)) + geom_boxplot()
ggplot(sl) + geom_bar(aes(x = CATEGORY , fill = Sales.Segment))

sl = merge(SALE, CRFM)
str(sl)
ggplot(sl, aes(x=SO.Date, y=Gross.Profit, group=CATEGORY)) +geom_line(aes(color=CATEGORY))
ggplot(sl, aes(x=SO.Date, y=Gross.Profit, group=Sales.Segment)) +geom_line(aes(color=Sales.Segment))
ggplot(sl, aes(x = `Sales.Person`, y = `Gross.Profit`, fill=`CATEGORY`)) + 
  geom_bar(stat="identity") + theme_minimal()
ggplot(sl, aes(x = `CATEGORY`, y = `Gross.Profit`, fill=`BRAND`)) + 
  geom_bar(stat="identity") + theme_minimal()
#Now the real party
CRFM %>% filter(CATEGORY == "At Risk")
CRFM %>% filter(CATEGORY == "At Risk")
sl2 %>% filter(CATEGORY == "Champion")


#we assume that the "at risk" is actually lost, from that we start to develop a churn prediction model
sl$churn[sl$CATEGORY == "At Risk" ] <-"Churn"
sl$churn[sl$CATEGORY != "At Risk" ] <-"Not Churn"
sl$churned[sl$CATEGORY == "At Risk" ] <- 1
sl$churned[sl$CATEGORY != "At Risk" ] <- 0

str(sl)
sl$churn = as.factor(sl$churn)
summary(sl$churn)

#little extra
sl$Description = as.character(sl$Description)
productWords <- data.table(unlist(strsplit(unique(sl$Description), "")))
summary(productWords)
setnames(productWords, 'words')
library(stringr)
library(stringi)
productWords$words <- str_replace_all(productWords$words,"[[:punct:]]"," ")
productWords$words <- str_replace_all(productWords$words,"[0-9]"," ")
productWords$words <- str_replace_all(productWords$words,"[gG]"," ")
wordsfreq <- words.freq[order(words, decreasing = T),]
#till here
#sl$BRAND <- gsub("([A-Za-z]+).*", "\\2", sl$Description)
sl$BRAND = word(sl$Description, 1,7, sep=" ")
summary(sl)
sl$PACKAGE_SIZE = word(sl$Description, 8,10 , sep=" ")

sl$BRAND = as.factor(sl$BRAND)
sl$PACKAGE_SIZE = as.factor(sl$PACKAGE_SIZE)
View(sl)
sl2 = na.omit(sl)
str(sl2)
View(sl2)
sl2 = sample_n(sl2,6744)
summary(sl2$Description)
0.75*6744
train = sl2[1:5058,]
test = sl2[5059:6744,]

#model1 making
logit <- glm(churned ~ SO.Date + Sales.Segment + Oder.Type + Sales.Person + 
               Transaction.Type + recency + monetary + frequency + BRAND + PACKAGE_SIZE, data = train, family = 'binomial')
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

p = predict(logit, data = train, type = "response")
p2 = as.factor(ifelse(p > 0.8, "Churn", "Not Churn"))
confusionMatrix(p2,train$churn)
1-((2+137)/(396+4523))

#===================================================#
  
#proxy model1
logif <- glm(churned ~ SO.Date + Sales.Segment + Oder.Type + Sales.Person + 
               Transaction.Type + recency + monetary + frequency + BRAND + PACKAGE_SIZE, data = sl2, family = 'binomial')
p = predict(logit, data = train, type = "response")
p3 = as.factor(ifelse(probab > 0.8, "Churn", "Not Churn"))
confusionMatrix(p3,sl2$churn)
probab = predict(logif, data = sl2, type = "response")
summary(sl2$recency)
sl2$probab = probab
sl2$clife = (1/sl2$probab) * 258
summary(probab)
#proxy model2
p = predict(logit, data = train, type = "response")
q = predict(logit, data = sl2, type = "response")
pro = rbind(p,q)
str(q)
summary(q)
pro2 = as.factor(ifelse(pro > 0.8, "Churn", "Not Churn"))
confusionMatrix(pro2,sl2$churn)

install.packages("mltools")
library(mltools)
install.packages("randomForest")
install.packages("rpart.plot")
library(rpart.plot)
library(randomForest)
rf <- randomForest(churned ~ SO.Date + Sales.Segment + Oder.Type + Sales.Person + 
               Transaction.Type + recency + monetary + frequency + BRAND + PACKAGE_SIZE, data = sl2)
rf
ro = predict(rf, sl2)
ro2 = as.factor(ifelse(ro > 0.8, "Churn", "Not Churn"))
confusionMatrix(ro2,sl2$churn)
summary(rf$importance)
str(sl2)
sl2$probab = exp(ro)/(1+exp(ro))
sl2$randomForest = ro
rf$importance

decisionTree_model
decisionTree_model <- rpart(churned ~ SO.Date + Sales.Segment + Oder.Type + Sales.Person + 
                              Transaction.Type + recency + monetary + frequency + BRAND + PACKAGE_SIZE, sl2, method = 'class')
predicted_val <- predict(decisionTree_model, sl2, type = 'class')
probability <- predict(decisionTree_model, sl2, type = 'prob')
rpart.plot(decisionTree_model)

rpart.plot(rf)
plot(rf)
sl2 %>% ggplot(sl2, aes(x=recency, y=monetary, color=Segment)) + geom_point() + geom_rug()
ggplot(sl2, aes(x=sl2$recency, y=sl2$monetary, color=sl2$CATEGORY)) + geom_point() + geom_density_2d()
ggplot(sl2, aes(x=sl2$frequency, y=sl2$monetary, color=sl2$CATEGORY)) + geom_point() + geom_density_2d()
ggplot(sl2, aes(x=sl2$recency, y=sl2$frequency, color=sl2$CATEGORY)) + geom_point() + geom_density_2d()
ggplot(sl2, aes(x=SO.Date, y=Gross.Profit, group=CATEGORY)) +geom_line(aes(color=CATEGORY))
ggplot(sl2, aes(x=SO.Date, y=Gross.Profit, group=Sales.Segment)) +geom_line(aes(color=Sales.Segment))
ggplot(sl2, aes(x = `Sales.Person`, y = `Gross.Profit`, fill=`CATEGORY`)) + 
  geom_bar(stat="identity") + theme_minimal()
ggplot(sl2, aes(x = `CATEGORY`, y = `Gross.Profit`, fill=`BRAND`)) + 
  geom_bar(stat="identity") + theme_minimal()
ggplot(sl2, aes(x = CATEGORY, y = monetary, fill = CATEGORY)) + geom_boxplot()
ggplot(sl2, aes(x = CATEGORY, y = Gross.Profit, fill = BRAND)) + geom_boxplot()
ggplot(sl2, aes(x = CATEGORY, y = recency, fill = CATEGORY)) + geom_boxplot()
ggplot(sl2, aes(x = CATEGORY, y = Gross.Profit, fill = PACKAGE_SIZE)) + geom_boxplot()
ggplot(sl2, aes(x = CATEGORY, y = frequency, fill = CATEGORY)) + geom_boxplot()
ggplot(sl2) + geom_bar(aes(x = CATEGORY , fill = Sales.Segment))

write.csv(sl2, "sl2.csv")
write.csv(ro, "ro.csv")
sl2 = read.csv("sl2.csv" , header = T)
