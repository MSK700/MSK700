library(ggplot2)
library(ggplot)
library(corrplot)
library(data.table)
library(readr)
library(dplyr)
library(mltools)



#install.packages("mltools")

canada = read.csv('C:/Users/fe/Downloads/CANADA.csv', header = T)
str(canada)

summary(canada$Highest.certificate..diploma.or.degree)
summary(canada$VALUE)

barplot(table(canada$Highest.certificate..diploma.or.degree),las=2, main="Highest Degree")


hist(canada$VALUE)
summary(canada)
summary(canada$Age.group)
View(canada)
plot(canada$VALUE, canada$GEO)

boxplot(canada$VALUE ~ canada$Highest.certificate..diploma.or.degree,
        main="Income by Highest Degree", xlab="Highest Degree", ylab="USD")

ggplot(canada, aes(x = Highest.certificate..diploma.or.degree, y = VALUE, fill = Highest.certificate..diploma.or.degree)) + geom_boxplot()

canada %>% group_by(Highest.certificate..diploma.or.degree) %>% summarise(Min_Value = min(VALUE), Max_Value = max(VALUE), Sum_Value = sum(VALUE), Avg_Value =mean(VALUE))

anova(lm(VALUE ~ Highest.certificate..diploma.or.degree, data = canada))


newdata <- one_hot(as.data.table(canada$Highest.certificate..diploma.or.degree))
str(newdata)
newdata$VALUE = canada$VALUE
newdata %>% select_if(is.numeric) %>% cor() %>% corrplot()
newdata %>% select_if(is.numeric) %>% cor() %>% data.table()
newdata %>% select_if(is.numeric) %>% cov() %>% data.table()
p= lm(VALUE ~ Highest.certificate..diploma.or.degree, data = canada)
po = step(lm(VALUE ~ ., data = newdata))
plot(newdata$VALUE, predict(po))
summary(p)
summary(po)
anova(lm(VALUE ~ Highest.certificate..diploma.or.degree, data = canada))
anova(lm(VALUE ~ ., data = newdata))
 write.csv(newdata , file = "no.csv")
no = read.csv('no.csv', header = T)
no %>% select_if(is.numeric) %>% cor() %>% corrplot()


install.packages("mctest")
library(mctest)
canada %>% omcdiag(x= Highest.certificate..diploma.or.degree, y = VALUE)
mean(p$residuals)
cor.test(newdata$`V1_Apprenticeship or trades certificate or diploma`, po$residuals)
install.packages("gvlma")
library(gvlma)

gvlma::gvlma(po)

ponka = lm(VALUE ~ `V1_No certificate, diploma or degree`, data = newdata)
punnka = lm(VALUE ~ `V1_University certificate or degree at bachelor level or above`, data = newdata)

plot(punnka, 1)
plot(punnka, 3)
plot(punnka, 2)
#http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
install.packages("broom")
library(broom)
model.diag.metrics <- augment(po)
head(model.diag.metrics)
  ggplot(model.diag.metrics, aes(`V1_University certificate or degree at bachelor level or above`, VALUE)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = `V1_University certificate or degree at bachelor level or above`, yend = .fitted), color = "red", size = 0.3)
  