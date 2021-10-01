ad <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv")
str(ad)
summary(ad)
ad2 = na.omit(ad)
write.csv("E://parhai", ad2.csv)
str(ad2)
ad$Channel = as.factor(ad$Channel)
ad$Region = as.factor(ad$Region)
plot(ad)
ap = lm(Frozen ~ ., data = ad)
ap
summary(ap)
aq = predict(ap)
plot(aq, ad$Frozen)
setwd("Desktop")
getwd()
ao = c(aq,ad)
ao
str(ao)
summary(ao)
write.csv(ao, "ao.csv")
install.packages('corrplot')
install.packages('ggplot')
library(ggplot2)
library(ggplot)
library(corrplot)
library(dplyr)

ad %>% select_if(is.numeric) %>%  cor() %>% corrplot()
ggplot(ad)+geom_boxplot(aes(x= Province,y = Cases))
ggplot(ad) + geom_bar(aes(x = Cases , fill = Province))
data <- read.csv('C:/Users/fe/Documents/ad1.csv', header = T)
data$date <- format(as.Date(data$date), "%Y/%m/%d")
data$Date = as.Date(data$Date, "%d-%m-%y")
data %>% select_if(is.numeric) %>%  cor() %>% corrplot()
str(data)
#====#

install.packages("recommenderlab")
m = read.csv("C:/Users/fe/Downloads/movies.csv")
r = read.csv("C:/Users/fe/Downloads/ratings.csv")
str(m)
str(r)
head(m)
m2 <- as.data.frame(m$genres, stringsAsFactors=FALSE)
str(m2)

library(data.table)
m3 <- as.data.frame(tstrsplit(m2[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 
str(m3)
summary(m3)
m$genres = format(as.character(m$genres))
m$title = format(as.character(m$title))
str(m3)
colnames(m3) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre
for (index in 1:nrow(m3)) {
  for (col in 1:ncol(m3)) {
    gen_col = which(genre_mat1[1,] == m3[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)
SearchMatrix <- cbind(m[,1:2], genre_mat2[])
head(SearchMatrix)    
summary(SearchMatrix)
str(SearchMatrix)
