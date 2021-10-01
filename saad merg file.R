#movie by msk 
m0 = read.csv("C:/Users/fe/Downloads/movies.csv")
r0 = read.csv("C:/Users/fe/Downloads/ratings.csv")
str(m0)
str(r0)
str(q)
q = merge(m0,r0,by="movieId")
q$genres = format(as.character(q$genres))
q$title = format(as.character(q$title))

#joiner to check and test the previous filtering 
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
#ok boss new resime
p = merge(SearchMatrix, q, by="movieId")

#new part fetch
p1 = read.csv("C:/Users/fe/Desktop/Saad/movie data.csv")
p1$timestamp <- as.Date.POSIXct(p1$timestamp, format = "%m/%d/%Y %H:%M:%S")
p1$timestamp <- as.POSIXct(p1$timestamp, format = "%m/%d/%Y %H:%M:%S")
p1
str(p1)
p1 %>% select_if(is.numeric) %>%  cor() %>% corrplot()
ggplot(p1)+geom_boxplot(aes(x= rating,y = movieId))

write.csv(p1,"C:/Users/fe/Desktop/Saad/p1.csv")
ratingMatrix <- dcast(p, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
library(recommenderlab)
lapply(recommendation_model, "[[", "description")
