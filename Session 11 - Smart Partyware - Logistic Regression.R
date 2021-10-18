data <- read.csv("SmartPartyWare Raw Data.csv", header = T)
colnames(data) #checking column names
str(data) #checking data structure
data$Gender <- as.factor(data$Gender) #converting gender to a factor variable instead of numeric
str(data)
data <- data[, 3:18] # Removing ID. and Seq column because it is not a variable
# Note that we can do both the above together as data <- data[, -c(1:2)]
data$Success <- as.factor(data$Success) #Converting success into a discrete/factor variable

str(data)
train <- data[1:1000, ] # Getting first 1000 rows as training
test <- data[1001:2000, ] #Getting next 1000 rows as testing
str(train)
str(test)

mymodel <- glm(formula = Success ~ ., 
               data = train, 
               family = "binomial")
mymodel <- step(mymodel)
summary(mymodel)

mypredictions_train <- mymodel$fitted.values

mypredictions_test <- predict(mymodel, 
                              test, 
                              type = "response")

allpredictions <- c(mypredictions_train, 
                    mypredictions_test)
write.csv(allpredictions, file = "SPW1.csv")
