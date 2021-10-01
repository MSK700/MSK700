iris <- read.csv("C:/Users/fe/Downloads/iris.csv", header = T)
str(iris)
iris = sample_n(iris, 149)
trn = iris[1:99,]
tst = iris[100:149,]
library(party)
irus = ctree(Iris.setosa~.,data = trn)
table(predict(irus),trn$Iris.setosa)
confusionMatrix(predict(irus),trn$Iris.setosa)
ir = predict(irus, newdata = tst)
confusionMatrix(ir,tst$Iris.setosa)
ir
tst$Iris.setosa
iris

