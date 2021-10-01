## == This example uses the Brazilian inflation data from
#Garcia, Medeiros and Vasconcelos (2017) == ##
data("BRinf")

## == Data preparation == ##
## == The model is yt = a + Xt-1'b + ut == ##
## == The autorregressive is a fixed control == ##
aux = embed(BRinf,2)
y=aux[,1]
x=aux[,-c(1:ncol(BRinf))]

model=bagging(x,y,pre.testing = "group-joint")
model$orig.coef
bagging$X
coefficients(cv.elastic.net.regression)
## == check selection frequency == ##
coef=coef(model)
coef[coef!=0]=1
frequency=(colSums(coef))[-1] # remove intercept
barplot(frequency)

## == see fitted values == ##
plot(y,type="l")
lines(fitted(model),col=2)