base=read.csv("Advertising.csv",sep=",",header=TRUE)
base$X = NULL
dim(base)
base[1,]
head(base)
summary(base)
plot(base$TV, base$sales, xlab="TV", ylab="Sales")
hist(base$sales,xlab="Sales", main="Ventas",col=rainbow(20))

######################

library(caret)
set.seed(150);particion=createDataPartition(y=base$sales,p=0.7,list=FALSE)
entrenar=base[particion,]
test=base[-particion,]
str(test)
head(test)
str(entrenar)
head(entrenar)
dim(test)
dim(entrenar)

######################

reg=lm(sales~.,entrenar)
predRL=predict(reg,test)
summary(reg)
reg
plot(reg)
predRL = predict(reg,test)
ECM=mean((predRL-test$sales)^2)
ECM

######################

library(nnet)
set.seed(8);red=nnet(sales~., entrenar, size=10, maxit=10000, linout=TRUE)
library(NeuralNetTools)
plotnet(red)
library(rpart)
library(rpart.plot)
arbol=rpart(sales~.,entrenar)
rpart.plot(arbol, extra=101, type=4)
library(e1071)
svm=svm(sales ~.,entrenar, kernel=”sigmoid”, cost=0.5)

######################

prediccionRN = predict(red, test)
ECM=mean((prediccionRN-test$sales)^2)
ECM
predArb = predict(arbol,test)
ECM=mean((predArb-test$sales)^2)
ECM
predictSVM=predict(svm, test)
ECM=mean((predictSVM-test$sales)^2)
ECM
