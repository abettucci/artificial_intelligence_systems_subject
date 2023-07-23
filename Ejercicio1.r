base = read.table("optdigits.tra.txt", sep=",")
dim(base)
library(jpg)
vector = base[15,] #tomamos la fila 15 por separado
vector #visualizamos que la fila sigue siendo un data.frame con títulos de columnas
dim(vector)
vector = vector[-65] #sacamos la columna V65 (variable a predecir – valor 3)
vector = as.numeric(vector) #transformamos el data.frame a vector
vector #visualizamos que la fila ahora es un vector
dim(vector) #da error porque ya no es data.frame
length(vector)
vector = vector/16 #transformamos los valores de (1:16) a (0:1)
imagen = array(vector, dim = c(8,8)) #creamos la imagen de 8x8
writeJPEG(imagen, "num.jpg") #creamos el jpg
names(base)[names(base)=="V65"]="Numero"
base$Numero=as.factor(base$Numero)
head(base$Numero)
summary(base$Numero)
plot(base$Numero, main=”Numeros reconocidos”, col=”red”)

#####################

library(caret)
set.seed(150); particion = createDataPartition(y= base$Numero, p= 0.7, list= FALSE)
entrenar = base[particion,]
test = base[-particion,]
str(test)
str(entrenar)
dim(entrenar)
dim(test)
table(test$Numero)
table(entrenar$Numero)

#####################

library(rpart)
library(rpart.plot)
arbol = rpart(Numero~., entrenar, method="class")
rpart.plot(arbol, extra=101, type=4, cex=0.55, box.palette=”Blues”, main=”Modelo: Árbol de Decision”)
predArbol = predict(arbol,test,type=”class”)
confusionMatrix(predArbol, test$Numero)

#####################

library(e1071)
svm = svm(Numero~., entrenar, kernel="sigmoid", cost=0.5)
predSVM = predict(svm, test)
confusionMatrix(predSVM, test$Numero)

#####################

nb = naiveBayes(Numero~., entrenar)
predNB = predict(nb, test)
confusionMatrix(predNB, test$Numero)
