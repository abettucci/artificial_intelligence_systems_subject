llibrary(jpeg)
imagen=readJPEG("zorzal.jpg")
gris=(imagen[,,1]+imagen[,,2]+imagen[,,3])/3
writeJPEG(gris,"gris.jpg")
library(jpeg)
imagen = readJPEG("zorzal.jpg")
rojo=as.vector(imagen[,,1])
verde=as.vector(imagen[,,2])
azul=as.vector(imagen[,,3])
base=data.frame(rojo,verde,azul)
km=kmeans(base,2)
segmR=rojo
segmV=verde 
segmA=azul 
segmR[km$cluster==1]=1
segmR[km$cluster==2]=0
segmV[km$cluster==1]=0
segmV[km$cluster==2]=0
segmA[km$cluster==1]=0
segmA[km$cluster==2]=1
segmentada=imagen
segmentada[,,1]=segmR
segmentada[,,2]=segmV
segmentada[,,3]=segmA
writeJPEG(segmentada,"segmentada.jpg")
km$centers
km$sizesegmentada[,,2]=segmV
segmentada[,,3]=segmA
writeJPEG(segmentada,"segmentada.jpg")
km$centers
km$size


library(jpeg)
imagen=readJPEG("zorzal.jpg")
segmA=azul
segmR=rojo
segmV=verde
rojo=as.vector(imagen[,,1])
verde=as.vector(imagen[,,2])
azul=as.vector(imagen[,,3])
base=data.frame(rojo,verde,azul)
km=kmeans(base,3)
segmR[km$cluster==1]=0.4
segmR[km$cluster==2]=0.9
segmR[km$cluster==3]=0.7
segmV[km$cluster==1]=0.8
segmV[km$cluster==2]=0.6
segmV[km$cluster==3]=0.5
segmA[km$cluster==1]=0.1
segmA[km$cluster==2]=0.2
segmA[km$cluster==3]=0.3
segmentada3=imagen
segmentada3[,,1]=segmR
segmentada3[,,2]=segmV
segmentada3[,,3]=segmA
writeJPEG(segmentada3,"segmentada3.jpg")

###############


data(Auto)
base=Auto
fix(base)
base$year=NULL
base$origin=NULL
base$name=NULL
head(base)
str(base)
summary(base)

set.seed(150);km=kmeans(base,5)
km$centers
km$size
km$cluster[3]
lot(base$mpg,base$horsepower,col=km$cluster,xlab="Mpg",ylab="Horsepower",main="Mpg-Horsepower Relation")
legend("topright",c("Cluster #1","Cluster #2","Cluster #3","Cluster #4","Cluster #5"),col=levels(as.factor(km$cluster)),pch=19,cex=1)

basesom=as.matrix(base)
library(kohonen)
set.seed(150);som=som(basesom,grid=somgrid(1,5,"hexagonal"))
plot(som,type="codes")
cantidad=plot(som,type="count")
som$codes
som$unit.classif
cantidad
som$unit.classif[3]
plot(basesom[,6],basesom[,5], col=som$unit.classif, xlab="Aceleración", ylab="Peso del auto", main="Aceleración vs. Peso", pch=19,cex=1)
legend("topright",c("Cluster #1","Cluster #2","Cluster #3","Cluster #4","Cluster #5"),col=levels(as.factor(som$unit.classif)),pch=19,cex=1)

c1=c(1,3)
c2=c(1,8)
c3=c(3,4)
p=c(2,8)
C1P=rbind(c1,p)
dist(C1P)
C2P=rbind(c2,p)
dist(C2P)
C3P=rbind(c3,p)
dist(C3P)

#################3

Oculta = function(x1,x2,w1,w2){return(0.2*x1+0.5*x2)}
alida = function(Oculta,w3){return(Oculta*0.3)}
culta(2,5)
alida(Oculta(2,5))

alida = function(umb=function(Oculta,we){return(Oculta*0.3)}){if( umb  5) {return("Alcanza el Umbral -1")}else{return("Por debajo del Umbral -0")}}
alida(Oculta(2,5))
alida(Oculta(4,8))
alida(Oculta(5,8))
alida(Oculta(5,2000000))
alida(Oculta(-1,3))
alida(Oculta(-5,-8))
alida(Oculta(-5,67))

###############

ead.table("vowel.train.txt",sep=",",header=TRUE)
library(caret)
ase=read.table(“vowel.train.txt”.sep=”,”,header=TRUE)
ase$row.names=NULL
ase$y=as.factor(base$y)
tr(base)
ead(base)
ummary(base)
ttach(base)
lot(x.1,x.2,xlab=”X1”,ylab=”X2”,pch=19,cex=1,col=y)
egend(“topright”,legend=levels(y),col=1:11,pch=19,cex=1)
ocal=factor(y,levels=c(1,2,3,4,5,6,7,8,9,10,11),labels=c(“i”,”I”,”E”,”A”,”a:”,”Y”,”O”,”C:”,”U”,”u”,”3:”))
lot(vocal,main=”Reconocimiento de vocales”,col=”Green”,xlab=”Vocales”)

#####################3

ibrary(caret)
et.seed(8);particion=createDataPartition(y=base$y,p=0.7,list=FALSE)
ntrenar=base[particion,]
est=base[-particion,]
tr(entrenar)
ead(entrenar)
ummary(entrenar)
tr(test)
ead(test)
ummary(test)

#################

library(nnet)
set.seed(8);red=nnet(y~.,entrenar,size=6,maxit=10000)
library(NeuralNetTools)
pred=predict(red,test,type=”class”)
prueba=cbind(test,pred)
fix(prueba)
matriz=confusionMatrix(factor(pred),test$y)

#################3


library(caret)
modelo=train(Species~.,iris,tuneGrid=expand.grid(size=c(10,20),decay=0),maxit=10000,trControl=trainControl(method=”cv”,10),method=”nnet”)
modelo
library(NeuralNetTools)
plotnet(modelo)
plotnet(modelo,bias=FALSE)
