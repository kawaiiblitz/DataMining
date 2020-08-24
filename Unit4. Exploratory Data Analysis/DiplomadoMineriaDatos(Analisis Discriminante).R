### DIPLOMADO MINERÍA DE DATOS (Análisis Discriminante)

library(MASS)
library(rgl)
library(sm)
data(crabs)
crabs

#crabs in spanish

cangrejos<-crabs

colnames(cangrejos)<-c("especie","sexo","id","ancho lobulo frontal","ancho tracero","longitud caparazon","ancho caparazon","profundidad del cuerpo")

cangre<-cangrejos



colnames(cangre)<-c("esp","sex","id","alf","atra","loncap","acap","procue")

#Objetivo es determinar si estas medidas diferencian a las especies de cangrejos

attach(cangre)

apply(cangre[,-c(1,2,3)],2,mean)
  ##Manova compara variables para muhchas poblaciones. Ho: Todas las medias de las variables son iguales. Si se rechaza por el pvalue es chico, nos dice que al menos dos tienen dif. media; esto requiere la prueba de normalidad multivariada.
cangreO<-cangre[cangre$esp=="O",]

cangreB<-cangre[cangre$esp=="B",]

apply(cangreO[,-c(1,2,3)],2,mean)

apply(cangreB[,-c(1,2,3)],2,mean)

#Parece ser que los cangrejos naranja ("O") son más grandes que los azules ("B")
#De verda' lo son?

X<-as.matrix(cangre[,-c(1,2,3)])
cangre.manova<-manova(X~cangre$esp)
summary(cangre.manova)
summary(cangre.manova,test="Wilks")

#EFECTIVAMENTE, AL MENOS SON DISTINTAS POR ALGUNA DE LAS VARIABLES

#Representación gráfica de estos cangrejos

ccp<-princomp(X)
win.graph()

plot(ccp$scores[,1], ccp$scores[,2],col = ifelse(crabs$sp == "O", "orange","blue"), pch = 16, xlab = "PC1", ylab = "PC2")

#No que los naranja eran más grandes que los azules?? Ton's qué pasó?

#Y cómo se comportan por sexo?
win.graph()

plot(ccp$scores[,1], ccp$scores[,2],col = ifelse(crabs$sp == "O", "orange","blue"), pch = ifelse(crabs$sex == "M", 1,8),
     xlab = "PC1", ylab = "PC2")

#Como ocurre generalmente en la naturaleza, los machos son más grandes que las hembras


win.graph()

plot3d(ccp$scores[,1],ccp$scores[,2],ccp$scores[,3], col= ifelse(crabs$sp == "O", "orange", "blue"),type = "s",cex=0.001)


#'Ora si discriminante
#Primero a PATIN

M.O<-apply(cangreO[,-c(1,2,3)],2,mean)
M.B<-apply(cangreB[,-c(1,2,3)],2,mean)
S.O<-var(cangreO[,-c(1,2,3)])
S.B<-var(cangreB[,-c(1,2,3)])
n1<-length(cangreO[,1])
n2<-length(cangreB[,1])
S.pool<-round(((n1-1)*S.O+(n2-1)*S.B)/(n1+n2-2),3)

#Ahora la construccion de las funciones lineales discriminantes

a.gorro<-(M.O-M.B)%*%solve(S.pool)  ##Invversa de la matriz.

pred.y<-as.matrix(cangre[,-c(1,2,3)])%*%t(a.gorro)

criterio<-0.5*(M.O-M.B)%*%solve(S.pool)%*%(M.O+M.B) 

win.graph()

plot(pred.y,type="n",main="Discriminante cangrejos naranja y azules")
text(pred.y,labels=as.character(crabs$sp),col=c(rep("blue",100),rep("orange",100)))
abline(h=(mean(pred.y[crabs$sp == "O"])+mean(pred.y[crabs$sp == "B"]))/2,col="green",lty=3)
points(50,mean(pred.y[crabs$sp == "B"]),col='black',pch=4,cex=1.5)
points(150,mean(pred.y[crabs$sp == "O"]),col='black',pch=4,cex=1.5)
lines(c(50,150),c(mean(pred.y[crabs$sp == "B"]),mean(pred.y[crabs$sp == "O"])),lty=3,col="violet")


legend(locator(1),
legend=c("Punto medio entre medias de grupos proyectados"),col="darkred",bty="n",lwd=0.5)

legend(locator(1),legend=c("Umbral de corte de clasificación"))

### Clasificación

clasificacion<-ifelse(pred.y> criterio[[1]],"O","B")

table(cangre$esp,clasificacion)

grupo1<-c(rep(0,100),rep(1,100))

win.graph()
library(lattice)

densityplot(pred.y[,1],groups=grupo1,xlab="",ylab="densidades ptos. proyectados",plot.points=TRUE,col=c("blue","orange"),pch=19)

sm.density.compare(pred.y[,1],grupo1,xlab="",ylab="densidades ptos. proyectados",col=c("blue","orange"))
abline(v=(criterio[1]),col="red")
title("Gráfica de las densidades de los grupos proyectados")
legend(locator(1),
legend=c("Grupo Blue","Grupo Orange"), 
col=c("blue","orange"),bty="n",lwd=5)

legend(locator(1),
legend="Pto. de corte", 
col="red",bty="n",lwd=5)


#Ahora de manera automática

lda.crabs<-lda(cangre[,-c(1,2,3)],cangre$esp)
pred<-predict(lda.crabs)$x   ##El valor predicho. Queremos ver que está cargada a la derecha y cargada a la izquierda. 

plot(lda.crabs, dimen=1, type="both")

crab.esp<-rep(c("O","B"),c(100,100))

win.graph()
plot(pred,type="n",main="Discriminante cangrejos naranja y azules")
text(pred,labels=as.character(crab.esp),col=c(rep("orange",100),rep("blue"rrr,100)))

### Clasificación

table(cangre$esp,predict(lda.crabs)$class)

### Clasificación a través de clasificación cruzada

pred1<-predict(lda.crabs,CV=TRUE)  ##QQuita el sujeto, lo predice.

table(cangre$esp,pred1$class)    ##Clasificación cruzada 

### Función construida con un 80% de los datos y usada para predecir el otro 20%

train<-sample(1:200,160)  ##Que me muestree al 980%

lda.crabs1<-lda(cangre[,-c(1,2,3)],cangre$esp,subset=train)

prelda.crabs1<-predict(object=lda.crabs1, newdata=cangre[,-c(1,2,3)][-train, ])  ##Que haga el entrenamiento y luego me haga la clasificación quitándole los de entrenamiento y me quedo con los 40 para clasificarlos. 


table(cangre$esp[-train],prelda.crabs1$class)


#USOS DE LAS FUNCIONES LINEALES
#Prediccion de un nuevo individuo

t.new1<-c(16.110,14.01,34.78,36.3,14.99)
t.new2<-c(13.56,12.28, 28.958, 33.17, 13.083)


##El valor criterio para este caso es los positivos y los negativos. 

predict(lda.crabs,t.new1)

predict(lda.crabs,t.new2)

### Si fueran muchos individuos para predecir, hay que ponerlos en un data.frame

t.new<-rbind(t.new1,t.new2)

colnames(t.new)<-c("alf","atra","loncap","acap","procue")
t.new<-data.frame(t.new)

predict(lda.crabs,t.new)


#Cuáles son las variables de mayor importancia para discriminar entre estas dos especies de cangrejos?
##Tenenmos que escalar pues nos está jugando la escala.

elda.crabs<-lda(scale(cangre[,-c(1,2,3)],center=T,scale=T),cangre$esp)$scaling

#Trabaja lda con las variables estandarizadas?

lda(iris[,-5],iris[,5])$scaling

lda(scale(iris[,-5],center=T,scale=T),iris[,5])$scaling

est1<-(iris[,1]-mean(iris[,1]))/(sd(iris[,1]))
est2<-(iris[,2]-mean(iris[,2]))/(sd(iris[,2]))
est3<-(iris[,3]-mean(iris[,3]))/(sd(iris[,3]))
est4<-(iris[,4]-mean(iris[,4]))/(sd(iris[,4]))

est<-cbind(est1,est2,est3,est4)

lda(est,iris[,5])$scaling

#SIMON

#Comparación de clasificación "a pie" y automática

table(predict(lda.crabs)$class,clasificacion)

#### "INDENTICAS"

### Y cómo discriminan un par de variables cualesquiera sin hacer nada más

pairs(cangre[,-c(1,2,3)], main = "Cangrejos azules y naranjas",pch = 21, bg = c("blue","orange")[unclass(cangre$esp)])




## Quitemos las varibales que no pesan 
##La idea es tener la misma clasificación 
lda.crabs2<-lda(cangre[,-c(1,2,3,5,6)[,cangre$esp)
pred2<-predict(lda.crabs2,CV=TRUE)
table(cangre$esp,pred2$clas)
##Aquí si funciona. 


##Y si quitams una más?

lda.crabs3<-lda(cangre[,-c(1,2,3,5,6,8)],cangre$esp)
pred3<-predict(lda.crabs3,CV=TRUE)
table(cangre$esp,pred3$class)









############################# SUPUESTOS

source("C:/Users/DELL56414/Desktop/MULTIVARIATE WITH R/BoxMTest.R")

BoxMTest(cangre[,-c(1:3)],cangre$esp,0.05)
##Entonces nos dice que las medias no son iguales. 
### !GACHO!


#LAS AVISPAS
# A researcher was interested in discriminating among four behavioral groups of primitively
# eusocial wasps from the genus Polistes based on gene expression data measured
# in the brain (based on qrt-PCR). The data was log transformed before the analysis.
# The four groups are: foundress (F), worker (W), queens (Q), and gynes (G). The gene expression data was based on 32 genes

avispas<-read.table("C:/Users/Rachel/Documents/Raquel/Diplomado Minería de Datos/Módulo 4 Análisis Exploratorio de Datos/wasp.txt",header=T,na.strings=".")
dim(avispas)

avis<-na.omit(avispas) #eliminamos registros con datos faltantes

### Componentes

cpa<-princomp(avis[,-1])

sum(cpa$sdev[1:3])/sum(cpa$sdev)   ##La varianza explicada de las primeras 3 variables.


win.graph()

plot3d(cpa$scores[,1],cpa$scores[,2],cpa$scores[,3],cex=0.1,type="n")
text3d(cpa$scores[,1],cpa$scores[,2],cpa$scores[,3],text=as.character(avis$grp),font=2,col=1+unclass(avis$grp),cex=0.8)

### Sin textos (el orden es: F G Q W)

plot3d(cpa$scores[,1],cpa$scores[,2],cpa$scores[,3],cex=0.1,type = "s",font=2,col=1+unclass(avis$grp),cex=0.8)


### No hay nada gráficamente !CON COMPONENTES!.


#'Ora si, a lo que te "truje CHENCHA"

lda.avis<-lda(scale(avis[,-1],center=T,scale=T),avis[,1]) ##Quitando la variable de clasificación

lda.avis

names(lda.avis)

lda.avis$svd/sum(lda.avis$svd)

sum(lda.avis$svd[1:2])/sum(lda.avis$svd)

table(verdaderos=avis$grp,predichos=predict(lda.avis)$class)  ##Clasificación sin quitar ni poner. 


#tasa de error aparente

3/74

lda.avis.cv <- lda(avis[,-1],avis[,1],CV=T)

table(verdaderos=avis$grp,predichos=lda.avis.cv$class)
##Esta clasificación es más "real" y nos dice que sí dbeemos hacer la clasificación cruzada.




#tasa de error 

26/73
#################
win.graph()
avis.pred<- predict(lda.avis,dimen=2)$x
eqscplot(avis.pred,type="n",xlab="LD1",
ylab="LD2")
text(avis.pred, labels=as.character(avis$grp),col=1+unclass(avis$grp),cex=0.8)

perp <- function(x, y) {
m <- (x+y)/2
s <- - (x[1] - y[1])/(x[2] - y[2])
abline(c(m[2] - s*m[1], s))
invisible()
}

win.graph()

avis.3d<-predict(lda.avis,dimen=3)$x
eqscplot(avis.3d[,1],avis.3d[,2], type = "n", xlab = "LD1", ylab = "LD2",main="Funciones lineales discriminantes")
text(avis.3d[,1],avis.3d[,2], labels = as.character(avis$grp),col=1+unclass(avis$grp),cex=0.8)

avis.m <- lda(avis.3d,avis[,1])$means
points(avis.m, pch = 3, mkh = 0.3)
perp(avis.m[1, ], avis.m[2, ])
perp(avis.m[2,],avis.m[3,])
perp(avis.m[1,],avis.m[3,])

win.graph()

plot3d(avis.3d,xlab="LD1",ylab="LD2",zlab="LD3",type = "s",col=1+unclass(avis$grp),cex = 0.01)


plot3d(avis.3d,xlab="LD1",ylab="LD2",zlab="LD3",col=1+unclass(avis$grp),cex = 0.01,type="n")
text3d(avis.3d,xlab="LD1",ylab="LD2",zlab="LD3",text=as.character(avis$grp),font=2,col=1+unclass(avis$grp),cex=0.8)

#######################################


qda.avis<-qda(avis[,-1],avis[,1])


### IRIS DATA

require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

pca <- prcomp(iris[,-5],
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca$sdev^2/sum(pca$sdev^2)

lda <- lda(Species ~ ., 
           iris, 
           prior = c(1,1,1)/3)

prop.lda = lda$svd^2/sum(lda$svd^2)

plda <- predict(object = lda,
                newdata = iris)

dataset = data.frame(species = iris[,"Species"],
                     pca = pca$x, lda = plda$x)

p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5)
       labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))
       

p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = species, shape = species), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)

### Discriminación de las variables

pairs(iris[,-5], main = "Iris de Fisher",pch = 21, bg = c("darkblue","darkgreen","darkred")[unclass(iris$Species)])








