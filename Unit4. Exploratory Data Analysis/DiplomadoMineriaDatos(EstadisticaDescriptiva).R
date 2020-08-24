### DIPLOMADO MINERÍA DE DATOS (Estadística Descriptiva)
install.packages("sm")

library(plotrix)
library(psych)
library(plyr)
library(MASS)
library(car)
library(sm)
library(scatterplot3d)
library(rgl)
library(lattice)
require(vcd)
library(epitools)
library(boot)
library(energy)

##################################################################################################################################

### Estadística Descriptiva

datos<-read.csv("C:/Users/Rachel/Documents/Raquel/Diplomado Minería de Datos/Módulo 4 Análisis Exploratorio de Datos/acero2.csv")


colnames(datos)

attach(datos)

#### Resúmenes numéricos

mean(consumo)

median(consumo)

var(consumo)

sd(consumo) 

summary(consumo)

range(consumo)

sd(consumo)/abs(mean(consumo))

fivenum(consumo)

quantile(consumo,probs=c(0.25,0.50,0.75))

skew(consumo)

kurtosi(consumo)

###
### Número de intervalos por default:[log2(n) + 1]

hist(consumo,col=rainbow(6),main="Histograma: consumo de energía",sub="PEMEX",xlab="consumo de energía",ylab="frecuencia absoluta",
col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ")

q<-quantile(consumo,prob=c(seq(0,1,0.1)))  ### Definimos nosotros el número de intervalos

hist(consumo,col=rainbow(10),main="Histograma: consumo de energía",sub="PEMEX",xlab="consumo de energía",ylab="frecuencia absoluta",
col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ",breaks=q)

lines(density(consumo),col="darkred",lwd=2)

### Histograma con frecuencias relativas

truehist(consumo,col="green",border="white",main="Histograma: consumo de energía",sub="PEMEX",xlab="consumo de energía",ylab="frecuencia relativa",
col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ",prob=TRUE,ylim=c(0,0.007))

curve(dnorm(x,mean(consumo),sd(consumo)),add=T,col="red")


### Histograma con las frecuencias de cada barra

hist(consumo, col = "green", border="white", main="Histograma: consumo de energía",sub="PEMEX",col.main="darkblue",col.axis="red",col.lab="darkgreen",
     col.sub=" skyblue ",label = T)

### Histograma con frecuencias relativas en cada barra

histPercent<-function(x, ...) {
   H<-hist(x,plot=FALSE)
   H$density<-with(H,100*density*diff(breaks)[1])
   labs<-paste(round(H$density), "%", sep="")
   plot(H,freq=FALSE,labels=labs,ylim=c(0,1.08*max(H$density)),col.lab="darkred",...)
}

histPercent(consumo,col="green",border="white",main="Histograma: consumo de energía",col.main="darkblue")


### Diagrama de tallo y hoja


stem(consumo) ### Muy poco claro

stem(consumo,scale=2,width=80,atom=1e-8) ### Mucho mejor


### Gráficas de barras

T1<-table(temperatura)

b<-barplot(T1,col=c("red","blue","yellow"),main="Gráfica de barras de la temperatura",sub="Niveles de temperatura",
col.main="red",col.sub="blue",ylab="Frecuencia absoluta",col.lab="green")
text(x=b,y=T1,labels=T1,pos=3,col="green",cex=1.5,xpd=TRUE)


b<-barplot(T1,col=c("red","blue","yellow"),horiz=TRUE,main="Gráfica de barras de la temperatura",ylab="Niveles de temperatura",
col.main="red",xlab="Frecuencia absoluta",col.lab="green")
text(x=b,y=T1,labels=T1,pos=3,col="green",cex=1.5,xpd=TRUE,main="Gráfica de barras de la temperatura")
legend("topright",c("Alta","Baja","Media"),col=c("red","blue","yellow"), pch=15, bty="n")

###

T2<-table(linea,averias)

par(mfrow=c(1,2))

barplot(T2,beside=T,legend.text=rownames(T2),main="Distribución de las averías por línea de producción",col=c("red","blue","yellow"),
        ylab="Frecuencia absoluta",col.main="darkblue",col.lab=c("darkgreen"))



barplot(T2,beside=F,legend.text=rownames(T2),main="Distribución de las averías por línea de producción",col=c("red","blue","yellow"),
        ylab="Frecuencia absoluta",col.main="darkblue",col.lab=c("darkgreen"))

### Diagramas de pie

T5<-table(linea)

pie(T5,legend.text=rownames(T5), main="Gráfica de pie: Líneas",border="white",col=c("darkblue","darkred","purple"),
                  col.main="darkgreen")

lbls=rownames(T5)
pct<-round(T5/sum(T5)*100)
lbls<-paste(lbls, pct) 
lbls<-paste(lbls,"%",sep="")

pie(T5,labels=lbls, main="Gráfica de pie: Líneas",border="white",col=c("darkblue","darkred","purple"),
                  col.main="darkgreen",col.lab="darkgreen")

### 3D

pie3D(T5,labels=lbls,explode=0.1,main="Gráfica de pie: Líneas",col=c("darkgreen","purple","darkred"),col.main="darkgreen")

### box-plot

par(mfrow=c(1,2))

boxplot(consumo,main="Box-Plot: consumo",col="violet",ylab="consumo",col.main="darkblue",col.lab="darkgreen")

boxplot(consumo,main="Box-Plot: consumo",col="green",ylab="consumo",col.main="darkblue",col.lab="darkred",horizontal=TRUE)

### Comparación de poblaciones a través del box-plot

table(averias)

par(mfrow=c(1,2))

boxplot(consumo~averias,main="Box-Plot:Comparación consumo por averías",col=" skyblue ",ylab="consumo",col.main="magenta",col.lab="violetred1")

boxplot(consumo~averias,main="Box-Plot:Comparación consumo por averías",col="purple",ylab="consumo",col.main="magenta",col.lab="seagreen1",horizontal=TRUE)

### Gráficas de dispersión

plot(pr.tbc,consumo,main="Gráfica de dispersión consumo: pr.tbc",col="green",xlab="pr.tbc",ylab="consumo")

lines(lowess(pr.tbc,consumo), col="blue")

id<-1:length(consumo)

identify(pr.tbc,consumo,labels=id,col="darkred")

### Gráficas de dispersión condicionadas

scatterplot(consumo~pr.tbc|linea, data=datos,xlab="pr.tbc", ylab="Consumo", main="Diagrama de dispersión condicional",labels=id)

### Diagramas de dispersión 3D


scatterplot3d(pr.tbc,pr.cc, consumo,main="Diagrama de dispersión 3D",color=par("col"))


scatterplot3d(pr.tbc,pr.cc, consumo, pch=16, highlight.3d=TRUE,type="h", main="Diagrama de dispersión 3D")

s3d<-scatterplot3d(pr.tbc,pr.cc, consumo, pch=16, highlight.3d=TRUE,type="h", main="Plano de regresión 3D")
fit<-lm(consumo~pr.tbc+pr.cc) 
s3d$plane3d(fit,col="darkblue")



identify(pr.tbc,pr.cc, consumo,labels=id)

plot3d(pr.tbc,pr.cc, consumo, xlab="pr.tbc",ylab="pr.cc",zlab="consumo",type = "s",cex = 0.05,
col=rainbow(50:52))


### Tan chidas las pelotitas, pero ¿a qué sujeto corresponde cada una?

plot3d(pr.tbc,pr.cc, consumo, xlab="pr.tbc",ylab="pr.cc",zlab="consumo",cex = 0.1, font=2,col="red",type="n")
text3d(pr.tbc,pr.cc, consumo, text=id,font=2,col=rainbow(50:52))


### Gráficas de matrices de datos

matdat<-cbind(NOx,CO,COV,SO2,CO2,N2O)

pairs(matdat, col=rainbow(20:25))

cor(matdat)

cor(matdat,method="spearman")

pairs(matdat)

pairs(matdat,lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)

pairs(matdat,lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.box)

scatterplotMatrix(~NOx+CO+COV+SO2+CO2+N2O|linea, data=datos, main="Matriz de dispersión condicional")

pairs.panels(matdat)

##############################################################################################################################################

### Medidas de asociación y correlación
### Existe asociación entre la tasa de desempleo y la tasa tributaria en estos 19 países?
## Tiene que ser de la misma longitud para que tenga sentido hacer la correlación ##
t.d<-c(2.6,5.6,7.3,7.9,6.4,6.6,10.6,6.4,5.5,5.2,10.4,8.4,2.4,2.7,3.3,5.8,2.4,0.8,6.0)

t.impuesto<-c(0.8,0.35,0.87,0.53,1.00,0.4,0.47,0.85,0.63,0.89,0.61,0.62,0.31,1.03,0.46,0.45,1.1,0.39,0.52)

length(t.d);length(t.impuesto)

names<-c("Alemania","Austria","Bélgica","Canadá","Dinamarca","EEUU","España","Francia","Grecia","Holanda","Irlanda","Italia","Japón",
         "Noruega","Nueva Zelanda","Portugal","Suecia","Suiza","UK")

length(names)

datos<-data.frame(t.d,t.impuesto)
row.names(datos)<-names

attach(datos)

plot(t.d,t.impuesto,col=rainbow(19),main="Gráfica de dispersión tasa de desempleo vs. tasa tributaria",col.main="darkred",
     xlab="Tasa de desempleo",ylab="Tasa tributaria",col.lab="darkgreen",pch=19)

text(t.d,t.impuesto,labels=abbreviate(names),col=rainbow(19))

###Correlación de Pearson

### "A mano"

cc<-cov(t.d,t.impuesto)

v1<-var(t.d)

v2<-var(t.impuesto)

corr<-cc/sqrt(v1*v2)

### De manera automática

cor(t.d,t.impuesto)  ### Por default es la correlación de Pearson

### Spearman

### "A mano"

d<-rank(t.d)-rank(t.impuesto)


n<-length(t.d)

scor<-1-(6*sum(d^2)/(n*(n^2-1)))

### Automática

cor(t.d,t.impuesto,method="spearman")

### Tau

Kendallcor(t.d,t.impuesto)

### Automático

cor(t.d,t.impuesto,method="kendall")

### Pruebas de hipótesis sobre si la correlación es o no significativamente distinta de cero.

### Pearson (Recordar que el supuesto de normal bivariada es muy fuerte)

### Prueba de normalidad multivariada

mvnorm.etest(datos,R=1000)

### !!! SI ES NORMAL BIVARIADA !!!

### "A patita"

hatrho<-cor(t.d,t.impuesto)

T<-hatrho*sqrt(n-2)/sqrt(1-hatrho^2)

1-pt(abs(T),n-2)

cor.test(t.d,t.impuesto, method=c("pearson"),alternative="two.sided")  ### less and grater

### Porqué no coincide con el anterior? El anterior solo especifica una cola, la cola derecha. El verdadero p-value de esta prueba
### DE DOS COLAS ES

2*(1-pt(abs(T),n-2))

### Intervalo de confianza

cor.test(t.d,t.impuesto, method=c("pearson"),alternative="two.sided")$conf.int

### Qué pasa si tratamos de probar la hipótesis via bootstrap?
## Es para hacer un remuestreo. Se hace con reemplazo. hacer deducciones cuando "ya no hay nada que hacer
## i son las veces que voy a hacer el remuestreo. La función de muestreo es sample.
## El estimador es el promedio de los estimadores, por lo tanto nos va a salir algo cercano a lo que hemos obteido anteriormete.
## Este es el último recurso que tengo el BOOTSTRAP


pval.rho<-function(x,y,i){
pval.rho<-cor.test(x[i],y[i],method=c("pearson"))
return(pval.rho$p.value)
}

set.seed(1) ##Para darle un valor inicial, por tanto es pseudoaleatorio.
    
boot.pval.rho<-boot(t.d,y=t.impuesto,pval.rho,R=1000)

summary(boot.pval.rho)

mean(boot.pval.rho$t)

kk<-cor.test(t.d,t.impuesto,method=c("pearson"), alternative="two.sided")
names(kk)
kk$estimate


pval.rho<-function(x,y,i){
pval.rho<-cor.test(x[i],y[i],method=c("pearson"))
return(pval.rho$estimate)
}

set.seed(1) ##Para darle un valor inicial, por tanto es pseudoaleatorio.
    
boot.pval.rho<-boot(t.d,y=t.impuesto,pval.rho,R=1000)

summary(boot.pval.rho)

mean(boot.pval.rho$t)

boot.ci(boot.pval.rho)  ##Para construir 4 intervalos de confianza
s<-sample(t.d,length(t.d),replace=T)
s

### Spearman

corsp<-cor.test(t.d,t.impuesto, method=c("spearman"),alternative="two.sided")

corsp

### A pata

T<-corsp$estimate*sqrt(n-2)/sqrt(1-corsp$estimate^2)

2*(1-pt(abs(T),n-2))

### Int. de conf.

corsp$conf.int

### No lo proporciona. Cómo hacemos pa' encontralo?
### Bootstrap

rho<-function(x,y,i){
rho<-cor.test(x[i],y[i],method=c("spearman"))
return(rho$estimate)
}


    
boot.rho<-boot(t.d,y=t.impuesto,rho,R=1000)

boot.ci(boot.rho)

### Tau de Kendall

cortau<-cor.test(t.d,t.impuesto, method=c("kendall"),alternative="two.sided")

cortau

### "A patin"

### IMPOSIBLE

source("C:/Users/Salvador/Desktop/Yazmín/CorrKendall.R")

Kendallcor(t.d,t.impuesto)

### No proporciona intervalos de confianza. Bootstrap

tau<-function(x,y,i){
tau<-cor.test(x[i],y[i],method=c("kendall"))
return(tau$estimate)
}

library(boot)
    
boot.tau<-boot(t.d,y=t.impuesto,tau,R=1000)

boot.ci(boot.tau)

### Correlacion monotona (no lineal)

x<-rnorm(200,0,0.3)

y<-2+3*log(x[x>0])+rnorm(length(x[x>0]),0,0.3)

plot(x[x>0],y,col="darkblue",main="Asociación monótona",col.main="darkred",xlab="x",ylab="y",col.lab="darkgreen",pch=19)

curve(2+3*log(x),from=0,to=max(x),col="darkred",add=T)

### Las correlaciones
## Compara las correlaciones y nos debe mostrar que Spearman es una correlación máss fuerte
cor(x[x>0],y)

cor(x[x>0],y,method="spearman")

cor(x[x>0],y,method="kendall")

###################################################################################################################################

### Medidas de asociación en tablas de contingencia
### Asociación entre variables categóricas
### La tabla de bebedor frecuente

Bebedor<- matrix(c(1630,5550,1684,8232),nrow=2,byrow=TRUE)

colnames(Bebedor)<-c("SI","NO")
rownames(Bebedor)<-c("Hombres","Mujeres")
names(dimnames(Bebedor))<-c("Sexo","Bebedor frecuente")
print(Bebedor)

### Tabla de distribución conjunta

prop.table(Bebedor)
round(prop.table(Bebedor),3)

### Distribuciones marginales

prop.table(margin.table(Bebedor,1))

prop.table(margin.table(Bebedor,2))


### Distribuciones condicionales

Cond.Sex<-prop.table(Bebedor, 1)
Cond.Sex
margin.table(Cond.Sex, 1)


Cond.Beb<-prop.table(Bebedor, 2)
Cond.Beb
margin.table(Cond.Beb, 2)

cb<-chisq.test(Bebedor)
names(cb)
cb$observed
cb$expected
## Como no se parecen entonces se concluye que no son independientes.


### Mostrar asociaciones gráficamente

mosaicplot(Bebedor,color=TRUE,shade=T,main="mosaico: BebedorFrecuente-Sexo")

assoc(Bebedor,color=TRUE,shade=T,main="mosaico: BebedorFrecuente-Sexo") ##Mientras más grande sea el residuo más asociada.


### Con nuestra tabla de Tipo de institución y documentación en regla

Institucion<- matrix(c(23,34,35,132),nrow=2,byrow=TRUE)

colnames(Institucion)<-c("Gobierno","NoGobierno")
rownames(Institucion)<-c("SI","NO")
names(dimnames(Institucion))<-c("EnRegla","Instituciones")
print(Institucion)

###Tabla de valores esperados y observados

Obs<-chisq.test(Institucion)$observed
Esp<-chisq.test(Institucion)$expected
Obs;Esp

chisq.test(Institucion)

mosaicplot(Institucion,color=TRUE,shade=T,main="mosaico: Tipo Institución-Documentación en Regla")

assoc(Institucion,color=TRUE,shade=T,main="mosaico: Tipo Institución-Documentación en Regla")

### Una con nuestra base de datos

X<-table(temperatura,linea)

chisq.test(X)

mosaicplot(X,color=TRUE,shade=T,main="mosaico: temperatura lineas")
assoc(X,color=TRUE,shade=T,main="mosaico: temperatura lineas")

### Fuerza de asociación en la tabla Institución documentos en regla

assocstats(Institucion)





















