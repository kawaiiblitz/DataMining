### DIPLOMADO MINERÍA DE DATOS (Análisis de Regresión)

install.packages(c("car","ADGofTest","lmtest","arm","MASS","ggplot2","boot","psych","alr3","corrplot","PerformanceAnalytics","GGally","sjPlot","effects","visreg"))

library(car)
library(ADGofTest)
library(lmtest)
library(arm)
library(MASS)
library(ggplot2)
library(boot)
library(psych)
library(alr3)
library(corrplot)
library(PerformanceAnalytics)Interacción TV radio
library(GGally)
library(sjPlot)
library(effects)
library(visreg)


### Datos

datos<-mtcars

dim(datos)

colnames(datos)

attach(datos)

### mpg: Millas por galón; cyl: Número de cilindros; disp: Desplazamiento(cu.in.); hp: Caballos de potencia; drat:Relación de eje trasero
### wt: Peso(lb/1000); qsec: Tiempo en un cuarto de milla; vs: V/S; am: Transmisión(0=automático,1=manual); 
### gear: Número de velocidades; carb: Número de carburadores

### OBJETIVO: Relación entre el rendimiento del automóvil dado en las millas recorridas por galón (3.785 litros) de combustible 
### y algunas características de los automoviles (los sujetos aquí son las marcas de autos)

### Regresión lineal simple

pairs(datos,main="datos",col=rainbow(12),pch=19)

### Diagrama de dispersion

plot(wt,mpg,main="Diagrama de dispersión: Millas por galón vs. peso",col="red",col.main="darkblue",col.lab="darkblue",pch=19)
lines(lowess(mpg~wt),col="blue")

cor(wt,mpg)

cor.test(wt,mpg,alternative="two.sided",conf.level=0.95)

cor.test(wt,mpg, method="spearman",alternative="two.sided",conf.level=0.95)
##La correlación es muy alta. 

### Ajuste "manual"

n<-length(wt)

X<-cbind(rep(1,n),wt) ##El 1 en las columnas es porque el Bo es para todos.
y<-mpg
beta.gorro<-solve(t(X)%*%X)%*%t(X)%*%y  ### resolución por álgebra lineal

beta.gorro ##El Bo no tiene interpretación porque no hay auto con peso 0. El B1 si: Por cada tonelada, el rendimiento en promedio disminuye 5.34.


beta1<-cov(wt,y)/var(wt)      ### resolución por mínimos cuadrados
beta0<-mean(y)-beta1*mean(wt)    ### resolución por mínimos cuadrados

c(beta0,beta1)

### Ajuste utilizando funciones de R

modelo<-lm(mpg~wt)   ##Variable de respuesta mpg (millas por galón). LINEAL MODEL.
			   ##Los coeficientes son distintos de cero porque el p value es pequeño. Son estadísticaente significativos. La relación peso rendimiento es significativa. El peso del auto si determina el rendimiento del auto. No sabemos si es el único.  			  
                     ## La R2 nos dice que la variabilidad del rendimiento es explicada en un 75% por el peso. 
summary(modelo)
##Vemos que un t2 es una F. 



### El modelo ajustado es: mpg=37.2851-5.3445*wt

plot(wt,mpg,main="Modelo ajustado",col="red",col.main="darkblue",col.lab="darkblue",pch=19)
abline(modelo,col="blue")

text(3.5,30,paste("mpg=",round(coef(modelo)[1],3),"",round(coef(modelo)[2],3),"wt",sep=""),cex=1.5,col="darkgreen")
text(5,30,paste("R^2=",round(summary(modelo)$r.squared,3),sep=""),cex=1.5,col="blue")

### Verificación de los supuestos

### Supuestos: media error=cero, varianza error=cte, errores no correlacionados, distribución normal de los errores

### Gráficas generadas de manera automática

plot(modelo)  ### Medio feas

### Todas juntas

par(mfrow=c(2,2))

plot(modelo,which=1, col="red")
plot(modelo,which=2,col="green")
plot(modelo,which=3,col="darkblue")
plot(modelo,which=4,col="darkred")

### Una importante
##Potencialmente son normales

rs<-rstandard(modelo)

plot(modelo$fitted.values,rs,main="Gráfica residuos estandarizados vs. valores ajustados",ylab="residuos estandarizados",
     xlab="valores ajustados",col="red",ylim=c(-3.5,3.5),col.main="darkmagenta") 
abline(h=0,col="blue",lty=3)
abline(h=c(-2,2),col="darkred",lty=2)
abline(h=c(-3,3),col="blue",lty=3)
 
identify(modelo$fitted.values,rs, labels=rownames(datos),col="darkgreen")

##abbreviate

### Y a todo esto, una prueba formal de bondad de ajuste dice que los residuos estandarizados son normales?

ad.test(pnorm(rs,mean=0,sd=1))   ##Los residuos son granes porque el p-value es grande. 

### PERFECTO!

### Prueba sobre homoscedasticidad de varianza. H0: La varianza es constante vs. Ha: No lo es

bptest(mpg~wt,data=datos) ##Nula: Los residuos tienen varianza constante

### Sin broncas.

### El modelo lineal ajusta de forma adecuada, o es necesario incluir algún término no lineal?
### Recordar que la gráfica de residuos contra valores ajustados mostraba una ligera tendencia cuadrática
### Intentemos verificar esta situación

resettest(mpg~wt,power=2,type="regressor",data=datos)

### Obsérvese que el modelo en la hipótesis nula es mpg~wt es el modelo lineal que estamos ajustando, mientras que el modelo
### en la hipótesis alternativa es este modelo más un término cuadrático: mpg~wt+wt^2. Ya que rechazamos la hipótesis nula: p-value=0.00286
### entonces, es indicativo que necesitamos agregar una variable cuadrática. Anexemos esta variable cuadrática a la base


datos$w2<-wt^2

colnames(datos)

attach(datos)

### Nuevo modelo

modelo1<-lm(mpg~wt+w2,data=datos)

summary(modelo1)

### Cómo sabemos que ajusta mejor que el modelo anterior???

anova(modelo,modelo1)     ##Si los dos son buenos me quedo con el que tenga menos parámetros por el principio de Parsimonía.
                          ##Si lo mejora.

### El nuevo modelo (modelo1)ajustado es: mpg=49.9308-13.3803*wt+1.1711*wt^2

plot(wt,mpg,main="Modelo1 ajustado",col="red",col.main="darkblue",col.lab="darkblue",pch=19)
curve(49.9308-13.3803*x+1.1711*x^2,from=min(wt),to=max(wt),col="blue",add=T)

text(3.5,30,paste("mpg=",round(coef(modelo1)[1],3),"",round(coef(modelo1)[2],3),"wt","+",
round(coef(modelo1)[3],3),"wt^2",sep=""),cex=1.5,col="darkgreen")

text(5,30,paste("R^2=",round(summary(modelo1)$r.squared,3),sep=""),cex=1.5,col="blue")


### Selección via Box-Tidwell

boxTidwell(mpg,wt)

### Sugiere una transformación 1/sqrt(x)

### Modelo con la transformación sugerida por Box-Tidwell

datos$sqrtwt<-wt^(-1/2)

colnames(datos)

attach(datos)


### El modelo 2 con esta transformacion es

modelo2<-lm(mpg~wt+sqrtwt,data=datos)

summary(modelo2)

### Mejor que el original y el modelo1?

anova(modelo,modelo2)

anova(modelo1,modelo2)  ### No es adecuado. No son comparables pues se necesita que el más grande contenga al otro y este son del mismo tamaño. 

AIC(modelo1,modelo2) ##Compara modelos de cualquier naturaleza. El que sea más chico.
BIC(modelo1,modelo2)

### Mejor modelo, AIC mas pequeño. En este caso es practicamente igual: Modelo1:158.0484 vs. Modelo2:159.2876

### El nuevo modelo (modelo2)ajustado es: mpg=-7.6579-0.5927*wt+51.3189*wt^(-1/2)

plot(wt,mpg,main="Modelo2 ajustado",col="red",col.main="darkblue",col.lab="darkblue",pch=19)
curve(-7.6579-0.5927*x+51.3189*x^(-1/2),from=min(wt),to=max(wt),col="blue",add=T)

text(3.5,30,paste("mpg=",round(coef(modelo2)[1],3),"",round(coef(modelo2)[2],3),"wt","+",
round(coef(modelo1)[3],3),"wt^(-1/2)",sep=""),cex=1.5,col="darkgreen")

text(5,30,paste("R^2=",round(summary(modelo2)$r.squared,3),sep=""),cex=1.5,col="blue") 


### Hay alguna sugerencia para trasformar la respuesta???

boxcox(modelo)

### Podría ser lambda=0, es decir, ytransformada=log(y). Esto es porque contiene al 0.

datos$lmpg<-log(mpg)
colnames(datos)

attach(datos)

###

modelo3<-lm(lmpg~wt,data=datos)
summary(modelo3)

### Es mejor que el modelo1???

summary(modelo1);summary(modelo3)

### El nuevo modelo (modelo3)ajustado es: log(mpg)=3.83191-0.27178*wt

plot(wt,lmpg,main="Modelo ajustado",col="red",col.main="darkblue",col.lab="darkblue",pch=19)
abline(modelo3,col="blue")

text(3.5,log(30),paste("lmpg=",round(coef(modelo3)[1],3),"-",round(coef(modelo3)[2],3),"wt",sep=""),cex=1.5,col="darkgreen")

text(5,log(30),paste("R^2=",round(summary(modelo3)$r.squared,3),sep=""),cex=1.5,col="blue") 


### Los modelos parecen equivalentes. Puede ser complicado la interpretacion con la transformación logaritmo y el mejor parece ser el
### modelo 1 

### Ton's optamos por el modelo 1

### INTERPRETACION DE LOS PARAMETROS???

### Verificando los supuestos del modelo

par(mfrow=c(2,2))

plot(modelo1,which=1, col="red")
plot(modelo1,which=2,col="green")
plot(modelo1,which=3,col="darkblue")
plot(modelo1,which=4,col="darkred")

### La gráfica con los residuos estandarizados

rs1<-rstandard(modelo1)

plot(modelo1$fitted.values,rs1,main="Gráfica residuos estandarizados vs. valores ajustados: modelo1",ylab="residuos estandarizados",
     xlab="valores ajustados",col="red",ylim=c(-3.5,3.5),col.main="darkmagenta") 
abline(h=0,col="blue",lty=3)
abline(h=c(-2,2),col="darkred",lty=2)
abline(h=c(-3,3),col="blue",lty=3)
 
identify(modelo1$fitted.values,rs1, labels=rownames(datos),col="darkblue")


### Sigue cumpliendo normalidad y homoscedasticidad?

ad.test(pnorm(rs1,mean=0,sd=1))   ##Anderson Darling


bptest(mpg~wt+w2,data=datos)

### "SIMON"


### Hay observaciones atípicas?

outlierTest(modelo1)

###

qqPlot(modelo1,id.n=3,col=c("blue"))

### Intervalos de confianza sobre los parámetros del modelo

confint(modelo1)  ### default 95%

confint(modelo1,level=0.99)

### Visualización de los intervalos

coefplot(modelo1,intercept=TRUE,col=c("blue","red","green"))

###### Evidencia empírica de la distribución de los parámetros estimados.

DAT<-cbind(mpg,wt,w2)
n<-5000
empbetas<-matrix(0,n,4)
id<-matrix(0,n,20)
for(i in 1:n){
id[i,]<-sample(seq(1:length(mpg)),20,replace=FALSE)
samdatos<-DAT[id[i,],]
empbetas[i,]<-c(lm(samdatos[,1]~samdatos[,2]+samdatos[,3])$coefficients,summary(lm(samdatos[,1]~samdatos[,2]+samdatos[,3]))$sigma)

}

par(mfrow=c(2,2))


plot(density(empbetas[,1]),col="blue",main=expression(paste("Distribución empírica de ", hat(beta)[0],"")),col.main="darkgreen",xlab=expression(hat(beta)[0]),col.lab="darkred")

abline(v=modelo1$coef[1],col="red",lty=3)

plot(density(empbetas[,2]),col="blue",main=expression(paste("Distribución empírica de ", hat(beta)[1],"")),col.main="darkred",xlab=expression(hat(beta)[1]),col.lab="darkred")

abline(v=modelo1$coef[2],col="red",lty=3)

plot(density(empbetas[,3]),col="blue",main=expression(paste("Distribución empírica de ", hat(beta)[2],"")),col.main="darkorange",xlab=expression(hat(beta)[2]),col.lab="darkred")

abline(v=modelo1$coef[3],col="red",lty=3)

plot(density(empbetas[,3]),col="blue",main=expression(paste("Distribución empírica de ", hat(sigma)^2,"")),col.main="darkviolet",xlab=expression(hat(sigma)^2),col.lab="darkred")


### Predicción con el modelo

conf1<-predict.lm(modelo1,interval="confidence")   ##El intervalo de confianza

pred1<-predict.lm(modelo1,interval="prediction")   ##El intervalo de predicción de la respuesta y es más grande.

vpred<-cbind(conf1,pred1)

vpred[1:10,]

wt1<-wt
w21<-wt1^2

conf1<-as.data.frame(predict(modelo1, newdata=data.frame(wt=wt1,w2=w21), level=0.95, interval="confidence"))
pred1<-as.data.frame(predict(modelo1, newdata=data.frame(wt=wt1,w2=w21), level=0.95, interval="prediction"))

plot(wt,mpg,col="darkmagenta",main="Intervalos de predicción y confianza",col.main="darkmagenta",pch=19)

curve(49.9308-13.3803*x+1.1711*x^2,from=min(wt),to=max(wt),col="green",add=T)

lines(wt1,conf1[,2], col="blue", lty=2)
lines(wt1,conf1[,3], col="blue", lty=2)

matlines(wt1,conf1[,c("lwr","upr")],col="red",lty=3,type="l")

matlines(wt1,pred1[,c("lwr","upr")],col="blue",lty=3,type="l")


lines(cbind(new,c.lim$lwr), col="blue", lty="dashed")
lines(cbind(new,c.lim$upr), col="blue", lty="dashed")


conf1<-predict(modelo1,new=wt,interval="confidence")


matlines(wt1,conf1[,c("lwr","upr")],col="red",lty=3,type="l")

matlines(wt1,pred1[,c("lwr","upr")],col="blue",lty=3,type="l")

model<-lm(data=datos,mpg~wt+w2)
datos$model <- stats::predict(model,newdata=data.frame(wt=wt,w2=w2))
err <- stats::predict(model, newdata=data.frame(wt=wt,w2=w2), se = TRUE)
datos$ucl <- err$fit + 1.96 * err$se.fit
datos$lcl <- err$fit - 1.96 * err$se.fit

g <- ggplot(datos)
g <- g + geom_point(aes(x=wt, y = model), size = 2, colour = "blue")
g <- g + geom_smooth(data=datos, aes(x=wt, y=mpg, ymin=lcl, ymax=ucl), size = 1.5, 
            colour = "red", se = TRUE, stat = "smooth")
g

### Predicción para algunos valores particulares

pwt=seq(min(wt),max(wt),length.out=10)
pw2<-pwt^2

pconf<-predict(modelo1,newdata=data.frame(wt=pwt,w2=pw2),level=0.95,interval="confidence")

ppred<-predict(modelo1,newdata=data.frame(wt=pwt,w2=pw2),level=0.95,interval="prediction")

### Graficas con bandas de confianza y predicción para las predicciones
plot(wt,mpg,col="darkmagenta",pch=19)
curve(49.9308-13.3803*x+1.1711*x^2,from=min(wt),to=max(wt),col="darkmagenta",add=T,lwd=3)
lines(pwt,pconf[,2], col="blue", lty=2)
lines(pwt,pconf[,3], col="blue", lty=2)
lines(pwt,ppred[,2], col="red", lty=3,lwd=2)
lines(pwt,ppred[,3], col="red", lty=3,lwd=2)
#identify(wt,mpg, labels=rownames(datos),col="darkblue")




### Modelo simple Bootstrap

betas.boot<-bootCase(modelo1,B=1000,coef) 

est.boot<-apply(betas.boot,2,mean)  

cl<-function(x) quantile(x,c(.025,.975))

### Calcula los intervalos de confianza para cada coeficiente

apply(betas.boot,2,cl) 

### Los intervalos con el modelo  

confint(modelo1) 

### DIAGNOSTICO

influencia<-influence.measures(modelo1)

which(apply(influencia$is.inf, 1, any)) #influyentes sin determinar en qué variable

which(influencia$is.inf, 1)#determinando en qué variable es influyente

influ<-influencia[[1]]  ### Como base de datos 

id<-rownames(mtcars)

plot(influ[,7],type="h",col="darkred",main="Palanca (leverage)",xlab="Indice",ylab=expression(h[ii]),col.lab="darkgreen")

abline(h=10/length(influ[,7]),lty=2,col="darkblue")

identify(influ[,7], labels=id, col="green")


plot(influ[,6],type="h",col="darkred",main="Distancia de Cook",xlab="Indice",ylab=expression(C[ii]),col.lab="darkgreen")

#abline(h=1,lty=2,col="darkblue")

identify(influ[,6], labels=id, col="green")

par(mfrow=c(2,2))

plot(influ[,1],type="h",col="darkred",main="DFbeta0",xlab="Indice",ylab=expression(DFbeta[0]),col.lab="darkgreen")

abline(h=c(2/sqrt(length(influ[,1])),-2/sqrt(length(influ[,1]))),lty=2,col="darkblue")

identify(influ[,1], labels=id, col="green")

plot(influ[,2],type="h",col="darkred",main="DFbeta1",xlab="Indice",ylab=expression(DFbeta[1]),col.lab="darkgreen")

abline(h=c(2/sqrt(length(influ[,2])),-2/sqrt(length(influ[,2]))),lty=2,col="darkblue")

identify(influ[,2], labels=id, col="green")

plot(influ[,3],type="h",col="darkred",main="DFbeta2",xlab="Indice",ylab=expression(DFbeta[2]),col.lab="darkgreen")

abline(h=c(2/sqrt(length(influ[,2])),-2/sqrt(length(influ[,2]))),lty=2,col="darkblue")

identify(influ[,3], labels=id, col="green")

plot(influ[,4],type="h",col="darkred",main="Dffits",xlab="Indice",ylab=expression(Dffits),col.lab="darkgreen")

abline(h=c(2*sqrt(5/length(influ[,4])),-2*sqrt(5/length(influ[,6]))),lty=2,col="darkblue")

identify(influ[,4], labels=id, col="green")



plot(influ[,5],type="h",col="darkred",main="Covratio",xlab="Indice",ylab=expression(Covratio),col.lab="darkgreen")

abline(h=c(1+(15/length(influ[,5])),1-(15/length(influ[,7]))),lty=2,col="darkblue")

identify(influ[,5], labels=id, col="green")

###############################################################################################################################################
#################################################### REGRESIÓN MÚLTIPLE #######################################################################
### Datos de publicidad

###Advertising data displays sales (in thousands of units) for a particular product as a function of advertising
### budgets (in thousands of dollars) for TV, radio, and newspaper media.


publicidad<-read.csv("C:/Users/Rachel/Documents/Raquel/Diplomado Minería de Datos/Módulo 4 Análisis Exploratorio de Datos/Publicidad.csv")

### Histograma y densidad Proclividad genética para la diabetes (diabetes)

p <- ggplot(publicidad, aes(x=TV)) + xlab("Inversión publicitaria en TV")
p + geom_histogram(bins=25)

p <- qplot(TV,data=publicidad,geom="histogram",main="Inversión publicitaria en TV",xlab = "TV",binwidth =20,fill=I("darkgreen"),col=I("white"))
p

p <- ggplot(publicidad, aes(x=TV)) + xlab("Inversión publicitaria en TV")
p+geom_density(aes(x=TV),col=I("darkblue"))

p <- ggplot(publicidad, aes(x=TV)) + stat_density(color="red",position="identity",geom="line")+ggtitle("Densidad: Inversión publicitaria en TV")
p


p <- qplot(radio,data=publicidad,geom="histogram",main="Inversión publicitaria en radio",xlab = "radio",binwidth =2,fill=I("darkblue"),col=I("white"))
p

p <- ggplot(publicidad, aes(x=radio)) + stat_density(color="red",position="identity",geom="line")+ggtitle("Densidad: Inversión publicitaria en radio")
p


p <- qplot(newspaper,data=publicidad,geom="histogram",main="Inversión publicitaria en periódico",xlab = "newspaper",binwidth =5,fill=I("darkblue"),col=I("white"))
p

p <- ggplot(publicidad, aes(x=newspaper)) + stat_density(color="red",position="identity",geom="line")+ggtitle("Densidad: Inversión publicitaria en periódico")
p



p <- qplot(sales,data=publicidad,geom="histogram",main="Ventas",xlab = "Ventas",binwidth =2,fill=I("darkblue"),col=I("white"))
p

p <- ggplot(publicidad, aes(x=sales)) + stat_density(color="red",position="identity",geom="line")+ggtitle("Densidad: Ventas")
p

### Scatterplot

p <- ggplot(publicidad,aes(x=TV,y=sales))+xlab("TV")+ylab("sales")
p + geom_point(color="blue")

p + geom_point(color="red") + geom_smooth()


p <- ggplot(publicidad,aes(x=radio,y=sales))+xlab("radio")+ylab("sales")
p + geom_point(color="blue")

p + geom_point(color="red") + geom_smooth()


p <- ggplot(publicidad,aes(x=newspaper,y=sales))+xlab("newspaper")+ylab("sales")
p + geom_point(color="blue")

p + geom_point(color="red") + geom_smooth()




cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

cor.prob(publicidad[,-1])   ##Ventas y TV son altas significativamente porque es muy pequeño y se encuentran arriba d ela diagonal.


flattenSquareMatrix <- function(m) {
    ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}


flattenSquareMatrix(cor.prob(publicidad[,-1]))


PM<-cor(publicidad[,-1],use="pairwise.complete.obs")
corrplot(PM,method="ellipse")  ##Entre más circular menos nivel de significancia.   

corrplot.mixed(PM,lower="ellipse",upper="number")

chart.Correlation(publicidad[,-1]) 

pm<-ggpairs(publicidad,columns=c("sales","TV","radio","newspaper"))
pm

respuesta<- c("sales")
explicativas<-c("TV","radio","newspaper")

loess_with_cor <- function(data, mapping, ..., method = "pearson") {
  x <- data[[deparse(mapping$x)]]
  y <- data[[deparse(mapping$y)]]
  cor <- cor(x, y, method = method)
  ggally_smooth_loess(data, mapping, ...) +
    ggplot2::geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 3, fontface = "bold"
    )
}

pp<-ggduo(publicidad,respuesta,explicativas,types = list(continuous = loess_with_cor))
suppressWarnings(pp)


### Modelo lineal simple con la variable respuesta (sales) y las predictoras

ggplot(publicidad,aes(TV,sales))+ geom_point(col="darkblue")+stat_smooth(method = "lm", col = "darkred")

qplot(TV, sales, data = publicidad, geom = c("point","smooth"))


ggplot(publicidad,aes(radio,sales))+ geom_point(col="darkblue")+stat_smooth(method = "lm", col = "darkred")

qplot(radio, sales, data = publicidad, geom = c("point","smooth"))


ggplot(publicidad,aes(newspaper,sales))+ geom_point(col="darkblue")+stat_smooth(method = "lm", col = "darkred")

qplot(newspaper, sales, data = publicidad, geom = c("point","smooth"))


ggplotRegression <- function (fit) {
ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  geom_point(col="darkblue") +
  stat_smooth(method = "lm", col = "darkred") +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercepto=",signif(fit$coef[[1]],5 ),
                     "Pendiente=",signif(fit$coef[[2]], 5),
                     "p-value=",signif(summary(fit)$coef[2,4], 5)))
}


pubfit1<- lm(sales~TV,data=publicidad)
ggplotRegression(pubfit1)


pubfit2<- lm(sales~radio,data=publicidad)
ggplotRegression(pubfit2)

pubfit3<- lm(sales~newspaper,data=publicidad)
ggplotRegression(pubfit3)


### El modelo con todas
pubfit<-lm(sales~TV+radio+newspaper,data=publicidad)
summary(pubfit)

### Efecto no lineal de newspaper?

publicidad$new2<-publicidad$newspaper^2
attach(publicidad)

pubfit1<-lm(sales~TV+radio+newspaper+new2,data=publicidad)
summary(pubfit1)

### Interacción TV radio?
##Se platica sobre las interacciones para obtener el producto de estos elementos, aquí la tv y radio.

publicidad$TR<-publicidad$TV*publicidad$radio
attach(publicidad)

pubfit2<-lm(sales~TV+radio+newspaper+TR,data=publicidad)
summary(pubfit2)

anova(pubfit,pubfit2) ## La R2 es bastante significativa, es mucho mejor la interacción.

### Sin newspaper

pubfit3<-lm(sales~TV+radio+TR,data=publicidad)
summary(pubfit3)
##Según esto invertir en periódico no funciona.


### SE VE MUY CHIDO

### Es mejor que el anterior???

anova(pubfit3,pubfit2)

### "NELSON"

### Evaluación de los supuestos

par(mfrow=c(2,2))

plot(pubfit3,which=1, col="red")
plot(pubfit3,which=2,col="green")
plot(pubfit3,which=3,col="darkblue")
plot(pubfit3,which=4,col="darkred")

### Verificando los supuestos del modelo

rs<-rstandard(pubfit3) ##Residuos estandarizados. Aquí 2 generan residuos negativos

plot(pubfit3$fitted.values,rs,main="Gráfica residuos estandarizados vs. valores ajustados",ylab="residuos estandarizados",
     xlab="valores ajustados",col="red",col.main="darkmagenta") 
abline(h=0,col="blue",lty=3)
abline(h=c(-2,2),col="darkred",lty=2)
abline(h=c(-3,3),col="blue",lty=3)
 
identify(pubfit3$fitted.values,rs, labels=id,col="darkgreen")

ad.test(rs,pnorm,mean=0,sd=1)   ##¿Son normales los residuos? La hipótesis nula es que son normales. Rechazamos la hipótesis nula pues es menor que el valor de significancia.

### Varianza constante

bptest(sales~TV+radio+TR,data=publicidad) ##Rechazamos la hip nula de varianza constante.

### No lo cumple

### Hay alguna transformación Box-Cox para la respuesta que pueda mejorar este supuesto y el modelo en general?

boxcox(lm(sales~TV+radio+TR,data=publicidad),lambda=seq(-4,4,by=.25)) ### Muy extraña, dice que sea cuadrada. Y de alguna variable explicativa???
summary(sales)

### Usualmente cuando se presenta una respuesta estrictamente positiva (y también de conteo), una buena alternativa es modelar el logaritmo de esta respuesta. Veamos qué pasa.

publicidad$logsales<-log(publicidad$sales)

attach(publicidad)

pubfit4<-lm(logsales~TV+radio+TR,data=publicidad)
summary(pubfit4)
##La R2 decae.



##Probemos con la cuadrada que nos sugiere Coxx

publicidad$sales2<-publicidad$sales^2

attach(publicidad)

pubfit5<-lm(sales2~TV+radio+TR,data=publicidad)
summary(pubfit5)
##Se muestra mejor. El 99% de la varianza es explicada.

### Evaluación de los supuestos

par(mfrow=c(2,2))

plot(pubfit5,which=1, col="red")
plot(pubfit5,which=2,col="green")
plot(pubfit5,which=3,col="darkblue")
plot(pubfit5,which=4,col="darkred")


rs1<-rstandard(pubfit5)

plot(pubfit4$fitted.values,rs1,main="Gráfica residuos estandarizados vs. valores ajustados",ylab="residuos estandarizados",
     xlab="valores ajustados",col="red",col.main="darkmagenta") 
abline(h=0,col="blue",lty=3)
abline(h=c(-2,2),col="darkred",lty=2)
abline(h=c(-3,3),col="blue",lty=3)
 
identify(pubfit5$fitted.values,rs1, labels=id,col="darkgreen")
##Se ve la relación sinoidal que nos está faltando.


ad.test(rs1,pnorm,mean=0,sd=1)

### Varianza constante

bptest(sales~TV+radio+TR,data=publicidad)


bptest(sales2~TV+radio+TR,data=publicidad)
ncvTest(pubfit5)

### La bronca NO ES LA RESPUESTA, es alguna covariable. Veamos si la transformación BOX-TIDWELL sugerida para TV funciona

publicidad$TV1<-sqrt(publicidad$TV)
attach(publicidad)

pubfit5<-lm(sales~TV+TV1+radio+TR,data=publicidad)
summary(pubfit5)

### Evaluación de los supuestos

par(mfrow=c(2,2))

plot(pubfit5,which=1, col="red")
plot(pubfit5,which=2,col="green")
plot(pubfit5,which=3,col="darkblue")
plot(pubfit5,which=4,col="darkred")


rs2<-rstandard(pubfit5)

plot(pubfit5$fitted.values,rs2,main="Gráfica residuos estandarizados vs. valores ajustados",ylab="residuos estandarizados",
     xlab="valores ajustados",col="red",col.main="darkmagenta") 
abline(h=0,col="blue",lty=3)
abline(h=c(-2,2),col="darkred",lty=2)
abline(h=c(-3,3),col="blue",lty=3)
 
identify(pubfit5$fitted.values,rs2, labels=id,col="darkgreen")

ad.test(rs2,pnorm,mean=0,sd=1)

### Varianza constante

bptest(sales~TV+TV1+radio+TR,data=publicidad)

ncvTest(pubfit5)

### ¿!!!Qué diablos está pasando!!!?


# Evaluate Nonlinearity
# component + residual plot
crPlots(pubfit5)

#### MODELO FINAL: pubfit5 ####

### DIAGNOSTICO
### Hay observaciones atípicas?

outlierTest(pubfit5)

###

qqPlot(pubfit5,id.n=3,col=c("blue"))

### Intervalos de confianza sobre los parámetros del modelo

confint(pubfit5)  ### default 95%

confint(pubfit5,level=0.99)

### Visualización de los intervalos

coefplot(pubfit5,intercept=TRUE,col=rainbow(5))  ### default 95%



coefplot(pubfit5,level=0.99,intercept=TRUE,col=rainbow(5))

### Más mejor

sjp.lm(pubfit5,axisLimits=c(-0.5,2.0),axisTitle.x="beta (blue)",axisLabels.y=lab,axisLabelSize=1,breakLabelsAt=30)

### Revisión de supuestos.

p<-sjp.lm(pubfit5,type ="ma")
plot_grid(p$plot.list,margin = c(0.1, 0.1, 0.1, 0.1))


### Graficas con las medidas de influencia

p<-5

influencia<-influence.measures(pubfit5)

which(apply(influencia$is.inf, 1, any)) #influyentes sin determinar en qué variable

which(influencia$is.inf, 1)#determinando en qué variable es influyente

influ<-influencia[[1]]  ### Como base de datos 

head(influ)

id<-1:dim(pima1)[1]

plot(influ[,9],type="h",col="darkred",main="Palanca (leverage)",xlab="Indice",ylab=expression(h[ii]),col.lab="darkgreen",col.main="darkviolet")
abline(h=2*p/length(influ[,9]),lty=2,col="darkblue")
identify(influ[,9], labels=id, col="darkgreen")


plot(influ[,8],type="h",col="darkred",main="Distancia de Cook",xlab="Indice",ylab=expression(C[ii]),col.lab="darkgreen",col.main="darkviolet")
identify(influ[,8], labels=id, col="darkorange")


par(mfrow=c(2,2))

plot(influ[,1],type="h",col="darkred",main="Dfbeta0",xlab="Indice",ylab=expression(Dfbeta[0]),col.lab="darkgreen",col.main="darkred")

abline(h=c(2/sqrt(length(influ[,1])),-2/sqrt(length(influ[,1]))),lty=2,col="darkblue")

identify(influ[,1], labels=id, col="darkorange")

plot(influ[,2],type="h",col="darkred",main="Dfbeta.TV",xlab="Indice",ylab="Dfbeta.TV",col.lab="darkgreen")

abline(h=c(2/sqrt(length(influ[,2])),-2/sqrt(length(influ[,2]))),lty=2,col="darkblue")

identify(influ[,2], labels=id, col="green")

plot(influ[,3],type="h",col="darkred",main="Dfbeta.TV1",xlab="Indice",ylab="Dfbeta.TV1",col.lab="darkgreen")
abline(h=c(2/sqrt(length(influ[,2])),-2/sqrt(length(influ[,2]))),lty=2,col="darkblue")

identify(influ[,3], labels=id, col="green")

plot(influ[,4],type="h",col="darkred",main="Dfb.radio",xlab="Indice",ylab="Dfb.radio",col.lab="darkgreen")

abline(h=c(2/sqrt(length(influ[,2])),-2/sqrt(length(influ[,2]))),lty=2,col="darkblue")

identify(influ[,4], labels=id, col="green")

par(mfrow=c(2,2))

plot(influ[,5],type="h",col="darkred",main="Dfbeta.TR",xlab="Indice",ylab="Dfbeta.TR",col.lab="darkgreen")

abline(h=c(2/sqrt(length(influ[,2])),-2/sqrt(length(influ[,2]))),lty=2,col="darkblue")

identify(influ[,5], labels=id, col="green")

plot(influ[,6],type="h",col="darkred",main="dffit",xlab="Indice",ylab="dffit",col.lab="darkgreen")

abline(h=c(2/sqrt(length(influ[,2])),-2/sqrt(length(influ[,2]))),lty=2,col="darkblue")

identify(influ[,6], labels=id, col="green")

plot(influ[,7],type="h",col="darkred",main="cov.ratio",xlab="Indice",ylab="cov.ratio",col.lab="darkgreen")

abline(h=c(2*sqrt(p/length(influ[,7])),-2*sqrt(p/length(influ[,7]))),lty=2,col="darkblue")

identify(influ[,7], labels=id, col="green")


### Efectos marginales


plot(allEffects(pubfit5))

### Con visgeg

visreg(pubfit5)

### Uno por uno

visreg(pubfit5,"TV")

visreg(pubfit5,"TV1")

visreg(pubfit5,"radio")

visreg(pubfit5,"TR")








































