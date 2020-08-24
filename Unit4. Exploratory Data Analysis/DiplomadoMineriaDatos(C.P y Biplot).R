###DIPLOMADO MINERÍA DE DATOS (C.P y Biplot)

### Introduccion a R

install.packages(c("rgl","dichromat","onion","mnormt","plot3D","lattice","scatterplot3d","TeachingDemos","aplpack","psych","profileR","car","ellipse","bpca"))



library(rgl)
library(dichromat)
library(onion)
library(mnormt)
library(plot3D)
library(MASS)
library(lattice)
library(scatterplot3d)
library(TeachingDemos)
library(aplpack)
library(psych)
library(profileR)
library(car)
library(ellipse)
library(bpca)



### Show gráfico


### Demostracion de las utilidades de esta libreria 3D dinámica

demo(hist3d)

example(surface3d)

example(bg3d) 

example(plot3d)

demo(bivar)

demo(subdivision)

demo(abundance) 

demo(regression) 



### Arcoiris

n<-40
plot(1:n, pch=CIRCLE<-16, cex=1:n, col=rainbow(n))


### Gradiente tonal

win.graph()

palette( rev(rich.colors(32)) ) # colors: 1 to 32
plot(log2(somite$Signal.1L), log2(somite$Signal.1R),
col=1 + 31*somite$worst.p)

hue <- seq(0.0, 1.0, by=1/40)
pie(rep(1,40),
labels=formatC(hue, digits=3,
format="f"),
cex=0.75,
col=hsv(hue, 1.0, 1.0),
radius=1.0,
main="HSV (S=1, V=1)" )

### Gradiente tonal 2

win.graph()

par(mfrow=c(6,1), mar=c(3,1,0,1))
BOTTOM <- 1
colorstrip <- function(colors, description,
ShowAxis=FALSE)
{
count <- length(colors)
m <- matrix(1:count, count, 1)
image(m, col=colors, ylab="", axes=FALSE)
if (ShowAxis)
{
axis(BOTTOM)
}
mtext(description, BOTTOM, adj=0.5, line=0.5)
}
COLOR.COUNT <- 256
colorstrip(rainbow(COLOR.COUNT), "rainbow")
colorstrip(heat.colors(COLOR.COUNT), "heat.colors")
colorstrip(terrain.colors(COLOR.COUNT), "terrain.colors")
colorstrip(topo.colors(COLOR.COUNT), "topo.colors")
colorstrip(cm.colors(COLOR.COUNT),
"cm.colors (cyan-magenta)")
colorstrip(gray(0:COLOR.COUNT / COLOR.COUNT),"gray")


### Gradiente tonal 3

data(dalton)
par(mfrow=c(3,1))
image(matrix(1:256,128),col=dalton.colors$deutan)
image(matrix(1:256,128),col=dalton.colors$protan)
image(matrix(1:256,128),col=dalton.colors$normal)

###

par(mfrow=c(1,2))

data(bunny)
p3d(bunny,theta=3,phi=104,box=FALSE,col.main="purple",main="FELIZ DIA DEL AMOR Y LA AMISTAD",col.sub="darkred",
cex.main=1.5, cex.sub=1.5)

dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
 xhrt <- function(t) 16*sin(t)^3
 yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
 dat$y=yhrt(dat$t)
 dat$x=xhrt(dat$t)
 with(dat, plot(x,y, type="l",col.main="pink",main="FELIZ DIA DEL AMOR Y LA AMISTAD"))
with(dat, polygon(x,y, col="purple"))

points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch=169, font=5,col="red")

###

mu <- c(0,0)
sigma <- matrix(c(1,.8,.8,1),2,2)                                
xv<-seq(-4,4,0.1)
yv<-seq(-4,4,0.1)
z<-NULL
for(x in xv){
   f <- dmnorm(cbind(x,yv), mu, sigma)
   z<-rbind(z,f)   
}

wireframe(z,drape=T)

persp(xv,yv,z,col=rainbow(5))

persp(xv,yv,z, box=T,axes=T, ticktype="detailed",col=rainbow(10))


###

mu <- c(0,0)
sigma <- matrix(c(1,0,0,1),2,2)                                
x<-seq(-4,4,0.1)
y<-seq(-4,4,0.1)
f<-function(x,y){dmnorm(cbind(x,y), mu, sigma)}
z<-outer(x,y,f)
for(t_ang in seq(0,180,5)){
   persp(xv,yv,z, theta=t_ang,phi=0,col=rainbow(10))

}

###
mu <- c(0,0)
sigma <- matrix(c(1,0,0,1),2,2)                                
x<-seq(-4,4,0.1)
y<-seq(-4,4,0.1)
f<-function(x,y){dmnorm(cbind(x,y), mu, sigma)}
z<-outer(x,y,f)
for(p_ang in seq(0,360,5)){
   persp(xv,yv,z, theta=0,phi=p_ang,col=rainbow(30:40))
}

###

rossler<-function(n=2000,a=.2,b=.2,c=1.7,x0=0.0001,y0=0.0001,z0=0.0001){
x<-c(x0,rep(NA,n-1))
y<-c(y0,rep(NA,n-1))
z<-c(z0,rep(NA,n-1))
h<-0.015
for (i in 2:n){
x[i]<-x[i-1]-h*(y[i-1]+z[i-1])
y[i]<-y[i-1]+h*(x[i-1]+a*y[i-1])
z[i]<-z[i-1]+h*(b+z[i-1]*(x[i-1]-c))
}
require(rgl)
rgl.clear()
rgl.points(x,y,z, color=heat.colors(n), size=1)
}

rossler(2000,x0=3,y0=4,z0=.4)



###


par(mfrow = c(2, 2), mar = c(0, 0, 0, 0))
# Shape 1
M <- mesh(seq(0, 6*pi, length.out = 80),
seq(pi/3, pi, length.out = 80))
u <- M$x ; v <- M$y
x <- u/2 * sin(v) * cos(u)
y <- u/2 * sin(v) * sin(u)
z <- u/2 * cos(v)
surf3D(x, y, z, colvar = z, colkey = FALSE, box = FALSE)
# Shape 2: add border
M <- mesh(seq(0, 2*pi, length.out = 80),
seq(0, 2*pi, length.out = 80))
u <- M$x ; v <- M$y
x <- sin(u)
y <- sin(v)
z <- sin(u + v)
surf3D(x, y, z, colvar = z, border = "black", colkey = FALSE)
# shape 3: uses same mesh, white facets
x <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*cos(v)
y <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*sin(v)
z <- sin(v/2)*sin(u) + cos(v/2)*sin(2*u)
surf3D(x, y, z, colvar = z, colkey = FALSE, facets = FALSE)
# shape 4: more complex colvar
M <- mesh(seq(-13.2, 13.2, length.out = 50),
seq(-37.4, 37.4, length.out = 50))
u <- M$x ; v <- M$y
b <- 0.4; r <- 1 - b^2; w <- sqrt(r)
D <- b*((w*cosh(b*u))^2 + (b*sin(w*v))^2)
x <- -u + (2*r*cosh(b*u)*sinh(b*u)) / D
y <- (2*w*cosh(b*u)*(-(w*cos(v)*cos(w*v)) - sin(v)*sin(w*v))) / D
z <- (2*w*cosh(b*u)*(-(w*sin(v)*cos(w*v)) + cos(v)*sin(w*v))) / D
surf3D(x, y, z, colvar = sqrt(x + 8.3), colkey = FALSE,
border = "black", box = FALSE)


###

# 3D Plot of Half of a Torus
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))
R <- 3
r <- 2
x <- seq(0, 2*pi,length.out=50)
y <- seq(0, pi,length.out=50)
M <- mesh(x, y)
 
alpha <- M$x
beta <- M$y
 
 
surf3D(x = (R + r*cos(alpha)) * cos(beta),
       y = (R + r*cos(alpha)) * sin(beta),
       z = r * sin(alpha),
       colkey=FALSE,
       bty="b2",
       main="Half of a Torus")


open3d(windowRect=c(50,50,800,800))

x <- seq(-10, 10, length=20)
y <- seq(-10, 10, length=20)
z <- outer(x,y, function(x,y) dnorm(x, 2, 3)*dnorm(y, 3, 7))

palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(z, 256)
persp3d(x, y, z, col=col.table[col.ind])


####################################################################################################################################################################
####################################################################################################################################################################

### Gráficas multivariadas

data(USArrests)

USArrests

class(USArrests)

apply(USArrests,2,mean)

cov(USArrests)

var(USArrests)

cor(USArrests)



plotcorr(cor(USArrests), type="lower", diag=FALSE, main="Bivariate correlations",col=rainbow(6))



names(USArrests)<-c("Asesinatos","Asaltos","Pro.pob.urbana","Violación")

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="magenta", ...)
}

panel.cor <- function(x, y)
{
par(usr = c(0, 1, 0, 1))
txt <- as.character(format(cor(x, y), digits=2))
text(0.5, 0.5, txt, cex = 6* abs(cor(x, y)))
}

panel.box <- function(x, ...)
{
usr <- par("usr",bty='n'); on.exit(par(usr))
par(usr = c(-1,1, min(x)-0.5, max(x)+0.5))
b<-boxplot(x,plot=FALSE)
whisker.i<-b$stats[1,]
whisker.s<-b$stats[5,]
hinge.i<-b$stats[2,]
mediana<-b$stats[3,]
hinge.s<-b$stats[4,]
rect(-0.5, hinge.i, 0.5,mediana,...,col="darkviolet")
segments(0,hinge.i,0,whisker.i,lty=2)
segments(-0.1,whisker.i,0.1,whisker.i)
rect(-0.5, mediana, 0.5,hinge.s,...,col="darkviolet")
segments(0,hinge.s,0,whisker.s,lty=2)
segments(-0.1,whisker.s,0.1,whisker.s)
}



pairs(USArrests, lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist, 
cex.labels = 2, font.labels=2,main="Gráfica utilizando el comando pairs")



pairs(USArrests,lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)

pairs(USArrests,lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.box)

pairs.panels(USArrests)

scatterplotMatrix(USArrests)


### Stars

stars(USArrests,col.stars =10:59)
title("Gráfica de estrellas: USArrests") 


stars(USArrests,draw.segments=TRUE)

title("Gráfica de estrellas: USArrests")

### Chernoff faces


faces(USArrests,main="Caritas de Chernoff: USArrests")


faces2(USArrests)


faces(USArrests,main="Caritas de Chernoff: USArrests",ncolors = 50)

legend(locator(1),legend="Caritas de Chernoff: USArrests",bty="n")


### Profiles

profileplot(USArrests,person.id=rownames(USArrests),by.pattern=TRUE,original.names=TRUE)

### 

parcoord(USArrests,col=rainbow(50))


### Curvas de Andrew

x<-USArrests
nt<-50
minx<-apply(x,2,min)
maxx<-apply(x,2,max)
rangex<-maxx-minx
for (i in 1:ncol(x))
	x[,i]<-(x[,i]-minx[i])/rangex[i]
x<-2*x-1
t<-seq(-pi,pi,length=nt)
nx<-nrow(x)
Aout<-matrix(0,nx,nt)
for (i in 1:nx)
	Aout[i,]<-rep(x[i,1]/sqrt(2),nt)

np<-floor((ncol(x)-1)/2)

if (np > 0)
	for (j in 1:np)
		for (i in 1:nx)
			Aout[i,]<-Aout[i,]+x[i,2*j]*sin(t*j)+x[i,2*j+1]*cos(t*j)

if (ncol(x) > 2*np+1)
	for (i in 1:nx)
		Aout[i,]<-Aout[i,]+x[i,ncol(x)]*sin(t*(np+1))



matplot(t,t(Aout),type='l',xlab='t',ylab='f(t)',main="Curvas Andrews: USArrests",lty=1,pch=1:3)


#COMPONENTES PRINCIPALES

#elipse

sigma<-matrix(c(1,0.99,0.99,1),2,2)
set.seed(7219)
puntos<-mvrnorm(1000,rep(0,2),sigma)

data.ellipse(puntos[,1], puntos[,2],levels=c(0.01,0.99))
abline(0,1)

#####INICIA SESION C.P.

ciudades<-c("Atlanta","Baltimore","Boston","Buffalo","Chicago","Cincinnati","Cleveland","Dallas","Detroit","Honolulu","Houston",
"Kansas C.","L.A", "Milwaukee","Minneapolis", "N.Y","Philadelphia","Pittsburg","St.Louis","SanDiego","San Fco","Seattle","Washington D.C")

productos<-c("Pan","Carne","Leche","Naranjas","Tomates")

pan<-c(24.5,26.5,29.7,22.8,26.7,25.3,22.8,23.3,24.1,29.3,22.3,26.1,26.9,20.3,24.6,30.8,24.5,26.2,26.5,25.5,26.3,22.5,24.2)

carne<-c(94.5,91,100.8,86.6,86.7,102.5,88.8,85.5,93.7,105.9,83.6,88.9,89.3,89.6,92.2,110.7,92.3,95.4,92.4,83.7,87.1,77.7,93.8)

leche<-c(73.9,67.5,61.4,65.3,62.7,63.3,52.4,62.5,51.5,80.2,67.8,65.4,56.2,53.8,51.9,66.0,66.7,60.2,60.8,57.0,58.3,62,66)

naranja<-c(80.1,74.6,104,118.4,105.9,99.3,110.9,117.9,109.7,133.2,108.6,100.9,82.7,111.8,106.0,107.3,98,117.1,115.1,92.8,101.8,91.1,81.6)

tomate<-c(41.6,53.3,59.6,51.2,51.2,45.6,46.8,41.8,52.4,61.7,42.4,43.2,38.4,53.9,50.7,62.6,61.7,49.3,46.2,35.4,41.5,44.9,46.2)


precios[c("Detroit","L.A"),]   ##No se parece tanto
precios[c("Buffalo","Houston"),]   ##No se parece tanto



precios<-data.frame(cbind(pan,carne,leche,naranja,tomate))
row.names(precios)<-ciudades


faces2(precios)

faces(precios)

stars(precios)

apply(precios,2,mean) ##Para la columna

cor(precios)
cov(precios)   ##¿Utilizar la matriz de correlación o covarianzas para utilizar c.p? 
		   ## Si utilizáramos la de cov la naranja sería la más grande. Pues está relacionada a las unidades, por lo tanto, se utiliza la matriz de correlación.
pairs(precios)


############################Componentes a mano

CMat<-cor(precios)

v.propios<-eigen(CMat)$values   ##Valores propios de la matriz. El primero es el más grande y así sucesivamente.

v.propios  				  ##Veremos qué tanto de la varianza podría explicar los 3 primeros componentes.

vect.propios<-eigen(CMat)$vectors

vect.propios                    ##En el componente 2 quien domina los precios es la naranja, los edos de abajo determina el precio alto. Para el comp. 1 estar a la izq son precios más altos.              

varexplicada<-v.propios/sum(v.propios)   ##Los 3 explican el 85% de la varianza.

Z<-as.matrix(precios)%*%vect.propios     ##Componentes de los sujetos. Coordenadas de los sujetos: sujetos x componetes i.e 23*5.

plot(Z[,1],Z[,2])
text(Z[,1],Z[,2],labels=ciudades)

plot(Z[,1],Z[,2],cex=0.8)
text(Z[,1],Z[,2],labels=abbreviate(ciudades))

precios1<-scale(precios,center=T,scale=T)    ##Centrar: restar media y escalar: dividir entre su varianza.

Z1<-as.matrix(precios1)%*%vect.propios

plot(Z1[,1],Z1[,2],cex=0.8)

text(Z1[,1],Z1[,2],labels=abbreviate(ciudades))

abline(h=0);abline(v=0)


############

#################Con rutina de R

p.comp<-princomp(precios,cor=TRUE)

names(p.comp)

plot(p.comp)

plot(p.comp$sdev,type="o",pch=10,main="Gráfica de codo",ylab="eigenvalores",xlab="Indice")


plot(p.comp$scores[,1],p.comp$scores[,2],col="darkred",main="Representación gráfica con dos componentes",col.main="darkblue",col.lab="darkgreen",
     xlab="Componente 1",ylab="componente 2")

abline(h=0);abline(v=0)

text(p.comp$scores[,1],p.comp$scores[,2],labels=abbreviate(ciudades),col=rainbow(10))

#biplot(p.comp)

identify(p.comp$scores[,1],p.comp$scores[,2])

scatterplot3d(p.comp$scores[,1],p.comp$scores[,2],p.comp$scores[,3], type="h", highlight.3d=TRUE,
angle=55, scale.y=0.7, pch=16, main = "3D C.P. consumo de alimentos CIUDADES USA")


##Vectores propios son los coeficientes en cada componente


#text(p.comp$scores[,1],p.comp$scores[,2],p.comp$scores[,3],labels=ciudades )

apply(precios,2,mean)
precios["Chicago",]    ##Observamos que Chicago está en el centro, como las escalamos, podríamos pensar que está cerca de la media.
precios["Atlanta",] 

identify(p.comp$scores[,1],p.comp$scores[,2],p.comp$scores[,3],labels=ciudades,roughly 0.25)

plot3d(p.comp$scores[,1],p.comp$scores[,2],p.comp$scores[,3], xlab="CP1",ylab="CP2",zlab="CP3",type = "s",cex = 0.05,
col=c("red","blue","green"))

### Chidas las pelotitas, pero a qué cd. corresponde cada una?

precios[order(naranja),]     ##Ordena por el precio.

cor(precios)


plot3d(p.comp$scores[,1],p.comp$scores[,2],p.comp$scores[,3], xlab="CP1",ylab="CP2",zlab="CP3",cex = 0.1,
font=2,col="brown",type="n")

text3d(p.comp$scores[,1],p.comp$scores[,2],p.comp$scores[,3], text=abbreviate(ciudades),font=2,col=rainbow(23))


### Biplot 2D

plot(bpca(precios, meth="sqrt"),
main="Biplot", sub="",xlim=c(-1,3.1),ylim=c(-2,3.5),xlab="Componente 1",ylab="Componente 2",
var.factor=2, var.cex=.6, var.col=rainbow(23), var.pch="v",
obj.pch="o", obj.cex=.8, obj.col=rainbow(8), obj.pos=1, obj.offset=.8,col.main="darkblue",col.lab="darkred")



### Bipot 3D estático



(obj.lab <- paste(c("Atlanta","Baltimore","Boston","Buffalo","Chicago","Cincinnati","Cleveland","Dallas","Detroit","Honolulu","Houston",
"Kansas C.","L.A", "Milwaukee","Minneapolis", "N.Y","Philadelphia","Pittsburg","St.Louis","SanDiego","San Fco","Seattle","Washington D.C"),sep=''))

plot(bpca(precios,
          d=1:3),
     var.factor=1.2,
     var.color=rainbow(5),
     var.cex=1.2,
     obj.names=FALSE,
     obj.cex=1,
     obj.col=rainbow(23),
     obj.labels=obj.lab
)


###3D estático


plot(bpca(precios,
          d=1:3),
     var.cex=1.2,
     obj.cex=0.8,
     obj.labels=abbreviate(obj.lab),
      obj.col=rainbow(23)
)




### Biplot 3D dinámico

plot(bpca(precios,
          d=1:3),
      rgl.use=TRUE,
     var.factor=1.2,
     var.color=rainbow(5),
     var.cex=2,
     obj.names=FALSE,
     obj.cex=1,
     obj.col=rainbow(23),
     obj.labels=obj.lab
)



plot(bpca(precios,d=1:3),
      rgl.use=TRUE,
      var.cex=1.5,
      obj.cex=1.1,
      obj.labels=abbreviate(obj.lab),
      obj.col=rainbow(23)
)




plot(bpca(precios,
method='hj',
          d=1:3),
     rgl.use=TRUE,
     var.col='brown',
     var.factor=1.2,
     var.cex=1.2,
     obj.names=FALSE,
     obj.cex=.8,
     obj.col=rainbow(23),
     obj.labels=obj.lab,
     simple.axes=FALSE,
     box=TRUE)

