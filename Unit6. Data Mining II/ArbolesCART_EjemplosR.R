#Ejemplo con IRIS
data(iris)
names(iris)
table(iris$Species)

library(ggplot2)

#Observemos, distintas especies tienen anchos de p�talo y s�palo caracter�sticos
qplot(Petal.Width,Sepal.Width,data = iris,colour = Species, size = I(4))

#Construyamos el �rbol (instalamos el paquete si no lo tenemos)
install.packages("tree");

#Cargamos la biblioteca
library(tree)

#Tomamos solo 2 caracter�sticas
arbol <- tree(Species ~ Sepal.Width + Petal.Width,data = iris)

#Veamos un resumen del �rbol
summary(arbol)

#Representaci�n gr�fica (b�sica) del �rbol
plot(arbol)
text(arbol)

#Vamos a visualizar las decisiones del �rbol
attach(iris)

#Creamos el diagrama de dispersi�n
grafica <- qplot(Petal.Width,Sepal.Width,data = iris,
                 colour = Species, size = I(4))

#Obtenemos el diagrama de dispersi�n
grafica

#Visualizamos las particiones que cre� el �rbol.
#Petal.Width < 0.8 --> SETOSA
grafica + geom_vline(aes(xintercept = 0.8))

#Petal.Width > 1.75 --> VIRGINICA
grafica + geom_vline(aes(xintercept = 0.8)) +
          geom_vline(aes(xintercept = 1.75))

#Petal.Width < 1.75 AND Petal.Width < 1.35 --> VERSICOLOR
grafica + geom_vline(aes(xintercept = 0.8)) +
          geom_vline(aes(xintercept = 1.75)) +
          geom_vline(aes(xintercept = 1.35))

#Petal.Width > 1.35 AND Sepal.Width < 2.65 --> VIRGINICA
grafica + geom_vline(aes(xintercept = 0.8)) +
          geom_vline(aes(xintercept = 1.75)) +
          geom_vline(aes(xintercept = 1.35)) +
          geom_hline(aes(yintercept = 2.65))

arbol

#Involucrar m�s variables en el �rbol
#Instalamos el paquete (si no contamos con el)
install.packages("rpart")

#Cargamos la biblioteca
library(rpart)

#Creamos el �rbol con RPART
dt.rpart <- rpart(Species ~ .,data = iris,method = "class")  ##Se puede hacer de manera de clasificaci�n (la etiqueta) o de predicci�n (num�rica)
##En la clasificaci�n 3 notamos que es heterog�nea pues tiene 50 y 50 de posibilidades.


#Visualizamos los resultados
dt.rpart

#Dibujamos el �rbol de decisi�n
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dt.rpart,main = "Iris")

################################################
#Veamos el ejemplo de las notas, cargamos los datos del dataset cr�dito
credito.dat <- read.table("C:/Users/Rachel/Documents/Raquel/Diplomado Miner�a de Datos/M�dulo 5 Miner�a de Datos/dataset_clasifica/credito.csv",
                          dec =",",sep =",",header = T)
attach(credito.dat)
credito.dat

#Crear el �rbol de decisi�n, sin configuraci�n
modelo <- rpart(comprar_computadora ~ .,credito.dat[2:6])

#lo visualizamos
rpart.plot(modelo,main = "Cr�dito")

#Ajustamos par�metros, CP (par�metro de complejidad) y minbucket. Si no establezco nada,el cp=0.01 con el m�nimo beneficio que tuviera una partici�n que valiera la pena.
#El problema de algunos es que se puede sobreajustar. Para que est� el �rbol sin podar y sobreajustado el cp va en 0.
#Va en relaci�n a Minsplit por omisi�n es 20 y nosotros tenemos 14, por ello no particion� y Minbucket: Cu�ntas tuplas quieres en cada hoja, por omisi�n es 7 i.e 1/3 de minsplit.

modelo <- rpart(comprar_computadora ~ .,
                control=rpart.control(cp = 0,minbucket = 0),
                credito.dat[2:6])

#Lo visualizamos
rpart.plot(modelo,main = "Cr�dito")

4#minsplit y maxdepth
modelo <- rpart(comprar_computadora ~ .,
                control=rpart.control(minsplit = 5,maxdepth = 10),
                credito.dat[2:6])

#Lo visualizamos
rpart.plot(modelo,main = "Cr�dito")
rpart.plot(modelo,extra=2,under=TRUE,varlen=0,faclen=0,cex=.8)

#Otra forma de visualizarlos, se requiere Rattle
#Instalamos el paquete, si no contamos con el
install.packages("rattle")

#Cargamos la biblioteca
library(rattle)

fancyRpartPlot(modelo)

#Otra forma de ver el �rbol de decisi�n, instalamos el paquete partykit
install.packages("partykit")

#Cargamos el paquete
library(partykit)

plot(as.party(modelo))


#predecir la clase para datos nuevos
credito_nuevo <- read.csv("C:/Users/Rachel/Documents/Raquel/Diplomado Miner�a de Datos/M�dulo 6 Miner�a de Datos II/credito_p.csv")
credito_nuevo

prediccion <- predict(modelo,credito_nuevo,type="class") ##Predicci�n para ese tipo de clase

prediccion

#agregamos la predicci�n al dataset
credito_nuevo$comprar_computadora <- prediccion

#escribimos el dataset
write.csv(credito_nuevo, file = "C:/Users/Rachel/Documents/Raquel/Diplomado Miner�a de Datos/M�dulo 6 Miner�a de Datos II/credito_completo.csv")

#Crear un �rbol de decisi�n con la versi�n gr�fica
rattle()