#Ejemplo con IRIS
data(iris)
names(iris)
table(iris$Species)

library(ggplot2)

#Observemos, distintas especies tienen anchos de pétalo y sépalo característicos
qplot(Petal.Width,Sepal.Width,data = iris,colour = Species, size = I(4))

#Construyamos el árbol (instalamos el paquete si no lo tenemos)
install.packages("tree");

#Cargamos la biblioteca
library(tree)

#Tomamos solo 2 características
arbol <- tree(Species ~ Sepal.Width + Petal.Width,data = iris)

#Veamos un resumen del árbol
summary(arbol)

#Representación gráfica (básica) del árbol
plot(arbol)
text(arbol)

#Vamos a visualizar las decisiones del árbol
attach(iris)

#Creamos el diagrama de dispersión
grafica <- qplot(Petal.Width,Sepal.Width,data = iris,
                 colour = Species, size = I(4))

#Obtenemos el diagrama de dispersión
grafica

#Visualizamos las particiones que creó el árbol.
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

#Involucrar más variables en el árbol
#Instalamos el paquete (si no contamos con el)
install.packages("rpart")

#Cargamos la biblioteca
library(rpart)

#Creamos el árbol con RPART
dt.rpart <- rpart(Species ~ .,data = iris,method = "class")  ##Se puede hacer de manera de clasificación (la etiqueta) o de predicción (numérica)
##En la clasificación 3 notamos que es heterogénea pues tiene 50 y 50 de posibilidades.


#Visualizamos los resultados
dt.rpart

#Dibujamos el árbol de decisión
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dt.rpart,main = "Iris")

################################################
#Veamos el ejemplo de las notas, cargamos los datos del dataset crédito
credito.dat <- read.table("C:/Users/Rachel/Documents/Raquel/Diplomado Minería de Datos/Módulo 5 Minería de Datos/dataset_clasifica/credito.csv",
                          dec =",",sep =",",header = T)
attach(credito.dat)
credito.dat

#Crear el árbol de decisión, sin configuración
modelo <- rpart(comprar_computadora ~ .,credito.dat[2:6])

#lo visualizamos
rpart.plot(modelo,main = "Crédito")

#Ajustamos parámetros, CP (parámetro de complejidad) y minbucket. Si no establezco nada,el cp=0.01 con el mínimo beneficio que tuviera una partición que valiera la pena.
#El problema de algunos es que se puede sobreajustar. Para que esté el árbol sin podar y sobreajustado el cp va en 0.
#Va en relación a Minsplit por omisión es 20 y nosotros tenemos 14, por ello no particionó y Minbucket: Cuántas tuplas quieres en cada hoja, por omisión es 7 i.e 1/3 de minsplit.

modelo <- rpart(comprar_computadora ~ .,
                control=rpart.control(cp = 0,minbucket = 0),
                credito.dat[2:6])

#Lo visualizamos
rpart.plot(modelo,main = "Crédito")

4#minsplit y maxdepth
modelo <- rpart(comprar_computadora ~ .,
                control=rpart.control(minsplit = 5,maxdepth = 10),
                credito.dat[2:6])

#Lo visualizamos
rpart.plot(modelo,main = "Crédito")
rpart.plot(modelo,extra=2,under=TRUE,varlen=0,faclen=0,cex=.8)

#Otra forma de visualizarlos, se requiere Rattle
#Instalamos el paquete, si no contamos con el
install.packages("rattle")

#Cargamos la biblioteca
library(rattle)

fancyRpartPlot(modelo)

#Otra forma de ver el árbol de decisión, instalamos el paquete partykit
install.packages("partykit")

#Cargamos el paquete
library(partykit)

plot(as.party(modelo))


#predecir la clase para datos nuevos
credito_nuevo <- read.csv("C:/Users/Rachel/Documents/Raquel/Diplomado Minería de Datos/Módulo 6 Minería de Datos II/credito_p.csv")
credito_nuevo

prediccion <- predict(modelo,credito_nuevo,type="class") ##Predicción para ese tipo de clase

prediccion

#agregamos la predicción al dataset
credito_nuevo$comprar_computadora <- prediccion

#escribimos el dataset
write.csv(credito_nuevo, file = "C:/Users/Rachel/Documents/Raquel/Diplomado Minería de Datos/Módulo 6 Minería de Datos II/credito_completo.csv")

#Crear un árbol de decisión con la versión gráfica
rattle()