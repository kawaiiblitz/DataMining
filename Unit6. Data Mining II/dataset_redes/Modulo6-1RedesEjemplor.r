#Cargamos el dataset del Zoológico
zoo <- read.csv("C:/Users/Gerardo/Desktop/Ejemplos_clasifica/zoo.csv")
summary(zoo)

#Convertir TRUE/FALSE a 0/1
cols <- sapply(zoo, is.logical)
cols

zoo[,cols] <- lapply(zoo[,cols], as.numeric)
head(zoo)
zoo

#Creamos una muestra (porcentaje)
install.packages("caTools")
library(caTools)

#Tomamos el 66% de las tuplas para entrenamiento, el resto para prueba
split = sample.split(zoo$animal, SplitRatio = 0.66)
summary(split)

#Partimos el dataset basado en el vector booleano split
entrena = subset(zoo, split == TRUE)
prueba = subset(zoo, split == FALSE)

summary(entrena)
summary(prueba)

#Antes de llamar a la función neuralnetwork(), necesitamos crear una 
#fórmula para insertar en el modelo de aprendizaje automático.
#La función neuralnetwork() no acepta el formato típico para una fórmula 
#que implique todas las características (Objetivo ~ .)

caracteristicas <- names(zoo)
caracteristicas

caracteristicas <- caracteristicas[-which(names(zoo) %in% c("animal","type"))]
caracteristicas

# Concatenamos las cadenas
form <- paste(caracteristicas,collapse=' + ')
form

#Hacemos coincidir con el dataset de entrenamiento
form <- paste('typebird + typefish + typeinsect + typeinvertebrate + 
              typemammal + typereptile ~',form)
form

# Convertir a formula
form <- as.formula(form)
form

#Instalamos el paquete para redes neuronales
install.packages("neuralnet")

library(neuralnet)

#ajustar los datos a un marcador binario
entrena <- model.matrix( 
  ~ type + hair + feathers + eggs + milk + airborne + aquatic + 
    predator + toothed + backbone + breathes + venomous + fins + 
    legs + tail + domestic + catsize, data = entrena)
entrena

#Vamos a quitar la columna INTERCEPT (se generó con model.matrix)	
entrena[,2:23]

#Entrenamos la red neuronal
red <- neuralnet(form,entrena[,2:23],hidden=c(5,3),linear.output=FALSE)

#Graficamos la red neuronal
plot(red)
red

#Veamos los resultados del entrenamiento
out <- cbind(red$covariate, 
             + red$net.result[[1]])
out

#Vamos a poner los nombres de columnas
dimnames(out) <- list(NULL,
                      c("hair","feathers","eggs",
                        "milk","airbone","aquatic",
                        "predator","tothed","backbone",
                        "breathes","venomous","fins",
                        "legs","tail","domestic","catsize",
                        "bird","fish","insetc",
                        "invertebrate","mammal","reptile"))
head(out)

#Probamos el modelo entrenado
prueba <- model.matrix( 
  ~ type + hair + feathers + eggs + milk + airborne + aquatic + 
    predator + toothed + backbone + breathes + venomous + fins + 
    legs + tail + domestic + catsize, data = prueba)

#Vamos a quitar INTERCEP y las variables que modelan la etiqueta de clase	
prueba[,8:23]

#Calcular las predicciones sobre el conjunto de prueba
#Debemos quitar las etiquetas de clase
prediccion <- compute(red,prueba[,8:23])

prediccion
#Verificar los resultados
print(head(round(prediccion$net.result,2)))


#Ahora vamos a crear una matriz de confusión simple:
table(prueba[,2:7],round(prediccion$net.result,2))

summary(red)
summary(prediccion)
