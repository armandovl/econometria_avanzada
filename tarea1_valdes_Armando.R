
################# Ejercicio 1 ###########################

# 1. Crear 3 vectores con 5 observaciones.
# 2. Deben ser  diferentes al ejercicio y con distinta lógica y 
# variables.
# 3. Crear un Data frame.
# 4. AÃ±adir una nueva variable con otro tipo de datos.
# 5. Crear una nueva variable a partir de una variable ya existente.
# 6. Visualizar el Data frame.
# 7. Abrir el Data frame en una nueva ventana.
# 8. Hacer un resumen de los datos contenidos en el Data frame.
# 9. Comentar.

# 1. Crear 3 vectores con 5 observaciones.
nombre <- c("Akira","Ghost","Dimo","Tigre","Honney")
raza <- c("Husky","Husky","Schnauzer","Callejero", "Callejero")
edad <- c(1,1,3,3,NA)

# 3. Crear un Data frame
mascotas <- data.frame(nombre,raza,edad)

# 4. AÃ±adir una nueva variable con otro tipo de datos.
mascotas$sexo <- c("HEMBRA","HEMBRA","MACHO","MACHO","HEMBRA")
mascotas$id <- (1:5)
mascotas$mio <- c(TRUE,FALSE,FALSE,FALSE,FALSE)

# 5. Crear una nueva variable a partir de una variable ya existente.
mascotas$edadHumano <- mascotas$edad*7

#ver como va quedando el df
head(mascotas)


#reacomodar las columnas
mascotas <- mascotas[,c(5,1,2,3,4,7,6)]#todas las filas
#ver como va quedando el df
head(mascotas)

#transformar a factores
mascotas$sexo <- as.factor(mascotas$sexo)
mascotas$raza <- as.factor(mascotas$raza)

# 6. Visualizar el Data frame.
print(mascotas)

# 7. Abrir el Data frame en una nueva ventana.
View(mascotas)

# 8. Hacer un resumen de los datos contenidos en el Data frame.

dim(mascotas) #filas y columnas

names(mascotas) #nombres

str(mascotas) #estructura

summary(mascotas) #resumen variables

summary(mascotas[,-1:-2]) #resumen solo variables significativas


############################ Ejercicio 2###########################################

# 1. Usar el Data frame cargado n=141.
# 2. Calcular frecuencia del género con table().
# 3. Calcular el promedio de la variable edad
# 4. Calcular el máximo y mínimo de la variable INAI
# 5. Calcular la mediana de la variable Escolaridad
# 6. Calcular la varianza de la variable Edad
# 7. Calcular el CV de la variable superficie_ha
# 8. Calcular el CV de la variable INAI
# 9. Comentar

# ctrl+shift+c comentario múltiple

# 1. Usar el Data frame cargado n=141.
# setwd("C:/Users/HP/Desktop/Econometria_Cuso") #ctrl+shift+h
# datos <- read.csv("BaseCacao.csv", stringsAsFactors = TRUE)


urldata <- "https://raw.githubusercontent.com/armandovl/econometria_avanzada/main/BaseCacao.csv"
datos <- read.csv(url(urldata),stringsAsFactors = T)

#inspeccionar datos
dim(datos) #ver filas y columnas
str(datos) #ver la estructura de los datos
head(datos,10) #ver primeros 10 registros
tail(datos,10) #ver últimos 10 registros


# 2. Calcular frecuencia del gÃ©nero con table().
freqAbsoluta <- table(datos$genero)
print(freqAbsoluta)


freqRelativa <- prop.table(freqAbsoluta) #frecuencie relativa
freqRelativa <- freqRelativa*100 #ponerla en porcentaje
freqRelativa <- round(freqRelativa,2) # solo con dos decimales
print(freqRelativa)

# 3. Calcular el promedio de la variable Edad
mediaEdad <- mean(datos$edad)
cat(c("El Promedio de Edad es:",round(mediaEdad,1)))

# 4. Calcular el máximo y minimo de la variable inai
minInai <- min(datos$inai_general)
maxInai <- max(datos$inai_general)

cat(c("El máximo de INAI es:", minInai,"\n", # con alt + 92 sale la diagonal invertida
      "El minimo de INAI es:", round(maxInai,1)))

# 5. Calcular la mediana de la variable escolaridad
medianaEscolaridad <- median(datos$escolaridad)
print(medianaEscolaridad)

# 6. Calcular la varianza de la variable edad
varianzaEdad <- var(datos$edad)
cat(c("La varianza de edad es:",round(varianzaEdad,2)))

# 7. Calcular el CV de la variable superficie_ha
coefSuperficie <- sd(datos$superficie_ha)/mean(datos$superficie_ha)
round(coefSuperficie,2)

# 8. Calcular el CV de la variable inai

coefInai <- sd(datos$inai_general)/mean(datos$inai_general)
round(coefInai,2)


#################### Ejercicio extra analizar rápidamente las variables ##################

#editor de temas en R
#https://tmtheme-editor.herokuapp.com/#!/editor/theme/1337

head(datos,10) #ver primeros 10 registros

#id y cadena son columnas irrelevantes y hay que eliminarlas

datos$id <- NULL
datos$cadena <- NULL

head(datos,10) #ver primeros 10 registros

# tenemos 3 variables cualitativas: estado    genero impacto_economico 
# y 5 variables numéricas


######## Empezemos con las variables categóricas ##################

# frecuencias absolutas
table(datos[,1])
table(datos[,2])
table(datos[,7])

#en grÃ¡fico ctrl+ alt + cursor seleccion multipleplot()

barplot(table(datos[,1]))
barplot(table(datos[,2]))
barplot(table(datos[,7]))

#ponerlos juntos

par(mfrow=c(1,3))
barplot(table(datos[,1]))
barplot(table(datos[,2]))
barplot(table(datos[,7]))
par(mfrow=c(1,1))


#combinacion de frecuencias
table(datos[,c(1:2,7)])

#vamos a reacomodar las variables
head(datos,10) #ver primeros 10 registros

datos <- datos[,c(1,2,7,3,4,5,6,8)]

head(datos,10) # nuevo reacomodo

#vamos a ver rápidamente como se distribuyen las variables cuantitativas


summary(datos[,4:8]) #estadísticos

par(mfrow=c(2,2))

for (i in 4:7){
  boxplot(datos[,i], main=names(datos)[i])

}

par(mfrow=c(1,1))


#con la media

par(mfrow=c(2,2))

for (i in 4:7){
  variableEnTurno <- datos[,i]
  media <- mean(variableEnTurno)
  media <- round(media,2)
  
  boxplot(datos[,i], main=names(datos)[i])

  points(1,media,col = "red",pch=19,cex=2)
  
  text(1.1,media + 2, labels = media, col = "red") #x=, y= labels=
  
}

par(mfrow=c(1,1))


###############Grafico con todo ###################

par(mfrow=c(2,2))

for (i in 4:7){
  variableEnTurno <- datos[,i]
  media <- mean(variableEnTurno)
  media <- round(media,1)
  
  mediana <- median(variableEnTurno)
  
  cua1<- quantile(variableEnTurno,.25)
  
  cua3<- quantile(variableEnTurno,.75)


  
  
  boxplot(datos[,i], main=names(datos)[i])
  
  points(1,media,col = "red",pch=19,cex=2)
  
  text(0.75,media, labels = media, col = "red") #x=, y= labels=
  
  text(1.22,cua1, labels =round(cua1,2), col = "blue") #x=, y= labels=
  text(1.22,cua3, labels =round(cua3,2), col = "blue") #x=, y= labels=
  text(1.22,mediana, labels =round(mediana,2), col = "blue") #x=, y= labels=
}

par(mfrow=c(1,1))

###############Grafico con todo colores aleatorios ###################

par(mfrow=c(2,2))

for (i in 4:7){
  variableEnTurno <- datos[,i]
  media <- mean(variableEnTurno)
  media <- round(media,1)
  
  mediana <- median(variableEnTurno)
  
  cua1<- quantile(variableEnTurno,.25)
  
  cua3<- quantile(variableEnTurno,.75)
  
  colores<-c("#00AFBB","#E7B800","#FC4E07","purple")
  
  
  
  boxplot(datos[,i], main=names(datos)[i],border = colores[i-3],col="white")
  
  points(1,media,col = "red",pch=19,cex=2)
  
  text(0.75,media, labels = media, col = "red") #x=, y= labels=
  
  text(1.22,cua1, labels =round(cua1,2), col = "blue") #x=, y= labels=
  text(1.22,cua3, labels =round(cua3,2), col = "blue") #x=, y= labels=
  text(1.22,mediana, labels =round(mediana,2), col = "blue") #x=, y= labels=
  
  
  stripchart(datos[,i], method="stack", # overplot, stack, jitter
             vertical=TRUE,
             pch=20,
             add=TRUE) #unir los dos graficos
  
}

par(mfrow=c(1,1))

#creo que escolaridad debe ser variable categÃ³rica

unique(datos$escolaridad)

table(datos$escolaridad)




#graficar edad por genero

library(ggstatsplot)

ggbetweenstats(   #seleccionar la funcion + f1 nos da la ayuda
  data  = datos,
  x     = genero,
  y     = edad,
  title = "Distribution of sepal length across Iris species",
  outlier.tagging = TRUE,
)


