main <- function() {
  #Importamos los datos.
  datos <- obtencion_datos()
  #Limpieza datos.
  datos <- limpieza(datos)
  #Tabulamos datos los datos obtenidos.
  View(datos)
  #Informacion de la estructura de los datos.
  info_datos(datos)
  #Graficamos datos individuales.
  histogramas(datos)
}

obtencion_datos <- function() {
  #Introducimos la ruta del archivo.
  setwd("C:/Users/breko/Documents/Practicas/Programacion_Ciencia_Datos/proyecto2")
  ruta <- "./estadisticas/new_datos.csv"
  datos_sp <- read.csv(file = ruta, header = T, sep = ",")
  #Limpiamos los datos.
  #Las columnas eliminadas, no contienen datos.
  datos <- datos_sp[,(2:11)]
  return(datos)
}

limpieza <- function(datos) {
  datos$Genero_Usuario <- as.factor(datos$Genero_Usuario)
  datos$Fecha_Retiro <- as.Date.factor(datos$Fecha_Retiro, "%d/%m/%Y")
  datos$Hora_Retiro <- as.factor(datos$Hora_Retiro)
  datos$Fecha_Arribo <- as.Date.factor(datos$Fecha_Arribo, "%d/%m/%Y")
  datos$Hora_Arribo <- as.factor(datos$Hora_Arribo)
  
  return(datos)
}

info_datos <- function(info) {
  print("Resumen Datos")
  str(info)
}

histogramas <- function(datos) {
  #Histograma Edad
  hist(datos$Edad_Usuario, 
       probability = T, 
       main = "Histograma Edad Usuario",
       ylab = "Frecuencia",
       xlab = "Edad")
  lines(density(datos$Edad_Usuario), lwd = 5, col = "red")
  #Histograma Genero
  gen <- barplot(table(datos$Genero_Usuario), 
          main = "Histograma del Genero de los usuarios", 
          xlab = "Genero", 
          ylab = "Frecuencia",
          col = rainbow(2))
  text(gen, y = 385000, labels = table(datos$Genero_Usuario))
  #Histograma Fecha Retiro
  barplot(table(datos$Fecha_Retiro), 
          main = "Histograma de la Fecha de Retiro", 
          xlab = "Fecha de Retiro", 
          ylab = "Frecuencia",
          col = rainbow(32))
  #Histograma Fecha Arribo
  barplot(table(datos$Fecha_Arribo), 
                 main = "Histograma de la Fecha de Arribo", 
                 xlab = "Fecha de Arribo", 
                 ylab = "Frecuencia",
                 col = rainbow(32))
  #Histograma Estacion Retiro
  barplot(table(datos$Ciclo_Estacion_Retiro), 
          main = "Histograma de la Estacion Retiro", 
          xlab = "Estacion de Arribo", 
          ylab = "Frecuencia",
          col = rainbow(32))
  #Histograma Estacion Arribo
  barplot(table(datos$Ciclo_Estacion_Arribo), 
          main = "Histograma de la Estacion de Arribo", 
          xlab = "Estacion de Arribo", 
          ylab = "Frecuencia",
          col = rainbow(32))
  #Histograma Bici
  barplot(table(datos$Bici),
          main = "Histograma de Numero Bicicleta",
          xlab = "Numero de Bicicleta",
          ylab = "Frecuencia")
  #Histograma Hora Retiro
  barplot(table(datos$Hora_Retiro), 
          main = "Hora de la Hora de Retiro", 
          xlab = "Hora de Retiro", 
          ylab = "Frecuencia",
          col = rainbow(32))
  #Histograma Hora Arribo
  barplot(table(datos$Hora_Arribo), 
          main = "Hora de la Hora de Arribo", 
          xlab = "Hora de Arribo", 
          ylab = "Frecuencia",
          col = rainbow(32))

}


#Cambios Yael
#estudioEdad<-function(datos){    #Conocer a que sector de personas es mas usado el servicio
 #mediaEdad<-mean(datos[,"Edad_Usuario"]) #Promedio de edad que usa ecobici
 #varEdad<-var(datos[,"Edad_Usuario"])    #variabilidad de la edad respecto a la media
 #desEstanEdad<-sqrt(varEdad)             #Que tan separados estan los datos respecto a la media
 #edadOrdenado <- sort (datos[,"Edad_Usuario"])
 #Deciles
  
 #cuartiles
 #cuartilesEdad<-quantile(edadOrdenado)
 #Moda La edad que usa mas ecobicis
 #modaEdad<-mfv(edadOrdenado)#Moda
#}


estudioEdadGenero<-function(datos){
 #Calculo de la covarianza


}



main()
