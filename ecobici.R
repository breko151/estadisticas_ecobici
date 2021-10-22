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
  datos <- datos_sp[,(2:10)]
  return(datos)
}

limpieza <- function(datos) {
  datos$Genero_Usuario <- as.factor(datos$Genero_Usuario)
  return(datos)
}

info_datos <- function(datos) {
  print("Resumen Datos")
  str(datos)
  print("Resumen Edad Usuarios")
  summary(datos$Edad_Usuario)
  print("Resumen Genero Usuarios")
  summary(datos$Genero_Usuario)
}

histogramas <- function(datos) {
  hist(datos$Edad_Usuario)
  barplot(table(datos$Genero_Usuario))
}


#Cambios Yael
estudioEdad<-function(datos){    #Conocer a que sector de personas es mas usado el servicio
 mediaEdad<-mean(datos[,"Edad_Usuario"]) #Promedio de edad que usa ecobici
 varEdad<-var(datos[,"Edad_Usuario"])    #variabilidad de la edad respecto a la media
 desEstanEdad<-sqrt(varEdad)             #Que tan separados estan los datos respecto a la media
 edadOrdenado <- sort (datos[,"Edad_Usuario"])
 #Deciles
  
 #cuartiles
 cuartilesEdad<-quantile(edadOrdenado)
 #Moda La edad que usa mas ecobicis
 modaEdad<-mfv(edadOrdenado)#Moda
}


estudioEdadGenero<-function(datos){
 #Calculo de la covarianza


}



main()
