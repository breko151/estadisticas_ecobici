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
  
  ####cambios Yael
  #Calculo de tiempo de uso 
  datos<-calculoTiempo(datos)
  View(datos)
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

calculoTiempo<- function (datos){ ######Nuevo
  #install.package(lubridate)
  library(lubridate)
  datos[,"Hora_Retiro"]<-hms(datos[,"Hora_Retiro"])
  datos[,"Hora_Arribo"]<-hms(datos[,"Hora_Arribo"])
  datos$tiempoTotal<-datos$Hora_Arribo - datos$Hora_Retiro
  datos
  }

estudioGenero <- function(datos) {
  genero <- datos[ ,"Genero_Usuario"] 
  numHombres <- sum(genero == "M")  #Numero de Hombres
  numMujeres <- sum(genero == "F")  #Numero de Mujeres
}

estudioEdad<-function(datos){    #Conocer a que sector de personas es mas usado el servicio
 #Media aritmetica
 mediaEdad<-mean(datos[,"Edad_Usuario"]) #Promedio de edad que usa ecobici
 #Varianza
 varEdad<-var(datos[,"Edad_Usuario"])    #variabilidad de la edad respecto a la media
 #Desviacion estandar
 desEstanEdad<-sqrt(varEdad)             #Que tan separados estan los datos respecto a la media
 edadOrdenado <- sort (datos[,"Edad_Usuario"])
 #Cuartiles
 cuartilesEdad<-quantile(edadOrdenado)
 #Moda La edad que usa mas ecobicis
 modaEdad<-mfv(edadOrdenado)#Moda
}

estudioEdadGenero<-function(datos){
#Calculo de la covarianza
  dataEdadGenero<-data.frame(datos[ ,c("Genero_Usuario","Edad_Usuario")])
#Covarianza Grado de dispersión de la edad y fgenerp con respecto a la media  
  covarianza <- cov(datos$Genero_Usuario,datos$Edad_Usuario)
  matCovarianza <- cov(dataEdadGenero)
#coeficiente de correlación
  coeR <- cor(DataEdadGenero,method="pearson","spearman","kendall" )
#Correlacion 2
  correlacion<-cor(dataEdadGenero,method="pearson")
  library (psych)
  pairs.panel(dataEdadGenero,method="pearson") 
  ggcorrplot(correlacion ,method = "circle")
  coefCor <- corr.test(datos$Genero_Usuario,datos$Edad_Usuario,method="pearson",adjust=none)

}

main()
