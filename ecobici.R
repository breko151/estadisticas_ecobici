main <- function() {
  #Importamos los datos.
  datos <- obtencion_datos()
  #Limpieza datos.
  datos <- limpieza(datos)
  #Obtencion de datos derivados dentro de la limpieza.
  datos <- calculoTiempo(datos)
  #Tabulamos datos los datos obtenidos.
  View(datos)
  #Informacion de la estructura de los datos.
  info_datos(datos)
  #Graficamos datos individuales.
  histogramas(datos)
  
}

obtencion_datos <- function() {
  #Configuramos la ruta donde se encuentra el archivo csv.
  setwd("C:/Users/breko/Documents/Practicas/Programacion_Ciencia_Datos/proyecto2/estadisticas")
  #Generamos la ruta del archivo.
  ruta <- "./new_datos.csv"
  #Leemos el archivo con terminacion csv.
  datos_sp <- read.csv(file = ruta, header = T, sep = ",")
  #Limpiamos los datos, las columnas eliminadas no contienen datos.
  datos <- datos_sp[,(2:11)]
  return(datos)
}

limpieza <- function(datos) {
  #Cambios el formato de los campos a tipo factor.
  datos$Genero_Usuario <- as.factor(datos$Genero_Usuario)
  datos$Fecha_Retiro <- as.Date.factor(datos$Fecha_Retiro, "%d/%m/%Y")
  datos$Hora_Retiro <- as.factor(datos$Hora_Retiro)
  datos$Fecha_Arribo <- as.Date.factor(datos$Fecha_Arribo, "%d/%m/%Y")
  datos$Hora_Arribo <- as.factor(datos$Hora_Arribo)
  return(datos)
}

calculoTiempo<- function (datos){ 
  #Activamos la libreria lubridate.
  library(lubridate)
  #Cambiamos el formato de las horas.
  datos_ayuda <- datos
  datos_ayuda[,"Hora_Retiro"] <- hms(datos[,"Hora_Retiro"])
  datos_ayuda[,"Hora_Arribo"] <- hms(datos[,"Hora_Arribo"])
  #Calculamos el tiempo total de los viajes. 
  datos$Tiempo_Total <- datos_ayuda$Hora_Arribo - datos_ayuda$Hora_Retiro
  #Cambios el formato del tiempo total.
  datos$Tiempo_Total <- as.duration(datos$Tiempo_Total)
  datos$Tiempo_Total <- as.numeric(datos$Tiempo_Total, "minutes")
  datos$Tiempo_Total <- ceiling(datos$Tiempo_Total) 
  datos$Tiempo_Total <- as.integer(datos$Tiempo_Total)
  datos$Tiempo_Total <- abs(datos$Tiempo_Total)
  return(datos)
}

info_datos <- function(info) {
  print("Resumen Datos")
  str(info)
}

histogramas <- function(datos) {
  #Histograma Edad.
  hist(datos$Edad_Usuario, 
       main = "Histograma Edad Usuario",
       ylab = "Frecuencia",
       xlab = "Edad")
  #Histograma Genero.
  gen <- barplot(table(datos$Genero_Usuario), 
          main = "Histograma del Genero de los usuarios", 
          xlab = "Genero", 
          ylab = "Frecuencia",
          col = rainbow(2))
  text(gen, y = 385000, labels = table(datos$Genero_Usuario))
  #Histograma Bici.
  barplot(table(datos$Bici),
          main = "Histograma de Numero Bicicleta",
          xlab = "Numero de Bicicleta",
          ylab = "Frecuencia")
  #Histograma Fecha Retiro.
  barplot(table(datos$Fecha_Retiro), 
          main = "Histograma de la Fecha de Retiro", 
          xlab = "Fecha de Retiro", 
          ylab = "Frecuencia",
          col = rainbow(32))
  #Histograma Fecha Arribo.
  barplot(table(datos$Fecha_Arribo), 
                 main = "Histograma de la Fecha de Arribo", 
                 xlab = "Fecha de Arribo", 
                 ylab = "Frecuencia",
                 col = rainbow(32))
  #Histograma Estacion Retiro.
  barplot(table(datos$Ciclo_Estacion_Retiro), 
          main = "Histograma de la Estacion Retiro", 
          xlab = "Estacion de Arribo", 
          ylab = "Frecuencia",
          col = rainbow(32))
  #Histograma Estacion Arribo.
  barplot(table(datos$Ciclo_Estacion_Arribo), 
          main = "Histograma de la Estacion de Arribo", 
          xlab = "Estacion de Arribo", 
          ylab = "Frecuencia",
          col = rainbow(32))
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
  #Histograma Tiempo Total
  barplot(table(datos$Tiempo_Total), 
       main = "Histograma Tiempo Viaje",
       ylab = "Frecuencia",
       xlab = "Minutos",
       xlim = 1000,
       col = rainbow(32),
       args.legend = list(x = "top"))
}




main()