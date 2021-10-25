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
  #Obtencion datos estadisticos.
    #Descripcion datos categoricos.
  descripcion(datos)
    #Medidas de tendencia central.
  tendencia_central(datos)
    #Medidas de posicion.
  posicion(datos)
    #Medidas de dispersion.
  dispersion(datos)
  #Grafica Dispersio, pruebas de normalidad, covarianza y correlacion.
  normalidad(datos)
  #Obtencion Modelo.
  modelo(datos)
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
       col = rainbow(32),
       args.legend = list(x = "top"))
}

descripcion <- function(datos) {
  print("Descripcion Datos Categoricos")
  #Descripcion Genero Usuario.
  cat("\tGenero Usuario\n")
  genero <- datos$Genero_Usuario
  numHombres <- sum(genero == "M")
  numMujeres <- sum(genero == "F")
  cat("\t\tNumero de hombres: ", numHombres, "\n")
  cat("\t\tNumero de mujeres: ", numMujeres, "\n")
  #Descripcion Hora Retiro.
  cat("\tHora Retiro\n")
  Ho_R <- datos$Hora_Retiro
  print(summary(Ho_R))
  #Descripcion Hora Arribo.
  cat("\tHora Arribo\n")
  Ho_A <- datos$Hora_Arribo
  print(summary(Ho_A))
  #Descripcion Fecha Retiro.
  cat("\tFecha Retiro\n")
  Fe_R <- datos$Fecha_Retiro
  print(summary(Fe_R))
  #Descripcion Fecha Arribo.
  cat("\tFecha Arribo\n")
  Fe_A <- datos$Fecha_Arribo
  print(summary(Fe_A))
  #Descripcion Numero Bicicleta.
  cat("\tBicicleta\n")
  print(summary(datos$Bici))
  #Descripcion Estacion Retiro.
  cat("\tEstacion Retito\n")
  print(summary(datos$Ciclo_Estacion_Retiro))
  #Descripcion Estacion Arribo.
  cat("\tEstacion Arribo\n")
  print(summary(datos$Ciclo_Estacion_Arribo))
}

tendencia_central <- function(datos) {
  #Importamos la libreria modeest.
  library(modeest)
  print("Medidas de Tendencia Central")
  #Medidas de Tendencia Central de Edad Usuario.
  cat("\tEdad del Usuario\n")
  cat("\t\tLa Media es:", mean(datos$Edad_Usuario, na.rm = TRUE), "\n")
  cat("\t\tLa Mediana es:", median(datos$Edad_Usuario, na.rm = TRUE), "\n")
  cat("\t\tLa Moda es:", mfv(datos$Edad_Usuario), "\n")
  #Medidas de Tendencia Central del Tiempo Total.
  cat("\tTiempo Total\n")
  cat("\t\tLa Media es:", mean(datos$Tiempo_Total, na.rm = TRUE), "\n")
  cat("\t\tLa Mediana es:", median(datos$Tiempo_Total, na.rm = TRUE), "\n")
  cat("\t\tLa Moda es:", mfv(datos$Tiempo_Total), "\n")
}

posicion <- function(datos) {
  #Importamos la libreria modeest.
  library(modeest)
  print("Medidas de Posicion")
  #Medidas de Tendencia Central de Edad Usuario.
  cat("\tEdad del Usuario\n")
  cat("\t\tLos Cuartiles son:\n")
  print(quantile(datos$Edad_Usuario, prob=c(0,0.25,0.5,0.75,1)))
  cat("\t\tLos Deciles son:\n")
  print(quantile(datos$Edad_Usuario, prob=seq(0, 1, length = 11)))
  cat("\t\tLos Centiles son\n:")
  print(quantile(datos$Edad_Usuario, prob=seq(0, 1, length = 101)))
  #Medidas de Tendencia Central del Tiempo Total.
  cat("\tTiempo Total\n")
  cat("\t\tLos Cuartiles son:\n")
  print(quantile(datos$Tiempo_Total, prob=c(0,0.25,0.5,0.75,1)))
  cat("\t\tLos Deciles son:\n")
  print(quantile(datos$Tiempo_Total, prob=seq(0, 1, length = 11)))
  cat("\t\tLos Centiles son:\n")
  print(quantile(datos$Tiempo_Total, prob=seq(0, 1, length = 101)))
}

dispersion <- function(datos) {
  #Importamos la libreria modeest.
  library(modeest)
  print("Medidas de Dispersion")
  #Medidas de Dispersion de Edad Usuario.
  cat("\tEdad del Usuario\n")
  cat("\t\tEl Rango es:", max(datos$Edad_Usuario, na.rm = TRUE) - min(datos$Edad_Usuario, na.rm = TRUE), "\n")
  cat("\t\tLa Varianza es:", var(datos$Edad_Usuario, na.rm = TRUE), "\n")
  cat("\t\tLa Desviacion Estandar es:", sd(datos$Edad_Usuario, na.rm = TRUE), "\n")
  cat("\t\tEl Coeficiente es:", sd(datos$Edad_Usuario) / mean(datos$Edad_Usuario) * 100, "\n")
  #Medidas de TDispersion del Tiempo Total.
  cat("\tTiempo Total\n")
  cat("\t\tEl Rango es:", max(datos$Tiempo_Total, na.rm = TRUE) - min(datos$Tiempo_Total, na.rm = TRUE), "\n")
  cat("\t\tLa Varianza es:", var(datos$Tiempo_Total, na.rm = TRUE), "\n")
  cat("\t\tLa Desviacion Estandar es:", sd(datos$Tiempo_Total, na.rm = TRUE), "\n")
  cat("\t\tEl Coeficiente es:", sd(datos$Tiempo_Total) / mean(datos$Tiempo_Total) * 100, "\n")
}

normalidad<- function (datos){
  #Importacion de librerias.
  library("nortest")
  library("ggplot2")
  library("ggcorrplot")
  #Normalidad de los datos.
  print("Normalidad de los datos")
  cat("\tTiempo Total\n")
  print(lillie.test(datos$Tiempo_Total))  #No es distribucion normal.
  cat("\tEdad Usuario\n")
  print(lillie.test(datos$Edad_Usuario)) #No es distribucion normal.
  #Grafica de Dispersion.
  plot(datos$Tiempo_Total~datos$Edad_Usuario, 
       main = "Relacion Edad y Tiempo de Uso",
       xlab = "Edad Usuario", ylab = "Tiempo de Uso",
       xlim = c(15,80), ylim = c(1,650), xasx = "i", yasx = "i", col = "red", pch = 3)
  #Normalidad de los datos.
  qqnorm(datos$Edad_Usuario, col = "steelblue", lwd = 2)  
  qqline(datos$Edad_Usuario, col = "steelblue", lwd = 2)  
  qqnorm(datos$Tiempo_Total, col = "steelblue", lwd = 2)  
  qqline(datos$Tiempo_Total, col = "steelblue", lwd = 2)  
  #Importamos libreria.
  library(psych)
  data <- data.frame(datos$Edad_Usuario, datos$Tiempo_Total)
  pairs.panels(data, method = "spearman")
  #Coeficiente de Correlacion.
  print("Coeficiente de Correlacion")
  coeR <- cor(data, method = "spearman") 
  print(coeR)
  #Coeficiente de Covarianza.
  print("Coeficiente de Covarianza")
  covarianza <- cov(data)
  print(covarianza) 
  #Si covarianza es positiva hay relacion directa entre las variables.
  #Si covarianza es negativa hay relacion inversa entre las variables.
  #Si covarianza es cero no hay relacion  entre las variables.
  
  #+- 0.00 +-0.09 Correlacion nula.
  #+- 0.10 +-0.19 Correlacion muy debil.
  #+- 0.20 +-0.49 Correlacion debil.
  #+- 0.50 +-0.69 Correlacion moderada.
  #+- 0.70 +-0.84 Correlacion significativa.
  #+- 0.85 +-0.95 Correlacion fuerte.
  #+- 0.96 +-1.0 Correlacion perfecta.
}

modelo <- function(datos) {4
  modelo <- lm(datos$Tiempo_Total~datos$Edad_Usuario)
  print("Modelo de Regresion Lineal")
  print(summary(modelo))
  #Generamos la grafica.
  grafica = ggplot(data = datos, aes(x = Edad_Usuario, y = Tiempo_Total))
  #Grafica con el modelo.
  plot(grafica + geom_point() + geom_smooth(method = "lm", color = "green"))
}

main()
