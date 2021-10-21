main <- function() {
  #Importamos los datos.
  datos <- obtencion_datos()
  #Visualizamos los datos obtenidos.
  #View(datos)
  #Informacion de la estructura de los datos.
  info_datos(datos)
}

obtencion_datos <- function() {
  #Introducimos la ruta del archivo.
  setwd("C:/Users/breko/Documents/Practicas/Programacion_Ciencia_Datos/proyecto2")
  ruta <- "./estadisticas/all_data.csv"
  datos_sp <- read.csv(file = ruta, header = T, sep = ",")
  #Limpiamos los datos.
  #Las columnas eliminadas, no contienen datos.
  datos <- datos_sp[,(1:10)]
  return(datos)
}

info_datos <- function(datos) {
  str(datos)
}

main()