# Función para leer los números desde el archivo
leer_numeros <- function(nombre_archivo) {
  # Verificar si el archivo existe
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe. Deteniendo la ejecución.")
  }
  
  # Leer el archivo y convertir los datos en un vector de números
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

# Función para calcular los estadísticos y verificar la desviación estándar
calcular_estadisticos <- function(numeros) {
  # Calcular la media, mediana y desviación estándar
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  
  # Verificar si la desviación estándar es mayor a 10
  if (desviacion_estandar > 10) {
    cat("Alta variabilidad: La desviación estándar es mayor que 10.\n")
  }
  
  # Retornar los resultados como una lista
  return(list(media = media, mediana = mediana, desviacion_estandar = desviacion_estandar))
}

# Función para calcular el cuadrado de cada número utilizando sapply
calcular_cuadrados <- function(numeros) {
  cuadrados <- sapply(numeros, function(x) x^2)
  return(cuadrados)
}

# Función para escribir los resultados en un archivo de salida
escribir_resultados <- function(estadisticos, cuadrados, nombre_archivo_salida) {
  # Crear el contenido del archivo
  resultado <- paste(
    "Estadísticos:\n",
    "Media: ", estadisticos$media, "\n",
    "Mediana: ", estadisticos$mediana, "\n",
    "Desviación Estándar: ", estadisticos$desviacion_estandar, "\n\n",
    "Cuadrados de los números:\n", 
    paste(cuadrados, collapse = ", "), "\n"
  )
  
  # Escribir en el archivo de salida
  writeLines(resultado, nombre_archivo_salida)
}

# Script principal
# Nombre del archivo de entrada y salida
archivo_entrada <- "numeros.txt"
archivo_salida <- "resultados.txt"

# Leer los números del archivo
numeros <- leer_numeros('numeros.txt')

# Calcular los estadísticos
estadisticos <- calcular_estadisticos(numeros)

# Calcular los cuadrados de los números
cuadrados <- calcular_cuadrados(numeros)

# Escribir los resultados en el archivo de salida
escribir_resultados(estadisticos, cuadrados, archivo_salida)

cat("Los resultados se han guardado en el archivo resultados.txt\n")
