# Librerías

if (!require(readxl) ) {
  install.packages("readxl", dependencies = TRUE )
  require (readxl)
}

if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}


data <- read.csv2(choose.files(), sep=";")

alfa <- 0.05








# P2


# Importación de datos
# Se le debe ingresar el archivo "Datos-PA"
datos_casen <- read_xls(file.choose())

# Pregunta 2: Harry tambi???n cree que, en promedio, los estudiantes de Gryffindor obtuvieron 1 
#puntos m???s que los de Ravenclaw durante el segundo trimestre. Ron, por su parte, est??? seguro de que la 
#ventaja lograda por los primeros fue mayor y est??? a punto de iniciar una pelea con Harry por esta raz???n.
#Para evitar el conflicto entre sus amigos, Hermione ha decidido verificar qui???n tiene la raz???n mediante una 
#simulaci???n de MonteCarlo con 2.000 repeticiones, usando el valor 847 como semilla y nivel de significaci???n ??=0,05.

# Hipótesis a Contrastar:
# H0: El promedio de los puntos de los estudiantes es el mismo para las casas de
# Griffindor y Ravenclaw (ua - ub = 0)

# HA: El promedio de los puntos de los estudiantes es distinto para las casas de
# Griffindor y Ravenclaw (ua - ub != 0)



# Definición de Funciones

# Función para calcular la diferencia de medias.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - FUN: función del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E_2.
calcular_diferencia <- function(muestra_1, muestra_2, FUN) {
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return (diferencia)
}

# Función para hacer una permutación y calcular el estadístico
# de interés.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - FUN: función del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E _2.
permutar <- function(muestra_1 , muestra_2, FUN) {
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutación.
  permutacion <- sample (c( muestra_1 , muestra_2) , size = n_1 + n_2, replace = FALSE)
  
  # Asignar elementos a los dos grupos .
  
  permutacion_1 <- permutacion [1:n_1]
  permutacion_2 <- permutacion [n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias .
  return (calcular_diferencia(permutacion_1, permutacion_2 , FUN))
}

# Función para calcular el valor p.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - valor_observado : valor del estadístico de interés para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# Valor :
# - el valor p calculado .
calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  else{
    numerador <- sum( distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  return(valor_p)
}

# Función para graficar una distribución.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function(distribucion, ...) {
  
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", ...)
  
  qq <- ggqqplot(observaciones , x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura <- ggarrange(histograma, qq ,ncol = 2 , nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones .
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : función del estadístico E para el que se calcula la diferencia .
# - alternative : tipo de hipó tesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# - plot : si es TRUE , construye el gráfico de la distribución generada .
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1 , muestra_2,
                                               repeticiones, FUN ,
                                               alternative, plot , ...){
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa :", alternative , "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2 , FUN)
  cat("Valor observado :", observado , "\n")
  distribucion <- rep(NA, repeticiones)
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  if(plot){
    graficar_distribucion(distribucion, ...)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")
  cat("Valor p:", valor_p , "\n\n")
}


set.seed(847)






# P3

# H0 = No existe preferencia al retorno de una actividad en específico.
# HA = Hay una preferencia en una actividad para el retorno a presencialidad.

# Contexto: La USACH visto el inminente retorno a la presencialidad desea saber
# la opinión de su estudiantado respecto al retorno.
# En concreto si están dispuestos a volver totalmente presencial o si se debe
# hacer un retorno gradual (donde tengan prioridad las actividades con mayor 
# preferencias, en caso de existir).

# Dado esto es que la universidad elige un grupo de 5000 personas al azar,
# siguiendo una distribución normal respecto al año que cursan. Y se les emplea
# una encuesta con escala Leininger preguntando acerca de cada una de las 
# actividades que ofrece la universidad como: asistir a clases, asistir a 
# ayudantías, ser ayudante, participar en actividades extra académicas, etc.
# donde cada estudiante indica su prioridad.

# 
# 
# 




