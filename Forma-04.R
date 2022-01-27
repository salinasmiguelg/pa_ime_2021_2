# Librerías

if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}
if (!require(bootES) ) {
  install.packages("bootES", dependencies = TRUE )
  require (bootES)
}
if (!require(tidyverse) ) {
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}



#------------------------------------------------------------------------------
datosLeidos <- read.csv2(file.choose(),head=TRUE ,sep=";", stringsAsFactors = TRUE  )
datos <- datosLeidos


# Pregunta 1
#(21 puntos) Harry cree que, en promedio, los estudiantes de Slytherin obtuvieron 10 puntos más durante el primer
#trimestre que durante el segundo. Aunque Hermione no está de acuerdo con que su amigo participe de apuestas, ha
#decidido corroborar este hecho usando bootstrapping con 5.000 repeticiones con el valor 531 como semilla y nivel de
#significación ??=0,05.

#Función para graficar las muestras.
graficar_distribucion <- function(distribucion, ...) {
  
  observaciones <- data.frame(distribucion)
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Media",
                            ylab = "Frecuencia", ...)
  qq <- ggqqplot(observaciones , x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión
  figura <- ggarrange(histograma, qq ,ncol = 2 , nrow = 1)
  print(figura)
}

#-------------------------------------------------------------------------------
#Hipótesis:

#H0: la diferencia en el promedio de los estudiantes de Slytherin durante el 
#    primer trimestres y el segundo trimestre es de 10 puntos

#HA: la diferencia en el promedio de los estudiantes de Slytherin durante el 
#    primer trimestres y el segundo trimestre es de mas de 10 puntos

#Sea uA: el promedio durante el primer trimestre.
#Sea uB: el promedio durante el segundo trimestre.

#De esta manera, las hipótesis en su forma matemática son:

#H0: uA-uB = 10
#HA: uA-uB > 10

#Lectura de los Datos:


datosSL <- datos %>% filter ( casa == "Slytherin")
set.seed (531)


# Ingresar datos originales .
instancias <- 1:10
trimestre1 <- datosSL$trim1
trimestre2 <- datosSL$trim2


#Prueba de normalidad para las muestras


graficar_distribucion(trimestre1)
graficar_distribucion(trimestre2)

# Al analizar los histogramas y el comportamiento de las muestras, podemos
# comprobar que estás no siguen una distribución normal, pues se presentan
# datos atípicos y se encuentran ligeramente desplazadas.

#-------------------------------------------------------------------------------
#Justificación:
# Como los datos no siguen una distribución normal y además, la cantidad de
# muestras es pequeña, se utilizará el procedimiento de Bootstrapping
# para muestras pareadas empleando como parámetro de interés la media.

# Establecer nivel de significación.
alfa <- 0.05

# Calcular la diferencia entre ambas observaciones .
diferencia <- trimestre2 - trimestre1

# Calcular la media observada de las diferencias .
valor_observado <- mean ( diferencia )

# Generar la distribución bootstrap.
B <- 5000
valor_nulo <- 10

distribucion_bootstrapES <- bootES ( diferencia , R = B , ci.type = "bca", ci.conf = 1 - alfa , plot = FALSE )
distribucion_nula <- distribucion_bootstrapES[["t"]] - valor_nulo

# Determinar el valor p.
p <- ( sum(  distribucion_nula  > valor_observado  ) + 1) / ( B + 1)
cat (" Valor p:", p )


# Conclusión:
# Luego de realizar la prueba, se obtiene un valor de p: 0.03839232, este es
# inferior al nivel de significancia (a = 0.05) por lo que se rechaza H0 en favor
# de HA, por lo que existe mucha evidencia a favor de HA.
# con esto, se puede afirmar con un 95% de confianza que en promedio, los 
# estudiantes de Slytherin obtuvieron 10 puntos más durante el primer que durante
# el segundo.



# ------------------------------------------------------------------
# Pregunta 2


# Importación de datos
# Se le debe ingresar el archivo "Datos-PA"
datos_p2 <- read.csv2(file.choose(),head=TRUE ,sep=";", stringsAsFactors = TRUE  )

# Pregunta 2: Harry también cree que, en promedio, los estudiantes de Gryffindor obtuvieron 1 
#puntos más que los de Ravenclaw durante el segundo trimestre. Ron, por su parte, está seguro de que la 
#ventaja lograda por los primeros fue mayor y está a punto de iniciar una pelea con Harry por esta razón.
#Para evitar el conflicto entre sus amigos, Hermione ha decidido verificar quién tiene la razón mediante una 
#simulación de MonteCarlo con 2.000 repeticiones, usando el valor 847 como semilla y nivel de significación alfa=0,05.

# Hipótesis a Contrastar:
# H0: El promedio de los puntos de los estudiantes es el mismo para las casas de
# Griffindor y Ravenclaw (ua - ub = 0)

# HA: El promedio de los puntos de los estudiantes es distinto para las casas de
# Griffindor y Ravenclaw (ua - ub != 0)



# Definición de Funciones

# Función para calcular la diferencia de medias.
# Argumentos :
# - muestra_1 , muestra_2: vectores numÃ©ricos con las muestras a comparar .
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
# - muestra_1 , muestra_2: vectores numÃ©ricos con las muestras a comparar .
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
                            xlab = "EstadÃ­stico de interÃ©s",
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
# - alternative : tipo de hipótesis alternativa . "two.sided" para
#  hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# - plot : si es TRUE , construye el gráfico de la distribución generada .
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1 , muestra_2,
                                               repeticiones, FUN ,
                                               alternative, plot , ...){
  cat("Prueba de permutaciones\n\n")
  cat("HipÃ³tesis alternativa :", alternative , "\n")
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
n <- 10
gryffindor <- datos_p2 %>% filter(casa == "Gryffindor", trim2 != "NA")
trim2_gryffindor <- sample(gryffindor$trim2, n)
a <- as.numeric(trim2_gryffindor)

ravenclaw <- datos_p2 %>% filter(casa == "Ravenclaw", trim2 != "NA")
trim2_ravenclaw <- sample(ravenclaw$trim2, n)
b <- as.numeric(trim2_ravenclaw)

print(shapiro.test(a))
print(shapiro.test(b))
# Al no cumplirse la normalidad, es posible emplear la prueba de permutaciones

R = 2000
contrastar_hipotesis_permutaciones(a, b, repeticiones = R, 
                                   FUN = mean, 
                                   alternative = "two.sided", 
                                   plot = TRUE,
                                   color = "blue", fill = "blue")

# Se muestra además el histograma y gráfico Q-Q de la distribución para la 
# diferencia de medias generada mediante permutaciones.


# Conclusión:
# Con respecto a la prueba realizada y utilizando para ello una simulaciÃ³n de
# Monte Carlo, el resultado del valor p obtenido de 0.00149925, inferior a un
# nivel de significaciÃ³n de 0.05, por lo que se rechaza la hipótesis nula a favor 
# de la hipótesis alternativa, de esta manera se concluye con 95% de confianza que 
# el promedio de los puntos de los estudiantes es distinto para las casas de
# Gryffindor y Ravenclaw.




#---------------------------------------------------------------------------------------

# Pregunta 3

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





