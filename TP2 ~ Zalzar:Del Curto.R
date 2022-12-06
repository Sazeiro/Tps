# Funcion para simular una realizacion del experimento
simulate_competencia <- function(m1, m2, v) {
  # Defino los intervalos (a, b) y (c, d) basedo en los valores ingresados de m1 y m2
  a <- sqrt(8 * m1 + m2)
  b <- sqrt(10 * m1 + 10 * m2)
  c <- log(200 * m1 + 300 * m2)
  d <- log(400 * m1 + 600 * m2)
  
  # Genero valores aleatorios de la cantidad de baldes y el tiempo de demora
  # para cada estudiante
  carga_balde_1 <- runif(1, a, b)
  carga_balde_2 <- runif(1, a, b)
  tiempo_1 <- runif(1, c, d)
  tiempo_2 <- runif(1, c, d)
  
  # Calculo los valores de n1, n2, t1, t2, w, y l 
  n1 <- v / carga_balde_1
  n2 <- v / carga_balde_2
  t1 <- n1 * tiempo_1
  t2 <- n2 * tiempo_2
  w <- if (t1 < t2) t1 else t2
  l <- if (t1 > t2) t1 else t2
  
  return(c(n1, n2, t1, t2, w, l))
}

# Defino los valores de m1, m2 y de v
m1 <-12
m2 <- 12
v <- 190.9

# Creo un  data frame vacio para guardar los resultados de las simulaciones
resultados <- data.frame(n1 = numeric(0), n2 = numeric(0), t1 = numeric(0), t2 = numeric(0), w = numeric(0), l = numeric(0))

# Simulo la competencia 1000 veces y guardo los resultados en el data frame
for (i in 1:1000) {
  resultado <- simulate_competencia(m1, m2, v)
  resultados <- rbind(resultados, resultado)
}


##A

#Media y varianza de N1:

plot(resultados[[1]],xlab = 'Simulaciones', ylab = 'Probabilidad N1', ylim = c(10,20), main = 'Cantidad de baldes arrojados al recipiente por el estudiante 1')
abline(h=mean(resultados[[1]]), col='chocolate')
Media_N1=mean(resultados[[1]])
Varianza_N1=var(resultados[[1]])

#Media y varianza de N2:

plot(resultados[[2]],xlab = 'Simulaciones', ylab = 'Probabilidad N2', ylim = c(10,20), main = 'Cantidad de baldes arrojados al recipiente por el estudiante 2')
abline(h=mean(resultados[[2]]), col='blue')
var(resultados[[2]])

##B

#Aproximaciones graficas de las funciones de densidad de T1, T2, W y L

plot(density(resultados[[3]]), main = "Densidad de T1")
plot(density(resultados[[4]]), main = "Densidad de T2")
plot(density(resultados[[5]]), main = "Densidad de W")
plot(density(resultados[[6]]), main = "Densidad de L")

#Media y varianza de W y L:

Media_W=mean(resultados[[5]])
Media_L=mean(resultados[[6]])
