# Function to simulate a single realization of the competition
simulate_competencia <- function(m1, m2, v) {
  # Define the intervals (a, b) and (c, d) based on the values of m1 and m2
  a <- sqrt(8 * m1 + m2)
  b <- sqrt(10 * m1 + 10 * m2)
  c <- log(200 * m1 + 300 * m2)
  d <- log(400 * m1 + 600 * m2)
  
  # Generate random samples of the bucket load and time spent filling the bucket
  # for each student
  carga_balde_1 <- runif(1, a, b)
  carga_balde_2 <- runif(1, a, b)
  tiempo_1 <- runif(1, c, d)
  tiempo_2 <- runif(1, c, d)
  
  # Calculate the values of n1, n2, t1, t2, w, and l for this realization of the competition
  n1 <- v / carga_balde_1
  n2 <- v / carga_balde_2
  t1 <- n1 * tiempo_1
  t2 <- n2 * tiempo_2
  w <- if (t1 < t2) t1 else t2
  l <- if (t1 > t2) t1 else t2
  
  # Return the values of n1, n2, t1, t2, w, and l
  return(c(n1, n2, t1, t2, w, l))
}

# Define the values of m1, m2, and v
m1 <-12
m2 <- 12
v <- 190.9

# Create an empty data frame to store the results of the simulations
resultados <- data.frame(n1 = numeric(0), n2 = numeric(0), t1 = numeric(0), t2 = numeric(0), w = numeric(0), l = numeric(0))

# Simulate the competition 1000 times and store the results in the data frame
for (i in 1:1000) {
  resultado <- simulate_competencia(m1, m2, v)
  resultados <- rbind(resultados, resultado)

}

# Print the sample vectors of the variables N1, N2, T1, T2, W, and L
print(resultados$n1)
print(resultados$n2)
print(resultados$t1)
print(resultados$t2)
print(resultados$w)
print(resultados$l)

#Funciones de probabilidad aproximadas de N1 y N2

Prob_N1=mean(resultados[[1]])/1000
Prob_N2=mean(resultados[[2]])/1000

#Media y varianza de




plot(resultados[[1]],xlab = 'Simulaciones', ylab = 'Probabilidad N1', ylim = c(5,25), xlim = c(0,1000))
abline(h=mean(resultados[[1]]))
abline(h=var(resultados[[1]]))

plot(density(resultados[[3]]), main = "Densidad de T1")
plot(density(resultados[[4]]), main = "Densidad de T2")
plot(density(resultados[[5]]), main = "Densidad de W")

plot(density(resultados[[6]]), main = "Densidad de L")

plot(resultados[[2]],xlab = 'Simulaciones', ylab = 'Probabilidad N2', ylim = c(5,25), xlim = c(0,1000))
abline(h=mean(resultados[[2]]))

##
##plot(resultados[[3]],xlab = 'Simulaciones', ylab = 'Probabilidad T1')
##abline(h=mean(resultados[[3]]))
##plot(resultados[[4]],xlab = 'Simulaciones', ylab = 'Probabilidad T2')
##abline(h=mean(resultados[[4]]))
##plot(resultados[[5]],xlab = 'Simulaciones', ylab = 'Probabilidad W')
##abline(h=mean(resultados[[5]]))
##plot(resultados[[6]],xlab = 'Simulaciones', ylab = 'Probabilidad L')
##abline(h=mean(resultados[[6]]))
##