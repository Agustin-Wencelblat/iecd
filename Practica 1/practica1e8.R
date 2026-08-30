library(readxl)

datos <- read_excel("Datasets/ciclocombinado.xlsx")

PE <- datos$PE

# a) Histograma y density

hist(PE,
     breaks = 30,
     main = "Histograma de PE",
     xlab = "Potencia entregada (MW)",
     ylab = "Frecuencia")

plot(density(PE),
     main = "Densidad de PE",
     xlab = "Potencia entregada (MW)",
     ylab = "Densidad")

hist(PE,
     breaks = 30,
     probability = TRUE,
     main = "Histograma y densidad de PE",
     xlab = "Potencia entregada (MW)",
     ylab = "Densidad")

lines(density(PE))

# b) Separacion segun HighTemp

PE0 <- datos$PE[datos$HighTemp == 0]
PE1 <- datos$PE[datos$HighTemp == 1]

d0 <- density(PE0)
d1 <- density(PE1)

xmin <- min(c(d0$x, d1$x))
xmax <- max(c(d0$x, d1$x))
ymax <- max(c(d0$y, d1$y))

par(mfrow = c(1, 2))

plot(d0,
     xlim = c(xmin, xmax),
     ylim = c(0, ymax),
     main = "HighTemp = 0",
     xlab = "PE (MW)",
     ylab = "Densidad")

plot(d1,
     xlim = c(xmin, xmax),
     ylim = c(0, ymax),
     main = "HighTemp = 1",
     xlab = "PE (MW)",
     ylab = "Densidad")

par(mfrow = c(1, 1))

# c) Estimar P (PE < 450|HighTemp = 0) y P (PE < 300|HighTemp = 1)

mean(PE0 < 450)

mean(PE1 < 300)

# d) Estimar P (PE < 450)

mean(PE < 450)

# Alternativamente, si uso proba total 

p0 <- mean(datos$HighTemp == 0)
p1 <- mean(datos$HighTemp == 1)

p_PE450_0 <- mean(PE0 < 450)
p_PE450_1 <- mean(PE1 < 450)

p <- p_PE450_0 * p0 + p_PE450_1 * p1

p

# e) Estimar la potencia minima garantizada con probabilidad 0.9 para un cierto dia con Hightemp = 1

quantile(PE1, 0.10)

# f) Estimar la potencia minima garantizada con probabilidad 0.9 para un cierto dia.

quantile(PE, 0.10)
