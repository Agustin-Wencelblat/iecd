datos <- read.csv("Datasets/Debernardi.csv")

# a) Histogramas de LYVE1 segun diagnosis

diagnosis1 <- datos$LYVE1[datos$diagnosis == 1]
diagnosis2 <- datos$LYVE1[datos$diagnosis == 2]
diagnosis3 <- datos$LYVE1[datos$diagnosis == 3]

hist(diagnosis1,
     main = "Histograma de LYVE1 con diagnosis 1",
     xlab = "LYVE1",
     ylab = "Frecuencia",
     col = "darkred")
hist(diagnosis2,
     main = "Histograma de LYVE1 con diagnosis 2",
     xlab = "LYVE1",
     ylab = "Frecuencia",
     col = "darkred")
hist(diagnosis3,
     main = "Histograma de LYVE1 con diagnosis 3",
     xlab = "LYVE1",
     ylab = "Frecuencia",
     col = "darkred")

# b) Comparacion de las FDAs empiricas de LYVE1

F1 <- ecdf(diagnosis1)
F2 <- ecdf(diagnosis2)
F3 <- ecdf(diagnosis3)

plot(F1,
     main = "Distribución empírica de LYVE1",
     xlab = "LYVE1",
     ylab = "F(x)",
     col = "red")

lines(F2, col = "blue")
lines(F3, col = "green")

legend("bottomright",
       legend = c("Diagnosis = 1",
                  "Diagnosis = 2",
                  "Diagnosis = 3"),
       col = c("red", "blue", "green"),
       lty = 1)

# c) Boxplots LYVE1 segun diagnosis, discriminando por sexo

par(mfrow = c(1,3))

boxplot(LYVE1 ~ sex,
        data = datos[datos$diagnosis == 1, ],
        main = "Diagnosis = 1",
        xlab = "Sexo",
        ylab = "LYVE1")

boxplot(LYVE1 ~ sex,
        data = datos[datos$diagnosis == 2, ],
        main = "Diagnosis = 2",
        xlab = "Sexo",
        ylab = "LYVE1")

boxplot(LYVE1 ~ sex,
        data = datos[datos$diagnosis == 3, ],
        main = "Diagnosis = 3",
        xlab = "Sexo",
        ylab = "LYVE1")

par(mfrow = c(1,1))

# d) Densidades estimadas de LYVE1 segun diagnosis

d1 <- density(diagnosis1)
d2 <- density(diagnosis2)
d3 <- density(diagnosis3)

xmin <- min(d1$x, d2$x, d3$x)
xmax <- max(d1$x, d2$x, d3$x)

ymax <- max(d1$y, d2$y, d3$y)

plot(d1,
     xlim = c(xmin, xmax),
     ylim = c(0, ymax),
     main = "Densidades estimadas de LYVE1",
     xlab = "LYVE1",
     ylab = "Densidad",
     col = "red")

lines(d2, col = "blue")
lines(d3, col = "green")

# e) Repito a y d con logaritmo

datos$logLYVE1 <- log(datos$LYVE1)

logLYVE1_1 <- datos$logLYVE1[datos$diagnosis == 1]
logLYVE1_2 <- datos$logLYVE1[datos$diagnosis == 2]
logLYVE1_3 <- datos$logLYVE1[datos$diagnosis == 3]

par(mfrow = c(1,3))

hist(logLYVE1_1,
     main = "Diagnosis = 1",
     xlab = "log(LYVE1)",
     ylab = "Frecuencia")

hist(logLYVE1_2,
     main = "Diagnosis = 2",
     xlab = "log(LYVE1)",
     ylab = "Frecuencia")

hist(logLYVE1_3,
     main = "Diagnosis = 3",
     xlab = "log(LYVE1)",
     ylab = "Frecuencia")

par(mfrow = c(1,1))

d1 <- density(logLYVE1_1)
d2 <- density(logLYVE1_2)
d3 <- density(logLYVE1_3)

xmin <- min(d1$x, d2$x, d3$x)
xmax <- max(d1$x, d2$x, d3$x)

ymax <- max(d1$y, d2$y, d3$y)

plot(d1,
     xlim = c(xmin, xmax),
     ylim = c(0, ymax),
     main = "Densidades de log(LYVE1)",
     xlab = "log(LYVE1)",
     ylab = "Densidad",
     col = "red")

lines(d2, col = "blue")
lines(d3, col = "green")

legend("topright",
       legend = c("Diagnosis = 1",
                  "Diagnosis = 2",
                  "Diagnosis = 3"),
       col = c("red", "blue", "green"),
       lty = 1)