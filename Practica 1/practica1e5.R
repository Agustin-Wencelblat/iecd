estudiantes <- read.table("Datasets/estudiantes.txt", header = TRUE)

grupo1 <- estudiantes$GRUPO1
grupo2 <- estudiantes$GRUPO2

# a) Histogramas con curva normal

par(mfrow = c(1,2))

hist(grupo1,
     probability = TRUE,
     main = "Grupo 1",
     xlab = "Concentracion de nitrato",
     col = "lightblue",
     border = "white"
)

curve(dnorm(x,
            mean = mean(grupo1),
            sd = sd(grupo1)),
      add = TRUE,
      col = "firebrick",
      lwd = 2
)

hist(grupo2,
     probability = TRUE,
     main = "Grupo 2",
     xlab = "Concentracion de nitrato",
     col = "lightgreen",
     border = "white"
)

curve(dnorm(x,
            mean = mean(grupo2),
            sd = sd(grupo2)),
      add = TRUE,
      col = "firebrick",
      lwd = 2
)

par(mfrow = c(1,2))

qqnorm(grupo1,
       main = "QQ-plot Grupo 1",
       col = "lightblue")
qqline(grupo1,
       col = "firebrick",
       lwd = 2)

qqnorm(grupo2,
       main = "QQ-plot Grupo 2",
       col = "lightgreen")
qqline(grupo2,
       col = "firebrick",
       lwd = 2)

par(mfrow = c(1,1))

# b) Comparacion de los dos grupos

medidas_centrales <- data.frame(
  Medida = c("Media", "Mediana"),
  Grupo1 = c(mean(grupo1), median(grupo1)),
  Grupo2 = c(mean(grupo2), median(grupo2))
)

medidas_dispersion <- data.frame(
  Medida = c("Desvio estandar","IQR","MAD"),
  Grupo1 = c(sd(grupo1),IQR(grupo1),mad(grupo1)),
  Grupo2 = c(sd(grupo2),IQR(grupo2),mad(grupo2))
)

boxplot(grupo1, grupo2,
        names = c("Grupo 1", "Grupo 2"),
        main = "Concentración de ion nitrato",
        ylab = "Concentración (μg/l)",
        col = c("lightblue", "lightgreen"))



