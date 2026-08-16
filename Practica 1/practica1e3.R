iridio <- scan("Datasets/iridio.txt", skip = 1)
rodio <- scan("Datasets/rodio.txt", skip = 1)

# a) Comparacion historgrama y boxplot

par(mfrow = c(1,2))

hist(
  iridio,
  main = "Temperatura de sublimacion - Iridio",
  xlab = "Temperatura",
  col = "lightblue"
)

hist(
  rodio,
  main = "Temperatura de sublimacion - Rodio",
  xlab = "Temperatura",
  col = "lightgreen"
)

par(mfrow = c(1,2))

boxplot(
  iridio,
  names = "Iridio",
  main =  "Temperatura de sublimacion - Iridio",
  ylab = "Temperatura",
  col = "lightblue"
)

boxplot(
  rodio,
  names = "Rodio",
  main =  "Temperatura de sublimacion - Rodio",
  ylab = "Temperatura",
  col = "lightgreen"
)

# b) Medidas centrales

media_podada <- function(x, porcentaje){
  mean(x, trim = porcentaje)
}

resultados_medidas_centrales <- data.frame(
  Medida = c("Media", 
             "Mediana", 
             "Media Podada 10%", 
             "Media Podada 20%"),
  Iridio = c(mean(iridio),
             median(iridio),
             media_podada(iridio, 0.10), 
             media_podada(rodio, 0.20)),
  Rodio = c(mean(rodio),
            median(rodio),
            media_podada(rodio, 0.10), 
            media_podada(rodio, 0.20))
)

resultados_medidas_centrales

# c) Medidas de dispersion

resultados_medidas_dispersion <- data.frame(
  Medida = c("Desvio estandar",
             "Distancia intercuartil",
             "MAD"),
  Iridio = c(sd(iridio),
    IQR(iridio),
    mad(iridio)
  ),
  Rodio = c(sd(rodio),
    IQR(rodio),
    mad(rodio)
  )
)

resultados_medidas_dispersion

# d) Cuantiles

probabilidades <- c(0.90, 0.75, 0.50, 0.25, 0.10)

cuantiles <- data.frame(
  Cuantil = probabilidades,
  Iridio = quantile(iridio, probs = probabilidades),
  Rodio = quantile(rodio, probs = probabilidades)
)

cuantiles


