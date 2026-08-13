dietas <- read.table("dietas.txt",header = TRUE)

#a
media <- sapply(dietas, mean)
mediana <- sapply(dietas, median)
podada1 <- sapply(dietas, mean, trim = 0.1)
podada2 <- sapply(dietas, mean, trim = 0.2)

medidas_centralidad <- rbind(media = media, mediana = mediana, podada1 = podada1, podada2 = podada2)
print(medidas_centralidad)

#b
desvio <- sapply(dietas, sd)
IQR <- sapply(dietas, IQR)
MAD <- sapply(dietas, mad)

medidas_dispersion <- rbind(desvio = desvio, IQR = IQR, MAD = MAD)
print(medidas_dispersion)

#c
percentiles <- sapply(dietas, quantile, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
print(percentiles)

#d
par(mfrow=c(1,3))
for(i in 1:3){
  hist(dietas[[i]],
  main=names(dietas)[i],
  xlab="Glucosa",
  col="lightblue")
}

#e
par(mfrow=c(1,3))
for(i in 1:3){
  boxplot(dietas[[i]],
       main=names(dietas)[i],
       col="lightgreen")
}

#f
par(mfrow=c(1,3))
for(i in 1:3){
  qqnorm(dietas[[i]],
       main=names(dietas)[i])
  qqline(dietas[[i]], col="red")
}