datos <- read.csv("Datasets/data_credit_card.csv", header = TRUE)


variables <- c("purchases", "credit_limit", "purchases_freq", "tenure")

# ---- item a) Funciones de distribución empírica ----------------------------
par(mfrow = c(2, 2))
for (v in variables) {
  plot(ecdf(datos[[v]]),
       main = paste("ECDF -", v),
       xlab = v, ylab = expression(F[n](x)),
       verticals = TRUE, do.points = FALSE)
}
par(mfrow = c(1, 1))

# ---- item b) Histograma y densidad de credit_limit -------------------------
hist(datos$credit_limit, freq = FALSE, breaks = "FD",
     main = "Histograma de credit_limit", xlab = "credit_limit",
     col = "lightgray", border = "white")
lines(density(datos$credit_limit), col = "steelblue", lwd = 2)

hist(datos$purchases, freq = FALSE, breaks = "FD",
     main = "Histograma de purchases", xlab = "purchases",
     col = "lightgray", border = "white")
lines(density(datos$purchases), col = "steelblue", lwd = 2)

hist(datos$tenure, freq = FALSE, breaks = "FD",
     main = "Histograma de tenure", xlab = "tenure",
     col = "lightgray", border = "white")
lines(density(datos$tenure), col = "steelblue", lwd = 2)

# ---- item c) Barplot de frecuencias relativas de tenure ---------------------
tab_tenure <- table(datos$tenure) / nrow(datos)
barplot(tab_tenure,
        main = "Frecuencias relativas de tenure",
        xlab = "tenure (meses)", ylab = "Frecuencia relativa",
        col = "lightblue")

# ---- item d) Media, mediana y media podada (alpha = 0.1) -------------------
media_podada <- function(x, alpha = 0.1) mean(x, trim = alpha)

resumen_centralidad <- data.frame(
  variable     = variables,
  media        = sapply(variables, function(v) mean(datos[[v]])),
  mediana      = sapply(variables, function(v) median(datos[[v]])),
  media_podada = sapply(variables, function(v) media_podada(datos[[v]], 0.1))
)
print(resumen_centralidad)

# ---- item e) Cuantiles, RIC, MAD y boxplots ---------------------------------
resumen_dispersion <- data.frame(
  variable = variables,
  Q1  = sapply(variables, function(v) quantile(datos[[v]], 0.25)),
  Q3  = sapply(variables, function(v) quantile(datos[[v]], 0.75)),
  RIC = sapply(variables, function(v) IQR(datos[[v]])),
  MAD = sapply(variables, function(v) mad(datos[[v]]))  # constant=1.4826 (comparable con sd)
)
print(resumen_dispersion)

par(mfrow = c(2, 2))
for (v in variables) {
  boxplot(datos[[v]], main = paste("Boxplot -", v), col = "lightyellow")
}
par(mfrow = c(1, 1))

# ---- item f) Desvío estándar, asimetría y curtosis --------------------------
# install.packages("moments")  # descomentar si no está instalado
library(moments)

resumen_forma <- data.frame(
  variable  = variables,
  sd        = sapply(variables, function(v) sd(datos[[v]])),
  asimetria = sapply(variables, function(v) skewness(datos[[v]])),
  curtosis  = sapply(variables, function(v) kurtosis(datos[[v]]))  # kurtosis "cruda" (normal = 3)
)
print(resumen_forma)

# ---- item g) Identificación y efecto de los datos atípicos -----------------
detectar_atipicos <- function(x) {
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  ric <- q3 - q1
  lim_inf <- q1 - 1.5 * ric
  lim_sup <- q3 + 1.5 * ric
  which(x < lim_inf | x > lim_sup)
}

atipicos <- lapply(variables, function(v) detectar_atipicos(datos[[v]]))
names(atipicos) <- variables
print(sapply(atipicos, length))  # cantidad de atípicos por variable

# Ejemplo: comparar medidas con y sin atípicos para purchases
idx_atip <- atipicos[["purchases"]]
purchases_sin_atipicos <- datos$purchases[-idx_atip]

comparacion_purchases <- data.frame(
  medida       = c("media", "mediana", "sd"),
  con_atipicos = c(mean(datos$purchases), median(datos$purchases), sd(datos$purchases)),
  sin_atipicos = c(mean(purchases_sin_atipicos), median(purchases_sin_atipicos), sd(purchases_sin_atipicos))
)
print(comparacion_purchases)

# Repetir el bloque de comparación para credit_limit, purchases_freq y tenure
# cambiando la variable en idx_atip y en el data.frame de comparación.


