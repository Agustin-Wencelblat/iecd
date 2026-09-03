# Ejercicio 2

set.seed(42)

# Defino los parametros reales alpha y lambda
alpha0 <- 3
lambda0 <- 4

numeros <- c(6,10,20,40,80,200)
N = 1000

# Estimador de momentos 
estimar_momentos <- function(x){
  m1 <- mean(x)
  m2 <- mean(x^2)
  var_n <- m2 - m1^2
  alpha_hat <- m1^2/var_n
  lambda_hat <- m1/var_n
  c(alpha = alpha_hat, lambda = lambda_hat)
}

# Calculo el EMV usando Newton-Raphson, usando las funciones digamma y trigamma para las derivadas de la funcion gamma

estimar_MV2 <- function(x){
  fit <- suppressWarnings(fitdistr(x, "gamma"))
  c(alpha = unname(fit$estimate["shape"]), lambda = unname(fit$estimate["rate"]))
}

# Si uso fitdistr, los resultados entre usar MM o EMV son distintos, si uso mi Newton Raphson terminan siendo iguales

estimar_MV <- function(x, iter = 200, tol = 1e-10){
  m1 <- mean(x)
  c_target <- log(m1) - mean(log(x))
  
  g <- function(a) log(a) - digamma(a) - c_target
  gp <- function(a) 1/a - trigamma(a)
  
  a <- estimar_momentos(x)["alpha"]
  for (i in 1:iter){
    paso <- g(a)/gp(a)
    a_nuevo <- a - paso
    if(a_nuevo <= 0) a_nuevo <- a/2
    if(abs(a_nuevo - a) < tol){a <- a_nuevo; break}
  }
  alpha_hat <- a
  lambda_hat <- alpha_hat/m1
  c(alpha = alpha_hat, lambda = lambda_hat)
}

# Hago las simulaciones para calcular los estimadores

resultados <- list()

for(n in numeros){
  mm <- matrix(NA_real_, N, 2, dimnames = list(NULL,c("alpha", "lambda")))
  emv <-matrix(NA_real_, N, 2, dimnames = list(NULL,c("alpha", "lambda")))
  
  for(i in 1:N){
    x <- rgamma(n, shape = alpha0, rate = lambda0)
    mm[i,]  <- estimar_momentos(x)
    emv[i, ] <- estimar_MV2(x)
  }
  resultados[[as.character(n)]] <- list(mm = mm, emv = emv)
}

print(resultados)

for(n in numeros){
  mm <- resultados[[as.character(n)]]$mm
  emv <- resultados[[as.character(n)]]$emv
  
  par(mfrow = c(2,2))
  hist(mm[, "alpha"], main = paste0("alpha MM, n=", n), xlab = "alpha_MM", col ="lightblue", breaks = 30)
  abline(v = alpha0, col = "red", lwd = 2)
  hist(emv[, "alpha"], main = paste0("alpha EMV, n=", n), xlab = "alpha_EMV", col ="lightgreen", breaks = 30)
  abline(v = alpha0, col = "red", lwd = 2)
  hist(mm[, "lambda"], main = paste0("lambda MM, n=", n), xlab = "lambda_MM", col ="lightblue", breaks = 30)
  abline(v = lambda0, col = "red", lwd = 2)
  hist(emv[, "lambda"], main = paste0("lambda EMV, n=", n), xlab = "lambda_EMV", col ="lightgreen", breaks = 30)
  abline(v = lambda0, col = "red", lwd = 2)
  
  par(mfrow = c(1,2))
  boxplot(mm[, "alpha"], emv[, "alpha"], names = c("MM", "EMV"), main = paste0("alpha, n=", n), col = c("lightblue", "lightgreen"))
  abline(v = alpha0, col = "red", lwd = 2)
  boxplot(mm[, "lambda"], emv[, "lambda"], names = c("MM", "EMV"), main = paste0("lambda, n=", n), col = c("lightblue", "lightgreen"))
  abline(v = lambda0, col = "red", lwd = 2)
  
}

# Ahora vamos a ver los ECMS

ecm_tabla <- data.frame(
  n = numeros,
  ecm_alpha_mm = sapply(numeros, function(n) mean((resultados[[as.character(n)]]$mm[,"alpha"] - alpha0)^2)),
  ecm_alpha_emv = sapply(numeros, function(n) mean((resultados[[as.character(n)]]$emv[,"alpha"] - alpha0)^2, na.rm = TRUE)),
  ecm_lambda_mm = sapply(numeros, function(n) mean((resultados[[as.character(n)]]$mm[,"lambda"] - lambda0)^2)),
  ecm_lambda_emv = sapply(numeros, function(n) mean((resultados[[as.character(n)]]$emv[,"lambda"] - lambda0)^2, na.rm = TRUE))
)

print(ecm_tabla)

plot(ecm_tabla$n, ecm_tabla$ecm_alpha_mm, type = "b", col = "blue", pch =16, lwd = 2,
     ylim = range(ecm_tabla[,-1]),
     xlab = "n", ylab = "ECM Muestral",
     main = "Comparacion ECM vs n, a escala completa")
lines(ecm_tabla$n, ecm_tabla$ecm_alpha_emv, type = "b", col = "darkblue", pch = 17, lwd = 2)
lines(ecm_tabla$n, ecm_tabla$ecm_lambda_mm, type = "b", col = "red", pch = 16, lwd = 2)
lines(ecm_tabla$n, ecm_tabla$ecm_alpha_emv, type = "b", col = "darkred", pch = 17, lwd = 2)
legend("topright", legend = c("alpha MM", "alpha EMV", "lambda MM", "lambda EMV"),
       col = c("blue","darkblue","red","darkred"), pch = c(16,17,16,17), lwd = 2)

# ahora lo hacemos con zoom

ns_zoom <- numeros[numeros >= 20]
idx_zoom <- ecm_tabla$n %in% ns_zoom

plot(ecm_tabla$n[idx_zoom], ecm_tabla$ecm_alpha_mm[idx_zoom], type = "b", col = "blue", pch =16, lwd = 2,
     ylim = range(ecm_tabla[idx_zoom,-1]),
     xlab = "n", ylab = "ECM Muestral",
     main = "Comparacion ECM vs n, a escala completa")
lines(ecm_tabla$n[idx_zoom], ecm_tabla$ecm_alpha_emv[idx_zoom], type = "b", col = "darkblue", pch = 17, lwd = 2)
lines(ecm_tabla$n[idx_zoom], ecm_tabla$ecm_lambda_mm[idx_zoom], type = "b", col = "red", pch = 16, lwd = 2)
lines(ecm_tabla$n[idx_zoom], ecm_tabla$ecm_alpha_emv[idx_zoom], type = "b", col = "darkred", pch = 17, lwd = 2)
legend("topright", legend = c("alpha MM", "alpha EMV", "lambda MM", "lambda EMV"),
       col = c("blue","darkblue","red","darkred"), pch = c(16,17,16,17), lwd = 2)






