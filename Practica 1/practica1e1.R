# a) Diagnosis y su frecuencia relativa

datos <- read.csv("Datasets/Debernardi.csv")

frecuencia_relativa <- prop.table(table(datos$diagnosis))

tabla_diagnosis <- data.frame(
  diagnosis = names(frecuencia_relativa),
  frecuencia_relativa = as.numeric(frecuencia_relativa)
)

# b) Grafico de barras de frecuencia relativa de diagnosis

barplot(
  tabla_diagnosis$frecuencia_relativa,
  names.arg = tabla_diagnosis$diagnosis,
  col = "steelblue",
  main = "Frecuencia relativa de tipos de diagnosis",
  xlab = "Diagnosis",
  ylab = "Frecuencia Relativa",
  ylim = c(0, max(tabla_diagnosis$frecuencia_relativa)*1.2)
)
