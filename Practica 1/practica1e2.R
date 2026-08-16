# a) P(mujer | sobrevivió) y P(mujer)

datos <- read.csv("Datasets/datos_titanic.csv")

total_sobrevivientes <- sum(datos$Survived == 1)

mujeres_sobrevivientes <- sum(datos$Survived == 1 & datos$Sex == "female")

prob_mujer_dado_sobrevivio <- mujeres_sobrevivientes / total_sobrevivientes

total_mujeres <- sum(datos$Sex == "female")

total_pasajeros <- nrow(datos)

prob_mujer <- total_mujeres / total_pasajeros

# b) Tabla de contingencia Survived y Pclass

tabla <- table(datos$Survived, datos$Pclass)
rownames(tabla) <- c("No sobrevivio", "Sobrevivio")
colnames(tabla) <- c("1ra clase", "2da clase", "3ra clase")

prob_sobrevivir_clase <- prop.table(tabla, margin = 2)

print(prob_sobrevivir_clase)

# c) Barras comparando PClass y Survived

datos$Survived <- factor(datos$Survived,
                           levels = c(0, 1),
                           labels = c("No sobrevivió",
                                      "Sobrevivió"))

datos$Pclass <- factor(datos$Pclass,
                         levels = c(1, 2, 3),
                         labels = c("1ra clase",
                                    "2da clase",
                                    "3ra clase"))

tabla_grafico <- table(datos$Pclass, datos$Survived)

barplot(tabla_grafico,
        beside = TRUE,
        col = c("firebrick", "royalblue", "seagreen"),
        main = "Supervivencia según clase",
        xlab = "Clase del pasajero",
        ylab = "Cantidad de personas",
        legend.text = TRUE,
        args.legend = list(title = "Resultado",
                           x = "topright"),
        ylim = c(0, max(tabla_grafico) * 1.2))

# Otra opcion, con proporciones

tabla_prop <- prop.table(tabla_grafico, margin = 1)

barplot(t(tabla_prop),
        beside = TRUE,
        col = c("firebrick", "seagreen"),
        main = "Proporción de supervivencia según clase",
        xlab = "Resultado",
        ylab = "Proporción",
        legend.text = TRUE,
        args.legend = list(title = "Clase",
                           x = "topright"),
        ylim = c(0, 1))


