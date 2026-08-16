salchichas_A <- read.table("Datasets/salchichas_A.txt", header=TRUE)
salchichas_B <- read.table("Datasets/salchichas_B.txt", header=TRUE)
salchichas_C <- read.table("Datasets/salchichas_C.txt", header=TRUE)

# a) Unificar en "salchichas.txt" 

names(salchichas_A) <- c("CALORIAS", "SODIO")
names(salchichas_B) <- c("CALORIAS", "SODIO")
names(salchichas_C) <- c("CALORIAS", "SODIO")

salchichas_A$TIPO <- "A"
salchichas_B$TIPO <- "B"
salchichas_C$TIPO <- "C"

salchichas <- rbind(salchichas_A,salchichas_B,salchichas_C)

write.table(salchichas,
            "salchichas.txt",
            row.names = FALSE,
            sep = "\t"
            )

# b) Histograma calorias

par(mfrow = c(1, 3))

hist(salchichas_A$CALORIAS,
     main = "Salchichas A",
     xlab = "Calorías",
     col = "lightblue",
     border = "white")

hist(salchichas_B$CALORIAS,
     main = "Salchichas B",
     xlab = "Calorías",
     col = "lightgreen",
     border = "white")

hist(salchichas_C$CALORIAS,
     main = "Salchichas C",
     xlab = "Calorías",
     col = "lightpink",
     border = "white")

# c) Boxplot de calorias segun tipo

par(mfrow = c(1, 1))

boxplot(CALORIAS ~ TIPO,
        data = salchichas,
        main = "Calorías según tipo de salchicha",
        xlab = "Tipo de salchicha",
        ylab = "Calorías",
        col = c("lightblue", "lightgreen", "lightpink"))

# d) Repetir con sodio

par(mfrow = c(1, 3))

hist(salchichas_A$SODIO,
     main = "Salchichas A",
     xlab = "Sodio",
     col = "lightblue",
     border = "white")

hist(salchichas_B$SODIO,
     main = "Salchichas B",
     xlab = "Sodio",
     col = "lightgreen",
     border = "white")

hist(salchichas_C$SODIO,
     main = "Salchichas C",
     xlab = "Sodio",
     col = "lightpink",
     border = "white")

par(mfrow = c(1, 1))

boxplot(SODIO ~ TIPO,
        data = salchichas,
        main = "Sodio según tipo de salchicha",
        xlab = "Tipo de salchicha",
        ylab = "Sodio",
        col = c("lightblue", "lightgreen", "lightpink"))

