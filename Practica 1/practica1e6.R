nubes <- read.table("Datasets/nubes.txt", header = TRUE)

# a) Boxplots segun tratamiento

nubes_tratadas <- nubes$TRATADAS
nubes_control <- nubes$CONTROLES

par(mfrow = c(1,2))

boxplot(nubes_tratadas, 
        main = "Cantidad de agua caida en nubes tratadas",
        ylab = "Agua caida",
        col = "lightblue"
        )


boxplot(nubes_control, 
        main = "Cantidad de agua caida en nubes control",
        ylab = "Agua caida",
        col = "lightgreen"
)

# b) Analizar normalidad con qqplots e histogramas

par(mfrow = c(1, 2))

hist(nubes_tratadas,
     probability = TRUE,
     main = "Nubes tratadas",
     xlab = "Agua caída",
     col = "lightblue",
     border = "white")

curve(dnorm(x,
            mean = mean(nubes_tratadas),
            sd = sd(nubes_tratadas)),
      add = TRUE,
      col = "red",
      lwd = 2)


hist(nubes_control,
     probability = TRUE,
     main = "Nubes control",
     xlab = "Agua caída",
     col = "lightgreen",
     border = "white")

curve(dnorm(x,
            mean = mean(nubes_control),
            sd = sd(nubes_control)),
      add = TRUE,
      col = "red",
      lwd = 2)

par(mfrow = c(1, 2))

qqnorm(nubes_tratadas,
       main = "QQ-plot Tratadas",
       col = "blue")
qqline(nubes_tratadas,
       col = "red",
       lwd = 2)

qqnorm(nubes_control,
       main = "QQ-plot Control",
       col = "darkgreen")
qqline(nubes_control,
       col = "red",
       lwd = 2)

par(mfrow = c(1, 1))

# c) Transformacion logaritmica

log_nubes_tratadas <- log(nubes_tratadas)
log_nubes_control <- log(nubes_control)

par(mfrow = c(1, 2))

hist(log_nubes_tratadas,
     probability = TRUE,
     main = "Log - Nubes tratadas",
     xlab = "log(Agua caída)",
     col = "lightblue",
     border = "white")

curve(dnorm(x,
            mean = mean(log_nubes_tratadas),
            sd = sd(log_nubes_tratadas)),
      add = TRUE,
      col = "red",
      lwd = 2)


hist(log_nubes_control,
     probability = TRUE,
     main = "Log - Nubes control",
     xlab = "log(Agua caída)",
     col = "lightgreen",
     border = "white")

curve(dnorm(x,
            mean = mean(log_nubes_control),
            sd = sd(log_nubes_control)),
      add = TRUE,
      col = "red",
      lwd = 2)

par(mfrow = c(1, 2))

qqnorm(log_nubes_tratadas,
       main = "QQ-plot Log - Tratadas",
       col = "blue")
qqline(log_nubes_tratadas,
       col = "red",
       lwd = 2)

qqnorm(log_nubes_control,
       main = "QQ-plot Log - Control",
       col = "darkgreen")
qqline(log_nubes_control,
       col = "red",
       lwd = 2)

# d) Boxplots transformacion logaritmica

par(mfrow = c(1, 1))

boxplot(log_nubes_tratadas, log_nubes_control,
        names = c("Tratadas", "Control"),
        main = "Logaritmo de la cantidad de agua caída",
        ylab = "log(Agua caída)",
        col = c("lightblue", "lightgreen"))

