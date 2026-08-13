buffalo <- scan("buffalo.txt")

# a
hist(buffalo,
     main="Normal",
     col="lightblue")

par(mfrow=c(2,2))
hist(buffalo,
     breaks = seq(20,130,10),
     main="A1",
     col="lightblue")

hist(buffalo,
     breaks = seq(22,132,10),
     main="A2",
     col="lightblue")

hist(buffalo,
     breaks = seq(24,134,10),
     main="A3",
     col="lightblue")

hist(buffalo,
     breaks = seq(16,136,10),
     main="A4",
     col="lightblue")

# b
hist(buffalo,
     breaks = seq(10,130,10),
     main="B",
     col="lightblue")

# c
prob.intervalo <- function(x, h, datos){
  mean(datos >= (x-h) & datos <= (x+h))
}

#d
p10 <- sapply(buffalo, prob.intervalo, h=10, datos=buffalo)
p20 <- sapply(buffalo, prob.intervalo, h=20, datos=buffalo)
p30 <- sapply(buffalo, prob.intervalo, h=30, datos=buffalo)


#e
rectangular <- function(x){
  ifelse(abs(x)<=1/2,1,0)
}

gaussiano <- function(x){
  dnorm(x)
}

epanechnikov <- function(x){
  ifelse(abs(x)<=1, 0.75*(1-x**2),0)
}

#f
kde <- function(datos,h,x_s,K){
  n <- length(datos)
  f_s <- sapply(x_s, function(x){
    mean(K((x-datos)/h))/h
  })
  return(f_s)
}

#g
x <- seq(min(buffalo),max(buffalo),length=200)

f10 <- kde(buffalo,10,x,rectangular)
f20 <- kde(buffalo,20,x,rectangular)
f30 <- kde(buffalo,30,x,rectangular)

par(mfrow=c(1,1))
plot(x,f10,type="l",col="red",ylim=range(c(f10,f20,f30)),main="Nucleo Rectangular",xlab="Nieve",ylab="Densidad")
lines(x,f20,col="blue")
lines(x,f30,col="darkgreen")
legend("topright",legend=c("h=10","h=20","h=30"), col=c("red","blue","darkgreen"),lty = 1)

#h
g10 <- kde(buffalo,10,x,gaussiano)
g20 <- kde(buffalo,20,x,gaussiano)
g30 <- kde(buffalo,30,x,gaussiano)

par(mfrow=c(1,1))
plot(x,g10,type="l",col="red",ylim=range(c(g10,g20,g30)),main="Nucleo Gaussiano",xlab="Nieve",ylab="Densidad")
lines(x,g20,col="blue")
lines(x,g30,col="darkgreen")

e10 <- kde(buffalo,10,x,epanechnikov)
e20 <- kde(buffalo,20,x,epanechnikov)
e30 <- kde(buffalo,30,x,epanechnikov)

par(mfrow=c(1,1))
plot(x,e10,type="l",col="red",ylim=range(c(e10,e20,e30)),main="Nucleo Epanchenikov",xlab="Nieve",ylab="Densidad")
lines(x,e20,col="blue")
lines(x,e30,col="darkgreen")

#i
par(mfrow=c(1,1))
hist(buffalo, probability=TRUE, col="lightgray",ylim=c(0,0.024))
lines(density(buffalo,kernel="gaussian",bw=5),col="red")
lines(density(buffalo,kernel="rectangular",bw=5),col="blue")
lines(density(buffalo,kernel="epanechnikov",bw=5),col="darkgreen")

legend("topright",legend=c("Gaussiana","Rectangular","Epanchenikov"), col=c("red","blue","darkgreen"),lty=1)

#j
par(mfrow=c(2,2))
for(h in c(1,5,10,50)){
  hist(buffalo, probability=TRUE, ylim=c(0,0.032), main=paste("h=",h),col="lightgray")
  lines(density(buffalo,kernel="gaussian",bw=h),col="red")
  lines(density(buffalo,kernel="rectangular",bw=h),col="blue")
  lines(density(buffalo,kernel="epanechnikov",bw=h),col="darkgreen")
}

















