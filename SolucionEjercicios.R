# Solucion Ejercicio1

x2 <- 0:6
f2 <- dpois(x2,lambda=0.929)
plot(x2, f2, type="h", col="red", lwd=3, main="Función de probabilidad", xlab="X", ylab="f(x)",
     xlim=c(-0.5,6.5), ylim=c(0,0.5)) 
points(x2, f2, col="red", lwd=8); 


x2 <- 0:6
F2 <- ppois(x2,lambda=0.929)
plot(c(-1,x2,7), c(0,F2,1), type="s", col="red", lwd=3, main="Función de distribución", xlab="X",
     ylab="F(x)")
points(x2, F2, col="red", lwd=8)


dpois(2,lambda=0.929)

ppois(0,lambda=0.929,lower.tail = F)

1-dpois(0,lambda=0.929)


x3 <- 0:576
f3 <- dbinom(x3,size=576,prob=0.1704284)
plot(x3, f3, type="h", col="red", lwd=1, main="Función de probabilidad", xlab="X", ylab="f(x)",
     xlim=c(50,150), ylim=c(0,0.05)) 
points(x3, f3, col="red", lwd=3); 


F3 <- pbinom(x3,size=576,prob=0.1704284)
plot(c(-1,x3,578), c(0,F3,1), type="s", col="red", lwd=1, main="Función de distribución", xlab="X",
     ylab="F(x)", xlim=c(50,150))
points(x3, F3, col="red", lwd=1)


pbinom(99,size=576,prob=0.1704284)
576*0.1704284

x4 <- 10:40
f4 <- dnbinom(x4-10,size=10,prob=0.6050515) # Se ha de poner el número de fallos que ocurren antes de cumplir el objetivo de éxitos
plot(x4, f4, type="h", col="red", lwd=1, main="Función de probabilidad", xlab="X", ylab="f(x)",
     xlim=c(0,40.5), ylim=c(0,0.15)) 
points(x4, f4, col="red", lwd=3); 

F4 <- pnbinom(x4-10,size=10,prob=0.6050515)
plot(c(0,x4,40), c(0,F4,1), type="s", col="red", lwd=1, main="Función de distribución", xlab="X",
     ylab="F(x)")
points(x4, F4, col="red", lwd=1)

pnbinom(19-10,size=10,prob=0.6050515, lower.tail = F)

10/0.6050515



# Solucion Ejercicio2

mu = 9000
sigma = 2000
curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-4*sigma, mu+4*sigma), col="red",
      lwd=3, main="N(9000,2000²)", xlab="X", ylab="f(x)")

curve(pnorm(x, mean=mu, sd=sigma), xlim = c(mu-4*sigma, mu+4*sigma), col="red",
      lwd=3, main="N(9000,2000²)", xlab="X", ylab="F(x)")

F10000 <- pnorm(10000, mean=mu, sd=sigma); F10000

curve(pnorm(x, mean=mu, sd=sigma), xlim = c(mu-4*sigma, mu+4*sigma), col="red",
      lwd=3, main="N(9000,2000²)", xlab="X", ylab="F(x)")
abline(v=10000, col="blue"); abline(h=F10000, col="blue")
text(10000, F10000,expression(P(X<=10000)), pos=2, col="blue")

FC12000 <- pnorm(12000, mean=mu, sd=sigma, lower.tail = FALSE); FC12000


curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-4*sigma, mu+4*sigma), col="red",
      lwd=3, main="N(9000,2000²)", xlab="X", ylab="f(x)")
cord.x=c(12000,seq(12000,15000,length=100),15000) # Vector de vértices en x para elpolígono
cord.y=c(0,dnorm(seq(12000,15000,length=100),mean=mu,sd=sigma),0) # Vector de vértices en y
polygon(cord.x, cord.y, col='skyblue')


pnorm(10000, mean=mu, sd=sigma) - pnorm(7500, mean=mu, sd=sigma)


curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-4*sigma, mu+4*sigma), col="red",
      lwd=3, main="N(9000,2000²)", xlab="X", ylab="f(x)")
cord.x=c(7500,seq(7500,10000,length=100),10000) # Vector de vértices en x para elpolígono
cord.y=c(0,dnorm(seq(7500,10000,length=100),mean=mu,sd=sigma),0) # Vector de vértices en y
polygon(cord.x, cord.y, col='skyblue')

0

qnorm(0.9, mean=mu, sd=sigma, lower.tail = FALSE)

qnorm(0.3, mean=mu, sd=sigma)

set.seed(123)
sim.X <- rnorm(10000, mean=mu, sd=sigma)

hist(sim.X, freq=FALSE);
curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-4*sigma, mu+4*sigma), col="red",
      lwd=3, main="N(9000,2000²)", xlab="X", ylab="f(x)",add=TRUE)


mean.sim <- mean(sim.X); mean.sim

med.sim <- median(sim.X); med.sim

sd.sim <- sd(sim.X); sd.sim



# Solucion Ejercicio3


lambda = 0.01005
curve(dexp(x, rate=lambda), xlim = c(0, 4/lambda), col="red",
      lwd=3, main="Exp(0.01005)", xlab="X", ylab="f(x)")

curve(pexp(x, rate=lambda), xlim = c(0, 4/lambda), col="red",
      lwd=3, main="Exp(0.01005)", xlab="X", ylab="F(x)")

F100 <- pexp(100, rate=lambda); F100

curve(pexp(x, rate=lambda), xlim = c(0, 4/lambda), col="red",
      lwd=3, main="Exp(0.01005)", xlab="X", ylab="F(x)")
abline(v=100, col="blue"); abline(h=F100, col="blue")
text(100, F10000,expression(P(X<=100)), pos=2, col="blue")

pexp(110, rate=lambda) - pexp(80, rate=lambda)

curve(dexp(x, rate=lambda), xlim = c(0, 4/lambda), col="red", lwd=3, main="Exp(0.01005)", xlab="X", ylab="f(x)")
cord.x=c(80,seq(80,110,length=100),110) # Vector de vértices en x para elpolígono
cord.y=c(0,dexp(seq(80,110,length=100),rate=lambda),0) # Vector de vértices en y
polygon(cord.x, cord.y, col='skyblue')

qexp(0.5, rate=lambda) 


set.seed(123)
sim.X <- rexp(10000, rate=lambda)

hist(sim.X, freq=FALSE);
curve(dexp(x, rate=lambda), xlim = c(0, 4/lambda), col="red", lwd=3, main="Exp(0.01005)", xlab="X", ylab="f(x)", add=TRUE)

mean.sim <- mean(sim.X); mean.sim

med.sim <- median(sim.X); med.sim

sd.sim <- sd(sim.X); sd.sim



