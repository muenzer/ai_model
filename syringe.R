library(deSolve)

model <- function(time, y, params) {
  with(as.list(c(y, params)), {
    dx <- speed
    db <- (0 / A) - dx
    dP <- -P / b * db
    dh <- -dx
    dV <- dh * A
    dV <- 0
    
    
    Q <- -dV
    P <- Q * R * mu
    list(c(dx, db, dP, dV))
  })
  
}

endofdelivery <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    V
  })}

yini <- c(
  x = 0,
  b = 100, #mm bubble size
  P = 0.101325, #1 ATM
  V = 2000 #mm^3
) 
params <- c(
  A = 58.77, #mm^2
  mu = 9.1, #cP
  R = .000108, #MPa / (cP * mm^3/s)
  #P = 0.2,
  speed = 3.4 #mm/s
)

steps <- seq(0, 20, 1)

out <- lsodar(yini, steps, model, params, rootfunc = endofdelivery)
out
