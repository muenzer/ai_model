library(deSolve)

model <- function(time, y, params) {
  with(as.list(c(y, params)), {
    V_air = (b * A) * (P_atm) / P
    dx = speed
    dP = -P / V_air * ((-dx * A) + (P / (R * mu)))
    dV = -P / (R * mu)
    
    F = P * A + Ff
    
    Q = -dV
    list(c(dx, dP, dV), F = F, b_air = V_air / A, Q = Q)
  })
}

endofdelivery <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    x - 34
  })}


yini <- c(
  x = 0,
  P = 0.101325, #1 ATM
  V = 2000 #mm^3
) 
params <- c(
  Ff = 1.5, #N
  A = 58.77, #mm^2
  mu = 9.1, #cP
  R = .000108, #MPa / (cP * mm^3/s)
  b = 3, #mm
  P_atm = 0.101325, #1 ATM
  speed = 680.7/60 #mm/s
)

steps <- seq(0, 15, .1)

out <- lsodar(yini, steps, model, params, rootfunc = endofdelivery)
out