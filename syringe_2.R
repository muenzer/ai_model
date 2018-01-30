library(deSolve)

v <- 200 / 60

# the forcing functions; rule = 2 avoids NaNs in interpolation
Speed <- approxfun(x = c(0,1,100), y = c(0,v,v), method = "linear", rule = 2)

model <- function(time, y, params) {
  with(as.list(c(y, params)), {
    V_air = (b * A) * (P_atm) / P
    dx = Speed(time)
    dP = -P / V_air * ((-dx * A) + (P / (R * mu)))
    dV = -P / (R * mu)
    
    dz = Speed(time) - ((sigma_0 * abs(Speed(time))) / Fc) * z
    Ff = sigma_0 * z
    
    F = P * A + Fc
    
    Q = -dV
    list(c(dx, dP, dV, dz), F = F, b_air = V_air / A, Q = Q, Ff = Ff)
  })
}

endofdelivery <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    x - 34
  })}


yini <- c(
  x = 0,
  P = 0.101325, #1 ATM
  V = 2000, #mm^3
  z = 0
) 
params <- c(
  Fc = 3, #N
  Fs = 5, #N
  v_s = 2,
  j = 1,
  sigma_0 = 100,
  A = 58.77, #mm^2
  mu = 9.1, #cP
  R = .000108, #MPa / (cP * mm^3/s)
  b = 3, #mm
  P_atm = 0.101325, #1 ATM
  speed = 500/60 #mm/s
)

steps <- seq(0, 15, .1)

out <- lsodar(yini, steps, model, params, rootfunc = endofdelivery)
out

plot(out[,2], out[,6], type = 'l')