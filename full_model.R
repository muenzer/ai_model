library(deSolve)

params <- c(
  nRT = 17.8646,
  k = 100,
  A = 58.77, #mm^2
  mu = 9.1, #cP
  R = .000108, #MPa / (cP * mm^3/s)
  b = 3, #mm
  P_atm = 0.101325, #1 ATM
  speed = 136/60, #mm/s
  sigma = 1000,
  F_c = 1.5, #N
  F_s = 60, #N
  v_s = 0.1, #mm/s
  j = 2
)

# the forcing functions; rule = 2 avoids NaNs in interpolation
Speed <- approxfun(x = c(0,1,100), y = c(0,params['speed'],params['speed']), method = "linear", rule = 2)

model <- function(time, y, params) {
  with(as.list(c(y, params)), {
    speed = Speed(time)
    v_f = speed
    dz = v_f - (sigma * z) / (F_c + (F_s - F_c) * exp(-(abs(v_f/v_s))^j)) * abs(v_f) 
    dFf = sigma * dz
    #dF_spring = 1/k
    dF_bubble = (nRT/((F - Ff) + P_atm * A)^2)
    dF_needle = (F - Ff)/(A^2 * mu * R)
    dF = (speed + dF_bubble * dFf - dF_needle) / (dF_bubble)
    dV = -(F - Ff) / A / (mu * R)
    dx = speed
    bubble = nRT/((F - Ff)/A + P_atm)/A
    list(c(dF, dV, dx, dz, dFf), Q = dV / 1000, b = bubble)
  })
}

endofdelivery <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    V
  })}


yini <- c(
  F = 0,
  V = 2000,
  x = 0,
  z = 0,
  Ff = 0
) 


steps <- seq(0, 100, .1)

out <- lsodar(yini, steps, model, params, rootfunc = endofdelivery)
tail(out)

plot(out[,4], out[,2], type = 'l')
