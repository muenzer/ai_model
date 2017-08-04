library(deSolve)

params <- c(
  nRT = 17.8646,
  k = 100,
  A = 58.77, #mm^2
  mu = 9.1, #cP
  R = .000108, #MPa / (cP * mm^3/s)
  b = 3, #mm
  P_atm = 0.101325, #1 ATM
  speed = 34/60, #mm/s
  sigma = 100,
  F_c = 1.5, #N
  F_s = 20, #N
  v_s = 0.1 #mm/s
)

# the forcing functions; rule = 2 avoids NaNs in interpolation
Speed <- approxfun(x = c(0,1,100), y = c(0,params['speed'],params['speed']), method = "linear", rule = 2)

model <- function(time, y, params) {
  with(as.list(c(y, params)), {
    speed = Speed(time)
    dF_spring = 1/k
    dF_bubble = (nRT/(F + P_atm * A)^2)
    dF_needle = F/(A^2 * mu * R)
    dF = (speed - dF_needle) / (dF_spring + dF_bubble)
    v_f = speed - dF/k
    dz = v_f - (sigma * z) / (F_c + (F_s - F_c) * exp(-(abs(v_f/v_s)))) * abs(v_f) 
    dV = -(F/A) / (R * mu)
    dx = speed
    bubble = nRT/(F/A + P_atm)/A
    list(c(dF, dV, dx, dz), Ff = sigma * z, xf_dot = speed - dF/k)
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
  z = 0
) 


steps <- seq(0, 5, .1)

out <- lsodar(yini, steps, model, params, rootfunc = endofdelivery)
out