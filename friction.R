library(deSolve)

v <- 34 / 60

# the forcing functions; rule = 2 avoids NaNs in interpolation
Speed <- approxfun(x = c(0,1,100), y = c(0,v,v), method = "linear", rule = 2)


model <- function(time, y, params) {
  with(as.list(c(y, params)), {
    v = Speed(time)
    s = F_c + (F_s - F_c) * exp(-(abs(v/v_s))^j)

    dx = v
    dz = v - ((sigma_0 * abs(v)) / s) * z

    #F = sigma_0 * z + sigma_1 * dz + sigma_2 * v
    F = sigma_0 * z


    list(c(dx, dz), F = F, v = v, s = s)
  })
}

yini <- c(
  x = 0,
  z = 0
)

params <- c(
  F_s = 60,
  F_c = 1,
  v_s = 0.1,
  j = 1,
  sigma_0 = 1000,
  sigma_1 = 0,
  sigma_2 = 0
)

steps <- seq(0, 5, 0.002)

out <- lsodar(yini, steps, model, params)
out

plot(out[,2], out[,4], type = 'l')
