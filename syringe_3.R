library(deSolve)
library(readxl)
library(dplyr)

v <- 136.1 / 60

# the forcing functions; rule = 2 avoids NaNs in interpolation
Speed <- approxfun(x = c(0,.1,100), y = c(0,v,v), method = "linear", rule = 2)

folder = 'C:/working/Shear Stress/rawdata/2016-087'
file = '2016-087_Shear_Force_20160525_BD_136,1_3.xls'
path = file.path(folder, file)

rawdata = read_excel(path, sheet = 4, skip = 2) %>%
  select(Force = one_of('N'), Distance = one_of('mm')) %>%
  mutate(Cutoff = 25) %>%
  mutate(Time = (row_number() - 1) / 100) %>%
  filter(Distance < Cutoff)

Distance <- approxfun(x = rawdata$Time, y = rawdata$Distance, method = "linear", rule = 2)
  
model <- function(time, y, params) {
  with(as.list(c(y, params)), {
    x = Distance(time)  
    
    V1P1 = (b * A) * (P_atm)
    b_air = (xb - x)
    P = V1P1 / (A * b_air)
    
    #dx = Speed(time)
    dxb = ((P-P_atm)) / (A * R * mu)

    dz = Speed(time) - ((sigma_0 * abs(Speed(time))) / (Fc * (1 + x/k))) * z
    Ff = (sigma_0 * z + sigma_1 * dz)
    
    F = (P-P_atm) * A + Ff
    
    Q = dxb * A
    
    list(c(dxb, dz), x = x, F = F, b_air = b_air, P = P, Q = Q, Ff = Ff)
  })
}

endofdelivery <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    x - 34
  })}


yini <- c(
  #x = 0,
  xb = 3,
  z = 0
) 
params <- c(
  Fc = 0.89, #N
  sigma_0 = 4,
  sigma_1 = 6.9,
  A = 58.77, #mm^2
  mu = 9.1, #cP
  R = .000108, #MPa / (cP * mm^3/s)
  b = 3, #mm
  P_atm = 0.101325, #1 ATM
  k = 3.7
)

steps <- seq(0, 15, .1)

out <- lsodar(yini, steps, model, params, rootfunc = endofdelivery)
out

plot(out[,4], out[,5], type = 'l')
points(rawdata$Distance, rawdata$Force)