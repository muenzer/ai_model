library(FME)
library(dplyr)
library(pspline)

curveFit <- function(curve, cutoff, viscosity, resistance, area, bubble, plot = FALSE, p = c(1, 5, 5, 25), method = "bobyqa") {
  curve <- curve %>% 
    filter(Distance < cutoff) %>%
    mutate(Time = (row_number() - 1) / 100)
  
  input <- approxfun(x = curve$Distance, y = curve$Force, method = "linear", rule = 2)
  
  Distance <- approxfun(x = curve$Time, y = curve$Distance, method = "linear", rule = 2)
  
  Speed <- approxfun(x = curve$Time, y = predict(sm.spline(curve$Time, curve$Distance), curve$Time, 1), method = "linear", rule = 2)
  
  Model <- function(p, x) {
    force <- function(time, y, params) {
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
        Distance(x) - cutoff
      })}
    
    
    yini <- c(
      #x = 0,
      xb = bubble,
      z = 0
    ) 
    params <- c(
      Fc = p[1], #N
      sigma_0 = p[2],
      sigma_1 = p[3],
      A = area, #mm^2
      mu = viscosity, #cP
      R = resistance, #MPa / (cP * mm^3/s)
      b = bubble, #mm
      P_atm = 0.101325, #1 ATM
      k = p[4]
    )
    
    steps <- seq(0, 15, .1)
    
    out <- lsodar(yini, steps, force, params, rootfunc = endofdelivery)
    
    return(data.frame(Distance = out[,4], Force = out[,5]))
  }
  
  Residuals <- function(p) {
    out <- Model(p, curve$Distance) %>% distinct(Distance, .keep_all = TRUE)
    obs <- data.frame(Distance = out$Distance, Force = input(out$Distance))
    
    return(modCost(out, obs, x = 'Distance'))
  }
  
  P <- modFit(f = Residuals, p, upper = c(5, 10, 10, 50), lower = c(0, 0, 0, 1), method = method)
  
  if(plot) {
    out = Model(P$par, curve$Distance)
    plot(out$Distance, out$Force, type = 'l')
    points(curve$Distance, curve$Force)
  }
  
  return(P$par)
}
