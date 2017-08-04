library(deSolve)

#Inputs
Fs <- 30    #Force at start of injection
Fe <- 15    #Force at end of injection
D <- 8.65   #Syringe diameter
d <- 0.285  #Needle diamter
L <- 17.5   #Needle length
mu <- 8.5   #Viscosity
f <- 3.88   #Stopper Friction
V <- 2.05   #Delivered volume
bubble <- 0 #Height of bubble

#Calculated constants
r <- d / 2
A <- pi*(D/2)^2
stroke <- V / A * 1000 + bubble
curve <- data.frame(F = c(Fs, Fe), x = c(0,stroke))
c <- (8 * mu * L * A^2) / (pi * r^4) / 10e8


model <- function (t, y, parms) {
  with(as.list(c(y, parms)), {
    F <- approx(curve$x, curve$F, x)$y
    v <- (F - f) / c
    dx <- (F - f) / c
    list(dx, Force = F, Velocity = v)
  })
}

endofstroke <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    x - (stroke - bubble)
  })}

yini <- c(x = 0)
parms <- c(
  c = c, f = f, stroke = stroke, curve = curve, bubble = bubble)
steps <- seq(0, 10, 0.1)
out <- lsodar(yini, steps, model, parms, rootfunc = endofstroke)
