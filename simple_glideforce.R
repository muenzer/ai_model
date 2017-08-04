library(deSolve)
library(dplyr)

model <- function (t, y, parms) {
  with(as.list(c(y, parms)), {
    F <- -k*x
    dx <- - F / c
    list(dx, Force = F, Velocity = v)
  })
}

endofstroke <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    x - stroke
  })}

spring <- data.frame(F = c(43, 23), x = c(39.1345, 73.1645))
spring.model <- lm(F ~ x, data = spring)
k <- - spring.model$coefficients[[2]]
FL <- spring.model$coefficients[[1]] / k

yini <- c(x = 0)
parms <- c(
  c = 3.3, k = k, f = 8.3, stroke = 34.71, FL = FL, CL = FL - 43 / k)
steps <- seq(0, 10, 0.1)
#out <- ode(y, steps, model, parms)
out <- lsodar(yini, steps, model, parms, rootfunc = endofstroke)

output <- as.data.frame(out) %>%
  mutate(Area = (x - lag(x))*(Force + lag(Force)) / 2)

output %>% summarize(time = max(time), Work = sum(Area, na.rm = TRUE))