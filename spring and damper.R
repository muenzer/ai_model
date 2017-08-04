library(deSolve)
library(dplyr)
library(ggplot2)

model <- function (t, y, parms) {
  with(as.list(c(y, parms)), {
    F <- -k * x
    dx <- F / c
    v <- dx
    list(dx, Force = F, Velocity = v)
  })
}

endofstroke <- function(x, y, parms) {
  with(as.list(c(y, parms)), {
    x - stroke
  })}

yini <- c(x = -40)
parms <- c(
  c = 3.4, k = 1, stroke = -6)
steps <- seq(0, 20, 0.1)
#out <- ode(y, steps, model, parms)
out <- lsodar(yini, steps, model, parms, rootfunc = endofstroke)

output <- as.data.frame(out) %>%
  mutate(Area = (x - lag(x))*(Force + lag(Force)) / 2) %>%
  mutate(Power = Force * Velocity)

output %>% 
  summarize(time = max(time), Work = sum(Area, na.rm = TRUE), Power = mean(Power), Force_Sq = mean(Force^2)) %>%
  mutate(Metric = Work / Force_Sq) %>%
  mutate(Time_estimate = Metric * parms[1])

ggplot(data = output, mapping = aes(x = time, y = Force)) + geom_path() 

ggplot(data = output, mapping = aes(x = Force, y = Velocity)) + geom_path() 