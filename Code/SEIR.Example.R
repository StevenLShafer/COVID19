library(SimInf)

model <- SEIR(
  u0 = data.frame(
    S = 1000, 
    E = 0, 
    I = 1, 
    R = 0),
  tspan = 1:100,
  beta = 0.16,
  epsilon = 0.25,
  gamma = 0.077
  )

## Run the SEIR model and plot the result.
set.seed(3)
result <- run(model)
plot(result)
# }


# https://cran.r-project.org/web/packages/EpiDynamics/EpiDynamics.pdf
install.packages('EpiDynamics')
library(EpiDynamics)

# Initial conditions
initials <- c(S = 0.9, E = .01, I = .001, R = 1 - 0.9 - .01 - .001)

# parameters must be in the following order
# mu = the per capita death rate (and the population level birth rate), 
# beta = transmission rate,
# sigma =  the movement form exposed to infectious,
# gamma =  the recovery rate
parameters <- c(mu = 1 / (70 * 365), beta = 500 / 365,
                sigma = 1 / 14, gamma = 1 / 7)
# this example is meant to be read in days, for example the recovery rate at 1/7 means an individual is infectious, but recovers in 7 days.

# Solve and plot.
seir <- SEIR(pars = parameters, init = initials, time = 0:180)
PlotMods(seir)
