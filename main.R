# -------- EMPIRICAL COMPARISON AVI vs FIXED-HORIZON --------
install.packages("future")
library(mixtureSPRT)
library(future)
library(future.apply)

source("simulation.R")
plan(multicore)
set.seed(123)

#simulations <- vector(mode = "list", length = 4)


simulations <- future_lapply(1:3, function(i) {
  params <- Parameters(B = 100, M = 10^i, alpha = 0.05, mean_theta = 0.01, sigma_theta = sqrt(0.01)) # nolint
  Simulation.simulate(params = params)
}, future.seed = TRUE)

output <- value(simulations)

