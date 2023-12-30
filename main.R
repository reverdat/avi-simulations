# -------- EMPIRICAL COMPARISON AVI vs FIXED-HORIZON --------
install.packages("future")
library(mixtureSPRT)
library(future)

source("simulation.R")
plan(multisession)
set.seed(123)

simulations <- vector(mode = "list", length = 4)
for(i in 1:4){
  simulations[[i]] <- future(
    {Simulation.simulate(B = 10000, M = 10^i, alpha = 0.01, sigma = sqrt(0.01))}
  )
}

output <- value(simulations)

sim$length_profile/sim$n_star_profile
power.t.test(n=NULL, 
             delta = 0.0006, 
             sd = sqrt(2)/2, 
             type="two.sample",
             alternative = "two.sided",
             power = 0.421, 
             sig.level = 0.05)
