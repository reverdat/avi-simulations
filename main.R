install.packages("future")
install.packages("comprehenr")
install.packages("remotes")
install.packages("latex2exp")
remotes::install_github("erik-stenberg/mixtureSPRT")

library(comprehenr)
library(mixtureSPRT)
library(future)
library(future.apply)
library(ggplot2)

source("simulation.R")
source(("plots/profiles_plot.R"))
source(("plots/length_hist_plot.R"))

plan(multicore)
set.seed(123)

m_partition <- to_vec(for(i in 1:100) if(i %% 5 == 0) i*100 )

simulations_1 <- future_lapply(m_partition, function(i) {
  params_1 <- Parameters(B = 10000, M = i, alpha = 0.05, mean_theta = 0.05, sigma_theta = sqrt(0.001)) # nolint
  Simulation.simulate(params = params_1)
}, future.seed = TRUE)

simulations_2 <- future_lapply(m_partition, function(i) {
  params_2 <- Parameters(B = 10000, M = i, alpha = 0.01, mean_theta = 0.05, sigma_theta = sqrt(0.001)) # nolint
  Simulation.simulate(params = params_2)
}, future.seed = TRUE)

simulations_3 <- future_lapply(m_partition, function(i) {
  params_3 <- Parameters(B = 10000, M = i, alpha = 0.10, mean_theta = 0.05, sigma_theta = sqrt(0.001)) # nolint
  Simulation.simulate(params = params_3)
}, future.seed = TRUE)

simulations_4 <- future_lapply(m_partition, function(i) {
  params_4 <- Parameters(B = 10000, M = i, alpha = 0.05, mean_theta = 0.05, sigma_theta = sqrt(0.001)) # nolint
  Simulation.simulate(params = params_4)
}, future.seed = TRUE)

simulations_5 <- future_lapply(m_partition, function(i) {
  params_5 <- Parameters(B = 10000, M = i, alpha = 0.05, mean_theta = 0.05, sigma_theta = sqrt(0.01)) # nolint
  Simulation.simulate(params = params_5)
}, future.seed = TRUE)

simulations_6 <- future_lapply(m_partition, function(i) {
  params_6 <- Parameters(B = 10000, M = i, alpha = 0.05, mean_theta = 0.05, sigma_theta = sqrt(0.1)) # nolint
  Simulation.simulate(params = params_6)
}, future.seed = TRUE)

profiles_1 <- profiles_plot(simulations = simulations)
profiles_2 <- profiles_plot(simulations = simulations_2)
profiles_3 <- profiles_plot(simulations = simulations_3)

combined <- cowplot::plot_grid(profiles_3, profiles_1, profiles_2, ncol = 3)
ggsave("combined_plot.png", combined, width = 12, height = 4, dpi = 300)

profiles_4 <- profiles_plot(simulations = simulations_4)
profiles_5 <- profiles_plot(simulations = simulations_5)
profiles_6 <- profiles_plot(simulations = simulations_6)

combined_2 <- cowplot::plot_grid(profiles_4, profiles_5, profiles_6, ncol = 3)
ggsave("combined_plot_2.png", combined_2, width = 12, height = 4, dpi = 300)

params_75 <- Parameters(B = 10000, M = 5500, alpha =  0.05, mean_theta = 0.05, sigma_theta = 0.1)
sim_75 <- Simulation.simulate(params = params_75)

lens_props_1 <- length_hist_plot(simulation = sim_75, mde = 0.5)
lens_props_2 <- length_hist_plot(simulation = sim_75, mde = 0.8)
lens_props_3 <- length_hist_plot(simulation = sim_75, mde = 1)

combined_3 <- cowplot::plot_grid(lens_props_1, lens_props_2, lens_props_3, ncol = 3)
ggsave("lens_combined.png", combined_3, width = 12, height = 4, dpi = 300)