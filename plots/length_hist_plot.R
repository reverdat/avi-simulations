library(ggplot2)
library(comprehenr)
library(latex2exp)


source("simulation.R")

length_hist_plot <- function(simulation, mde_prop){
    # ----------------------------------------------------
    # Creates a histogram of the run-length ratio for
    # a given simulation and MDE %.
    #
    # Parameters:
    #   simulation - Simulation object
    #   mde_prop -  MDE estimate as proportion of true effect size
    # ----------------------------------------------------
    lengths <- simulation$lengths
    params <- simulation$params
    lengths <- lengths[ lengths < params$M ]
    n_star <- power.t.test(n=NULL, 
               delta = params$mean_theta*mde_prop, 
               sd = sqrt(2)/2, 
               type="one.sample",
               alternative = "two.sided",
               power = simulation$power_profile, 
               sig.level = params$alpha)
    n <- n_star$n
    lengths_per_n <- lengths / n
    
    ggplot(data = data.frame(LengthsPerN = lengths_per_n), aes(x = LengthsPerN)) +
        geom_histogram(binwidth = 0.05, color = "black",   aes(y = ..density..)) +
        labs(title = "Run-length ratio histogram",
            subtitle = TeX(paste("($\\hat{\\nu}$ = ", simulation$power_profile, ", $\\mu_{\\theta}$ = ", params$mean_theta,  ", $MDE$\\% = ", mde_prop*100,  ")")),
             x = TeX("$\\hat{\\rho}(M, \\alpha) / \\hat{\\rho_{f}}(M, \\alpha)$"),
             y = "Percentage of rejected tests") +
        theme_minimal() +
        xlim(0, 4) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 6)) +
        theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
        )

}