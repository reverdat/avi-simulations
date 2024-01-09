library(ggplot2)
library(comprehenr)
library(latex2exp)


source("simulation.R")

profiles_plot <- function(simulations){
  # ----------------------------------------------------
  # Creates a scatter plot of the average power profile
  # and average run-length profile as a function of M.
  #
  # Parameters:
  #   simulations - List of Simulation objects
  # ----------------------------------------------------
    powers <- to_vec(for(sim in simulations) sim$power_profile)
    lengths <- to_vec(for(sim in simulations) sim$length_profile)
    m_partition <- to_vec(for(sim in simulations) (sim$params)$M)
    params <- to_vec(for(sim in simulations) (sim$params))
    print(params)
    data <- data.frame(m_partition = m_partition, powers = powers, lengths = lengths) # nolint: line_length_linter.

    ggplot(data, aes(x = m_partition)) +
      geom_line(aes(y = powers, linetype = "Power Profile"), show.legend = TRUE) +
      geom_point(aes(y = powers, shape = "Power Profile"), show.legend = TRUE) +
      geom_line(aes(y = lengths, linetype = "Length Profile"), show.legend = TRUE) +
      geom_point(aes(y = lengths, shape = "Length Profile"), show.legend = TRUE) +
      labs(title = "Power and Length Profiles",
            subtitle = TeX(paste("($\\alpha$ = ", params[3], ", $\\mu_{\\theta}$ = ",params[4], ", $\\sigma_{\\theta}^{2}$ = ", params[5]^2, ")")),
            x = "M",
            y = "Profile") +
      scale_x_continuous(limits = c(0, 10000)) +
      scale_y_continuous(limits = c(0.0, 1.0)) +
      scale_shape_manual(name = "Profiles", values = c("Power Profile" = 16, "Length Profile" = 17),
                          labels = c("Power Profile" = expression(hat(nu)(M, alpha)),
                                    "Length Profile" = expression(hat(rho)(M, alpha)))) +
      scale_linetype_manual(name = "Profiles", values = c("Power Profile" = "solid", "Length Profile" = "solid"),
                            labels = c("Power Profile" = expression(hat(nu)(M, alpha)),
                                        "Length Profile" = expression(hat(rho)(M, alpha)))) +
      theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.title = element_blank()
        )
}