library(mixtureSPRT)

Parameters <- function(B, M, alpha, mean_theta, sigma_theta) {
  # ----------------------------------------------------
  # Simulation parameters object. 

  # Attributes:
  #   B - Number of simulations to perform estimation
  #   M - Maximum sample size of user
  #   alpha - Significance level of user
  #   mean_theta - Mean of effect size prior distribution
  #   sigma_theta - Std of effect size prior distribution
  # ----------------------------------------------------

  params <- list(B = B, M = M,  alpha = alpha, mean_theta = mean_theta, sigma_theta = sigma_theta) # nolint
  class(params) <- "Parameters"
  return(params)
}

Simulation <- function(params, power_profile, length_profile, ump_profile) {
  # ----------------------------------------------------
  # Simulation results object. 
  # 
  # Attributes:
  #   params - Parameters of simulation
  #   power_profile - Average power profile of simulation
  #   length_profile - Average run-length profile
  #   ump_profile - Sample size/M corresponding to the UMP test with same power as power_profile # nolint
  # ----------------------------------------------------
  
  sim <- list(params = params, power_profile = power_profile, length_profile = length_profile, ump_profile = ump_profile) # nolint: line_length_linter.
  class(sim) <- "Simulation"
  return(sim)
}

Simulation.simulate <- function(params) {

  B <- params$B
  M <- params$M
  alpha <- params$alpha
  mean_theta <- params$mean_theta
  sigma_theta <- params$sigma_theta
  
  tau <- calcTau(alpha = alpha, sigma = sqrt(2)/2, truncation = M)
  rejections <- c()
  lengths <- c()
  thetas <- c()
  for(b in 1:B) {
    theta <- rnorm(n = 1, mean = mean_theta, sd = sigma_theta)[1]
    thetas <- append(thetas, theta)
    m <- mSPRT.default(x = rnorm(M, mean = 0, sd = sqrt(2)/2),
                        y = rnorm(M, mean = theta, sd = sqrt(2)/2),
                        sigma = sqrt(2)/2,
                        tau = tau,
                        theta = 0,
                        distribution = "normal",
                        alpha = alpha,
                        useCpp = T)
    if (m$decision == "Accept H1"){
      rejections <- append(rejections, 1)
      lengths <- append(lengths, m$n.rejection)
    } else {
      lengths <- append(lengths, M)
    }
  }
  pow <- sum(rejections)/B
  len <- sum(lengths)/(as.numeric(B*M))
  
  n_star <- power.t.test(n=NULL, 
               delta = mean_theta, 
               sd = sqrt(2)/2, 
               type="two.sample",
               alternative = "two.sided",
               power = pow, 
               sig.level = alpha)
  ump_profile <- min(n_star$n, M)/as.numeric(M)

  return(Simulation(params = params, power = pow, length = len, ump_profile = ump_profile)) # nolint
}


