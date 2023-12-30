library(mixtureSPRT)

Simulation <- function(power_profile, length_profile, n_star_profile) {
  # ----------------------------------------------------
  # Simulation results object. 
  #
  # Attributes:
  #   power_profile - Average power profile of simulation
  #   length_profile - Average run-length profile
  #   n_star_profile - Sample size/M corresponding to the UMP test with same power as power_profile
  # ----------------------------------------------------
  
  sim <- list(power_profile = power_profile, length_profile = length_profile, n_star_profile = n_star_profile)
  class(sim) <- "Simulation"
  return(sim)
}

Simulation.simulate <- function(B, M, alpha, sigma) {
  
  tau <- calcTau(alpha = alpha, sigma = sqrt(2)/2, truncation = M)
  rejections <- c()
  lengths <- c()
  thetas <- c()
  for(b in 1:B) {
    theta <- rnorm(n = 1, mean = 0, sd = sigma)[1]
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
    } else{
      lengths <- append(lengths, M)
    }
  }
  pow <- sum(rejections)/B
  len <- sum(lengths)/(as.numeric(B*M))
  
  n_star <- power.t.test(n=NULL, 
               delta = mean(thetas), 
               sd = sqrt(2)/2, 
               type="two.sample",
               alternative = "two.sided",
               power = pow, 
               sig.level = alpha)
  
  return(Simulation(power = sum(rejections)/B, length = len, n_star = n_star$n/M))
}


