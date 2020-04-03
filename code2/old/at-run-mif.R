# run-mif.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)
library(doParallel)


# Load the pomp object ----------------------------------------------------

pomp_object <- readRDS("../output/covid-ga-pomp-object.RDS")
pomp_object <- readRDS("../output2/pomp-model.RDS")


# Set the parameters to estimate (i.e., those to vary) --------------------

# We have to fix several parameters. E.g. it's impossible to estimate 
# all beta and the reduction factor, they are fully collinear. So, we
# fix all the betas here.

params_to_estimate <- c("beta_d", "beta_u",  "beta_e", "beta_red_factor", 
                        "gamma_u", "gamma_d", "detect_frac_0")

params_perts <- rw.sd(beta_d = 0,  # change to let it vary 
                      beta_u = 0,  # change to let it vary
                      beta_e = 0,  # change to let it vary
                      beta_red_factor = 0.02,
                      gamma_u = 0.02,
                      gamma_d = 0.02, 
                      detect_frac_0 = 0.02)

curr_theta <- coef(pomp_object)


# Define "proposal" function for starting values --------------------------

prop_func <- function(theta) {
  betas <- theta[c("beta_d", "beta_u", "beta_e")]
  one <- rnorm(n = length(betas), mean = betas, sd = 0)  # update sd if desired
  others <- theta[-(which(names(theta) %in% names(betas)))]
  two <- rlnorm(n = (length(others)), 
                meanlog = log(others), 
                sdlog = 1)
  out <- c(one, two)
  names(out) <- names(theta)
  return(out)
}


# Run MIF from different starting points ----------------------------------

num_particles <- 2000
num_mif_iterations <- 50
#num_cores <- parallel::detectCores() - 1  # alter as needed
num_cores <- 1  # alter as needed
foreach (i = 1:num_cores, 
         .combine = c, 
         .export = c("params_perts", 
                     "prop_func", 
                     "curr_theta")) %dopar% {
  print(sprintf('starting mif number %d',i))
 theta_guess <- curr_theta
  theta_guess[params_to_estimate] <- prop_func(curr_theta[params_to_estimate])
  mif2(pomp_object, Nmif = num_mif_iterations, params = theta_guess, 
       Np = num_particles, cooling.fraction = 0.5, rw.sd = params_perts)
} -> mifs

mifs %>%
  traces() %>%
  melt() %>%
  filter(variable %in% c("loglik", params_to_estimate)) %>%
  ggplot(aes(x=iteration,y=value,group=L1,color=L1))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  guides(color=FALSE)


# Use particle filter to get the likelihood at the end of MIF run ---------

pf1 <- foreach(mf = mifs, .combine = c) %dopar% {
  pf <- replicate(n = 10, logLik(pfilter(mf, Np = 10000)))
  logmeanexp(pf)
}


# Extract and save best parameter set for MCMC ----------------------------

mf1 <- mifs[[which.max(pf1)]]
theta_mif <- coef(mf1)
saveRDS("../output/mif-mles.RDS")





# Cache -------------------------------------------------------------------

# 
# # Question: Are there rules of thumb for specifying Nmif, Np, coooling.fraction and rw.sd? Or ways to diagnose if one is choosing them right?
# # Other question: Is this only estimating those parameters that are specified in rw.sd and all others are assumed fixed?
# 
# # pf <- pfilter(covid_ga_pomp, params = coef(covid_ga_pomp), Np = 1000)
# 
# test <- mif2(pomp_object, Nmif = 50, params = theta.guess, 
#              Np = 2000, cooling.fraction = 1,
#              rw.sd = rw.sd(beta_red_factor = 0.02, gamma_u = 0.02,
#                            gamma_d = 0.02, detect_frac_0 = 0.02))
# 
# 
# mifs <- foreach (i = 1:10, .combine = c) %dopar% {   #Inspect from multiple, randomly chosen starting points
#         theta.guess <- theta.true
#         theta.guess[estpars] <- rlnorm(n = length(estpars),
#                                  meanlog = log(theta.guess[estpars]), sdlog = 1)
#         } 
# 
# 
# 

# 
