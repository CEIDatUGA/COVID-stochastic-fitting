# run-mif.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)


# Load the pomp object ----------------------------------------------------

covid_ga_pomp <- readRDS("../output/covid-ga-pomp-object.RDS")




theta.guess <- theta.true <- coef(covid_ga_pomp)
estpars <- c("beta_d", "beta_u",  "beta_e", "beta_red_factor", "gamma_u", "gamma_d", "detect_frac_0")  #parameters to estimate

test <- mif2(covid_ga_pomp, Nmif = 500, params = theta.guess, 
             Np = 2000, cooling.fraction = 0.5,
             rw.sd = rw.sd(beta_d = 0.02, beta_u = 0.02, beta_e = 0.02, 
                           beta_red_factor = 0.02, gamma_u = 0.02,
                           gamma_d = 0.02, detect_frac_0 = 0.02))


foreach (i = 1:10, .combine = c) %dopar% {   #Inspect from multiple, randomly chosen starting points
  theta.guess <- theta.true
  theta.guess[estpars] <- rlnorm(n = length(estpars),
                                 meanlog = log(theta.guess[estpars]), sdlog = 1)
  
} -> mifs

# Use particle filter to get the likelihood at the end of MIF run
pf1 <- foreach(mf = mifs, .combine = c) %dopar% {
  pf <- replicate(n = 10, logLik(pfilter(mf, Np = 10000)))
  logmeanexp(pf)
}

# Pick the best parameter set
mf1 <- mifs[[which.max(pf1)]]
theta.mif <- coef(mf1)

