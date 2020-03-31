# run-particle-mcmc.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)


# Load the pomp object ----------------------------------------------------

covid_ga_pomp <- readRDS("../output/covid-ga-pomp-object.RDS")


# Define the prior density ------------------------------------------------

prior_dens <- Csnippet(
  "
  lik = dlnorm(beta_d,log(2e-6),1,1) +
    dlnorm(beta_u,log(5e-8),1,1) +
    dlnorm(beta_e,log(5e-8),1,1) +
    dunif(beta_red_factor,0.01,1,1) +
    dlnorm(gamma_u, log(0.5), 1, 1) +
    dlnorm(gamma_u, log(0.5), 1, 1) +
    dunif(detect_frac_0, 0.01, 0.6, 1);
  if (!give_log) lik = exp(lik);
"
)


# Run pMCMC ---------------------------------------------------------------

n <- 1   # number of mcmc chains

estpars <- c("beta_d", "beta_u",  "beta_e", "beta_red_factor", 
             "gamma_u", "gamma_d", "detect_frac_0") 

# Set noise level for parameter random walk for proposals
rw.sd <- rep(0.0000000001, length(estpars))
names(rw.sd) <- estpars

out_mcmc <- pmcmc(
  pomp(
    covid_ga_pomp,
    dprior = prior_dens,
    paramnames = c("beta_d", "beta_u", "beta_e", "beta_red_factor", 
                   "gamma_u", "gamma_d", "detect_frac_0")
  ),
  Nmcmc = 5,
  Np = 100,
  proposal = mvn.diag.rw(rw.sd),
  verbose = TRUE
)

