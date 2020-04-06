# run-particle-mcmc.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)


# Load the pomp object ----------------------------------------------------

covid_ga_pomp <- readRDS("../output/pomp-model.RDS")


# Define the prior density ------------------------------------------------

prior_dens <- Csnippet(
  "
  lik = dnorm(beta_d,log(2e-7), 0.4, 1) +
    dnorm(beta_u, log(5e-8), 0.2, 1) +
    dnorm(beta_e, log(5e-8), 0.2, 1) +
    dunif(beta_red_factor, 0.01, 1, 1) +
    dlnorm(gamma_u, log(0.5), 0.2, 1) +
    dlnorm(gamma_u, log(0.5), 0.2, 1) +
    dunif(detect_frac_0, 0.01, 0.6, 1);
  if (!give_log) lik = exp(lik);
"
)


# Run pMCMC ---------------------------------------------------------------

n <- 1   # number of mcmc chains

estpars <- c("beta_d", "beta_u",  "beta_e", "beta_red_factor", 
             "gamma_u", "gamma_d", "detect_frac_0", "theta") 

# Set noise level for parameter random walk for proposals
rw.sd <- rep(0.1, length(estpars))
names(rw.sd) <- estpars
rw.sd <- c(beta_d = 0.05, beta_u = 0.05, beta_e = 0.05,
           beta_red_factor = 0.005, gamma_u = 0.1, gamma_d = 0.1,
           detect_frac_0 = 0.005, theta = 0.5)

out_mcmc <- pmcmc(
  pomp(
    covid_ga_pomp,
    dprior = prior_dens,
    paramnames = c("beta_d", "beta_u", "beta_e", "beta_red_factor", 
                   "gamma_u", "gamma_d", "detect_frac_0", "theta")
  ),
  Nmcmc = 2000,
  Np = 2000,
  proposal = mvn.diag.rw(rw.sd),
  verbose = TRUE
)

saveRDS(object = out_mcmc, file = "../output/pomp-pmcmc-object.RDS")

chain <- as.data.frame(out_mcmc@traces)[1001:2000,]

par(mfrow = c(4,2))
plot(exp(chain$beta_d)*10600000, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = expression(beta[d]))
plot(exp(chain$beta_u)*10600000, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = expression(beta[u]))
plot(exp(chain$beta_e)*10600000, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = expression(beta[e]))
plot(chain$beta_red_factor, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = expression(xi))
plot(chain$gamma_d, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = expression(gamma[d]))
plot(chain$gamma_u, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = expression(gamma[u]))
plot(chain$detect_frac_0, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = "detect_frac_0")
plot(chain$theta, type = "l", bty = "n",
     xlab = "MCMC iteration", ylab = expression(theta))

