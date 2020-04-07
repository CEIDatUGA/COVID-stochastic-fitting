# run-particle-mcmc.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)
library(here)
library(doParallel)
library(foreach)


# Load the pomp object and mif results ------------------------------------

mif_results <- readRDS(here('output/mif-results.RDS'))


# Extract MLEs as starting values -----------------------------------------

# Get the parameter set with the highest log likelihood and then
# pull those parameter estimates into a named vector
mle_inits <- mif_results$loglik_dfs %>%
  filter(LogLik == max(LogLik)) %>%  # get MLEs
  pull(MIF_ID)

mif_init <- mif_results$mif_objects[[mle_inits]]



# Define the prior density ------------------------------------------------

prior_dens <- Csnippet(
  "
  lik = dnorm(log_beta_s, -18.8975, 0.2, 1) +
        dunif(trans_e, -5, 5, 1) +
        dunif(trans_a, -5, 5, 1) +
        dunif(trans_c, -15, 15, 1) +
        dnorm(beta_reduce, -0.6190392, 0.5, 1) +
        dnorm(log_g_e, -0.2231436, 0.05, 1) +
        dunif(log_g_a, -5, 5, 1) +
        dunif(log_g_su, -5, 5, 1) +
        dnorm(log_g_c, 5.764807, 1.157302, 1) +
        dunif(log_g_h, -5, 5, 1) +
        dnorm(log_diag_speedup, 0.6931472, 0.5, 1) +
        dunif(detect_0, -5, 5, 1) +
        dunif(detect_1, -5, 5, 1) +
        dunif(theta, 80, 120, 1) +
        dunif(theta_hosp, 80, 120, 1) +
        dunif(theta_death, 80, 120, 1);
  
  if (!give_log) lik = exp(lik);
  "
)


# Run pMCMC ---------------------------------------------------------------

params_to_estimate <- names(coef(mif_init))
rmones <- which(params_to_estimate %in% c("t_int1", "t_int2", "t_int3", "rho",
                                          "frac_asym", "frac_hosp", "frac_dead"))
params_to_estimate <- params_to_estimate[-rmones]
rmtwos <- grep("_0", params_to_estimate)
params_to_estimate <- params_to_estimate[-rmtwos]
params_to_estimate <- c(params_to_estimate, "detect_0")

# Set noise level for parameter random walk for proposals
rw.sd <- rep(0.02, length(params_to_estimate))
names(rw.sd) <- params_to_estimate

pomp_for_mcmc <- pomp(
  mif_init,
  dprior = prior_dens,
  paramnames = params_to_estimate,
  cdir = getwd()  # just to fix a Windows error when compiling...
)

num_mcmc <- 2000

num_cores <- parallel::detectCores() - 2  # alter as needed
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)

foreach(i = 1:num_cores, .combine = c, .packages = c("pomp"), 
        .export = c("prior_dens", "params_to_estimate", "rw.sd", 
                    "pomp_for_mcmc", "num_mcmc")) %dopar% {
  pomp::pmcmc(
    pomp_for_mcmc,
    Nmcmc = num_mcmc,
    Np = 2000,
    proposal = pomp::mvn.diag.rw(rw.sd)
  )
} -> out_mcmc

stopCluster(cl)



# Save the output ---------------------------------------------------------

outfile <- here("output/pcmcm-output.RDS")
saveRDS(out_mcmc, outfile)








# Cache -------------------------------------------------------------------


# mcmcmat <- matrix(ncol = num_mcmc+1, nrow = length(out_mcmc))
# for(i in 1:length(out_mcmc)) {
#   mcmcmat[i, ] <- as.data.frame(out_mcmc[[i]]@traces)$log_beta_s
# }
# 
# matplot(t(exp(mcmcmat)*10600000), type = "l")



# saveRDS(object = out_mcmc, file = "../output/pomp-pmcmc-object.RDS")
# 
# chain <- as.data.frame(out_mcmc@traces)
# 
# par(mfrow = c(1,1))
# plot(exp(chain$log_beta_s)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[d]))
# plot(exp(chain$beta_u)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[u]))
# plot(exp(chain$beta_e)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[e]))
# plot(chain$beta_red_factor, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(xi))
# plot(chain$gamma_d, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(gamma[d]))
# plot(chain$gamma_u, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(gamma[u]))
# plot(chain$detect_frac_0, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = "detect_frac_0")
# plot(chain$theta, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(theta))
# 
