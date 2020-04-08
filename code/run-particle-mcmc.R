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
  lik = dnorm(log_beta_s, -17.56266, 0.5, 1) +
        dunif(trans_e, -5, 5, 1) +
        dunif(trans_a, -5, 5, 1) +
        dunif(trans_c, -15, 15, 1) +
        dunif(beta_reduce, -5, 5, 1) +
        dnorm(log_g_e, -0.2231436, 0.05, 1) +
        dunif(log_g_a, -5, 5, 1) +
        dunif(log_g_su, -5, 5, 1) +
        dnorm(log_g_c, 5.764807, 1.157302, 1) +
        dunif(log_g_h, -5, 5, 1) +
        dnorm(log_diag_speedup, 0.6931472, 0.5, 1) +
        dunif(detect_0, -5, 5, 1) +
        dunif(detect_1, -5, 5, 1) +
        dunif(log_theta_cases, -6, 6, 1) +
        dunif(log_theta_hosps, -6, 6, 1) +
        dunif(log_theta_deaths, -6, 6, 1) +
        dnorm(E1_0, 40, 5, 1) +
        dnorm(E2_0, 40, 5, 1) +
        dnorm(E3_0, 40, 5, 1) +
        dnorm(E4_0, 40, 5, 1) +
        dnorm(Ia1_0, 22, 4, 1) +
        dnorm(Ia2_0, 22, 4, 1) +
        dnorm(Ia3_0, 22, 4, 1) +
        dnorm(Ia4_0, 22, 4, 1) +
        dnorm(Isu1_0, 90, 7, 1) +
        dnorm(Isu2_0, 90, 7, 1) +
        dnorm(Isu3_0, 90, 7, 1) +
        dnorm(Isu4_0, 90, 7, 1) +
        dnorm(Isd1_0, 14, 3, 1) +
        dnorm(Isd2_0, 14, 3, 1) +
        dnorm(Isd3_0, 14, 3, 1) +
        dnorm(Isd4_0, 14, 3, 1);
  
  if (!give_log) lik = exp(lik);
  "
)


# Run pMCMC ---------------------------------------------------------------

params_to_estimate <- names(coef(mif_init))
rmones <- which(params_to_estimate %in% c("t_int1", "t_int2", "t_int3", "S_0",
                                          "C1_0", "C2_0", "C3_0", "C4_0",
                                          "H1_0", "H2_0", "H3_0", "H4_0",
                                          "R_0", "D_0"))
params_to_estimate <- params_to_estimate[-rmones]


# Set noise level for parameter random walk for proposals
rw.sd <- rep(0.075, length(params_to_estimate))
names(rw.sd) <- params_to_estimate

# Forecast horizon, days
horizon <- 7 * 4
newtimes <- c(time(mif_init), max(time(mif_init)) + seq_len(horizon))
newdata <- t(mif_init@data) %>%
  as.data.frame() %>%
  bind_rows(
    data.frame(cases = rep(NA, horizon),
               hosps = rep(NA, horizon),
               deaths = rep(NA, horizon)) 
  ) %>%
  t()

mif_init@data <- newdata
mif_init@times <- newtimes

pomp_for_mcmc <- pomp(
  mif_init,
  dprior = prior_dens,
  paramnames = params_to_estimate,
  cdir = getwd()  # just to fix a Windows error when compiling...
)

num_mcmc <- 50

num_cores <- parallel::detectCores() - 2  # alter as needed
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)

foreach(i = 1:num_cores, .combine = c, .packages = c("pomp"),
        .export = c("prior_dens", "params_to_estimate", "rw.sd",
                    "pomp_for_mcmc", "num_mcmc")) %dopar% {
  pomp::pmcmc(
    pomp_for_mcmc,
    Nmcmc = num_mcmc,
    Np = 200,
    proposal = pomp::mvn.diag.rw(rw.sd)
  ) -> test
} -> out_mcmc

stopCluster(cl)

pfilter(out_mcmc[[1]], Np = 1000) -> test

# Save the output ---------------------------------------------------------

outfile <- here("output/pcmcm-output.RDS")
saveRDS(out_mcmc, outfile)


theta <- exp(test@traces[, "log_theta_hosps"])
hstates <- t(test@filter.traj["H_new",,]) %>%
  as.data.frame() %>%
  melt()
hstates$hosps <- rnbinom(n = nrow(hstates), size = theta, mu = hstates$value)
hstates$time = rep(c(0,newtimes), times = num_mcmc)
hstates <- hstates %>%
  mutate(period = ifelse(time < 40, "calibration", "forecast"))

ggplot(hstates, aes(x = time, y = value, group = variable)) +
  geom_line(aes(color = period)) +
  xlab("Time since March 1") +
  ylab("Number of new hospitalizations")




# Cache -------------------------------------------------------------------


mcmcmat <- matrix(ncol = num_mcmc+1, nrow = length(out_mcmc))
for(i in 1:length(out_mcmc)) {
  mcmcmat[i, ] <- as.data.frame(out_mcmc[[i]]@traces)$log_beta_s
}

matplot(t(exp(mcmcmat)*10600000), type = "l")



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
