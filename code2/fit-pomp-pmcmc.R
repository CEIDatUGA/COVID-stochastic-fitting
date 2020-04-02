# fit-pomp-mcmc.R
# loads pomp object and performs fitting using pMCMC
# can be run in parallel (doing parallelization on multiple chains)

# Clear the decks ---------------------------------------------------------
rm(list = ls(all.names = TRUE))
graphics.off()

# Load libraries ----------------------------------------------------------
library(pomp)


# Specify if we want to parallelize or not ---------------------------------------------------------------
parallel_run = TRUE
#parallel_run = FALSE

# Turn on parallel or not ---------------------------------------------------------------
if (parallel_run == TRUE)
{
  library(doParallel)
  library(foreach)
  # Set up parallel structure ----------------------------------------------------
  n_cores = 3
  cl <- makeCluster(n_cores) 
  registerDoParallel(cl)
}

# Load the pomp object ----------------------------------------------------
filename = here('output2/pomp-model.RDS')
pomp_model <- readRDS(filename)


# Define the prior density ------------------------------------------------
# needs to be specified for each parameter that is being fit

#beta are assumed to be normally distributed
#note that in the model itself, they are used as exp(beta)
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

#parameters to be estimated
estpars <- c("beta_d", "beta_u",  "beta_e", "beta_red_factor", 
             "gamma_u", "gamma_d", "detect_frac_0", "theta") 

# Set noise level for parameter random walk for proposals
# this is needed for parameters that are being estimated
rw.sd <- c(beta_d = 0.05, beta_u = 0.05, beta_e = 0.05,
           beta_red_factor = 0.005, gamma_u = 0.1, gamma_d = 0.1,
           detect_frac_0 = 0.005, theta = 0.5)


#######################################################################
# Define values for parameters and initial conditions
# for all quantities 
# This includes parameters that are being fit and those that are not being fit
#######################################################################

# Initial values ----------------------------------------------------------

inivals <- c(S_0 = 10600000, 
             E1_0 = 35, E2_0 = 35, E3_0 = 35, E4_0 = 35, E5_0 =35, E6_0 =35,
             I1_0 = 14, I2_0 = 14, I3_0 = 14, I4_0 = 14, 
             Iu1_0 = 111, Iu2_0 = 111, Iu3_0 = 111, Iu4_0 = 111, 
             C_0 = 1,
             Ru_0 = 1)

Ntot <- sum(inivals)  # total population size

# Values for parameters
# beta is scaled by population size here instead of inside the process model
parvals <- c(beta_d = log(2/Ntot), 
             beta_u = log(0.25/Ntot), 
             beta_e = log(0.1/Ntot), 
             beta_red_factor = 0.5, 
             t_int1 = 12, t_int2 = 12, t_int3 = 12, 
             gamma_u = 4*0.1,
             gamma_d = 4*0.5, 
             detect_frac_0 = 0.1, 
             detect_frac_1 = 0.5,
             sigma = 6*0.18, 
             rho = 0.5, 
             theta = 100)


#######################################################################
#update the model with information specified above
#######################################################################
pomp_model <- pomp_model %>% pomp(
  dprior = prior_dens,
  paramnames = c("beta_d", "beta_u", "beta_e", "beta_red_factor", 
                 "gamma_u", "gamma_d", "detect_frac_0", "theta"),
  params=c(parvals,inivals)
)



# Run pMCMC ---------------------------------------------------------------
n_chains <- 3   # number of mcmc chains
n_MCMC_iter <- 100 #number of MCMC iterations
n_particles <- 100 #number of particles

#run the pMCMC algorithm sequential
if (parallel_run == FALSE)
{
  out_mcmc = list()
  for (i in 1:n_chains) 
    {
    out_mcmc[[i]] <- pmcmc(
      pomp_model,
      Nmcmc = n_MCMC_iter,
      Np = n_particles,
      proposal = mvn.diag.rw(rw.sd),
      verbose = TRUE
    )
  }
}

#run the pMCMC algorithm in parallel
if (parallel_run == TRUE)
{
  out_mcmc <- foreach(i=1:n_chains, .inorder = FALSE, .export = "pmcmc", .packages = c("pomp")) %dopar% 
    {
      pomp::pmcmc(
      pomp_model,
      Nmcmc = n_MCMC_iter,
      Np = n_particles,
      proposal = pomp::mvn.diag.rw(rw.sd),
      verbose = FALSE
    )
  }
  stopCluster(cl)
}

  
# save pMCMC fitting result 
saveRDS(object = out_mcmc, file = "../output2/pomp-pmcmc-object.RDS")

# plot results for 1 chain
chain <- as.data.frame(out_mcmc[[1]]@traces)
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

