# run-mif.R
# loads pomp object and performs fitting using MIF
# can be run in parallel (doing parallelization on multiple chains)

# Clear the decks ---------------------------------------------------------
rm(list = ls(all.names = TRUE))
graphics.off()

# Load libraries ----------------------------------------------------------
library(pomp)
library(here) #for setting paths
library(lhs) #for latin hypercube sampling of initial conditions

# Specify if we want to parallelize or not ---------------------------------------------------------------
#parallel_run = TRUE
parallel_run = FALSE

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

# We have to fix several parameters. E.g. it's impossible to estimate 
# all beta and the reduction factor, they are fully colinear. 

#perturbations for parameters, needed for estimation
#only parameters listed here are estimated
params_to_estimate <- c("log_beta_d", "log_beta_u",  "log_beta_e", "beta_reduce", 
                        "log_gamma_u", "log_gamma_d", "detect_0")

params_perts <- rw.sd(log_beta_d = 0,  # change to let it vary 
                      log_beta_u = 0,  # change to let it vary
                      log_beta_e = 0,  # change to let it vary
                      beta_reduce = 0.02,
                      log_gamma_u = 0.02,
                      log_gamma_d = 0.02, 
                      detect_0 = 0.02)


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
parvals <- c(log_beta_d = log(0.75/Ntot), 
             log_beta_u = log(0.5/Ntot), 
             log_beta_e = log(0.1/Ntot), 
             beta_reduce = 0.1, 
             t_int1 = 12, t_int2 = 12, t_int3 = 12, 
             log_gamma_u = log(4*0.2),
             log_gamma_d = log(4*0.3), 
             detect_0 = 1, 
             detect_1 = 1.5,
             log_sigma = log(6*0.18), 
             rho = 0.5, 
             theta = 100)


#######################################################################
#update the model with information specified above
#######################################################################
pomp_model <- pomp_model %>% pomp(
  params=c(parvals,inivals)
)

curr_theta <- coef(pomp_model)


# Define "proposal" function for starting values --------------------------

prop_func <- function(theta) {
  betas <- theta[c("log_beta_d", "log_beta_u", "log_beta_e")]
  one <- rnorm(n = length(betas), mean = betas, sd = 0)  # update sd if desired
  others <- theta[-(which(names(theta) %in% names(betas)))]
  two <- rlnorm(n = (length(others)), 
                meanlog = log(others), 
                sdlog = 1)
  out <- c(one, two)
  names(out) <- names(theta)
  return(out)
}




# Run multiple MIF from different starting points ----------------------------------

num_particles <- 100
num_mif_iterations <- 50
n_ini_cond = 2

#initial conditions for parameters for each MIF run
rngseed = 100 #random number seed for starting value sampling
set.seed(rngseed)
param_pert = params_to_estimate
lhssample=lhs::randomLHS(n_ini_cond,length(param_pert))
param_start = matrix(rep(parvals[(param_pert)],n_ini_cond),nrow = n_ini_cond, byrow = TRUE)
param_start = param_start * lhssample #currently not the best way of choosing initial conditions
colnames(param_start) = (param_pert)


foreach (i = 1:n_ini_cond, 
                  .export = c("params_perts", "prop_func", "curr_theta")) %dopar% 
                      {
                       print(sprintf('starting mif number %d',i))
                       theta_guess <- curr_theta
                       theta_guess[params_to_estimate] <- prop_func(curr_theta[params_to_estimate])
                       
                       mifs <- mif2(pomp_model, 
                                    Nmif = num_mif_iterations, 
                                    params = theta_guess, 
                                    Np = num_particles,
                                    cooling.fraction = 0.5, 
                                    rw.sd = params_perts)
                     } 


browser()




#run the pMCMC algorithm sequential
if (parallel_run == FALSE)
{
  out_mif = list()
  for (i in 1:n_ini_cond) 
  {
    out_mif[[i]] <- pomp::mif2(pomp_model, 
                               Nmif = num_mif_iterations, 
                               params = param_start[i,], 
                               Np = num_particles, 
                               cooling.fraction = 0.5, 
                               rw.sd = param_rw,
                               verbose = TRUE
                              )
      
  }
}

browser()

#run the pMCMC algorithm in parallel
if (parallel_run == TRUE)
{
  out_mif <- foreach(i=1:n_chains, .inorder = FALSE, 
                                   .export = c("params_perts", "prop_func", "curr_theta"), 
                                    .packages = c("pomp")) %dopar% 
    {
      theta_guess <- curr_theta
      theta_guess[params_to_estimate] <- prop_func(curr_theta[params_to_estimate])
      out_mif[[i]] <- pomp::mif2(pomp_model, 
                                 Nmif = num_mif_iterations, 
                                 params = theta_guess, 
                                 Np = num_particles, 
                                 cooling.fraction = 0.5, rw.sd = params_perts,
                                 verbose = TRUE
      )
    }
  stopCluster(cl)
}


unlist(out_mif) %>%
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



