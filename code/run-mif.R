# run-mif.R
# This script uses the pomp::mif2() function to estimate parameters
# of the stochastic SEIR model using maximization by iterated filtering.
# The data are new daily case counts, new daily hospitalizations, and
# new daily deaths. See the pomp_model object for details.
#
# ******************************************************
# NOTE: THIS CODE ASSUMES IT IS BEING SOURCED FROM THE
#       00-MASTER-RUN-ANALYSIS.R SCRIPT. YOU MUST SET
#       PARAMETER VALUES, ETC., IN THAT SCRIPT BEFORE
#       RUNNING THIS SCRIPT.
# ******************************************************

rm(list = ls(all.names = TRUE))


library(pomp)  # must be at least version 2.x
library(here)
library(doParallel)
library(foreach)

# turn on parallel running or not
parallel_run <- TRUE
num_cores <- parallel::detectCores() - 2  # alter as needed


# Turn on parallel or not --------------------------------------------------
if (parallel_run == TRUE) {
  # Set up parallel structure 
  n_cores <- num_cores
  cl <- makeCluster(num_cores) 
  registerDoParallel(cl)
} else {
  n_cores <- 1
}



# Load the pomp object ----------------------------------------------------
filename <- here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)

# load values for model parameters and initial conditions -----------------
filename <- here('output/parvals.RDS')
allparvals <- readRDS(filename)

# load list that contains names of model parameters and initial conditions that are estimated-----------------
filename <- here('output/estpars.RDS')
est_list <- readRDS(filename)
params_to_estimate = est_list$params_to_estimate
inivals_to_estimate = est_list$inivals_to_estimate

# Specify mif random walk intensity ---------------------------------
# set up the rw.sd structure, i.e. perturbations for parameters needed for mif
# assign perturbations following order of parameters above, 
# hard-coded is not a good idea, but ok for now

pert_par_vals <- c(rep(0.05,18), rep(0.1,3))  
pert_ini_vals <- c(rep(0.2,12), rep(0.1,4))

# make long string containing all parameter names and values
# in a form required by rw.sd
param_perts_string <- paste(params_to_estimate, 
                            '=',
                            pert_par_vals,
                            collapse = ', ')
ini_perts_string <- paste0(inivals_to_estimate, 
                           ' = ivp(',
                           pert_ini_vals, 
                           ")", 
                           collapse = ', ')

# this string is being fed into sw.rd inside mif 
# below in a way suggested by Aaron
perts_string <- paste0("rw.sd(",param_perts_string,", ",ini_perts_string,")")


# Define function that runs the whole mif --------------------------
# function that runs the whole mif, either in parallel or not
# does it in 2 parts. Variables set in MASTER script.

run_mif <- function(pomp_model, num_mif_iterations, params, num_particles, 
                    c_frac, param_perts, verbose) 
{
  out_mif <- pomp::mif2(pomp_model, # first part of mif run to converge
                        Nmif = num_mif_iterations[1], 
                        params = params, 
                        Np = num_particles[1], 
                        cooling.fraction.50 = c_frac[1], 
                        rw.sd = eval(parse(text=perts_string)),
                        cooling.type = "geometric",
                        verbose = verbose)
  
  out_mif <- out_mif %>% 
    pomp::continue( # 2nd part of mif run to 'polish off' things
      Nmif = num_mif_iterations[2],
      Np = num_particles[2], 
      cooling.fraction.50 = c_frac[2], 
      cooling.type = "geometric",
      verbose = verbose)
  
  return(out_mif)
}


# Compute different starting values for each run --------------------------
# number of initial conditions to try, set to number of cores or multiple 
# thereof for best performance
n_ini_cond <- 1*n_cores 

# set up matrix for starting values, each row is one set of initial conditions
param_start <- matrix(0,nrow = n_ini_cond, ncol = length(params_to_estimate)) 

# columns of matrix contain starting values for parameters to be estimated
colnames(param_start) <- params_to_estimate 

# fill matrix with starting values drawn from normal distribution
for (i in 1:nrow(param_start)) {
  param_start[i,] = rnorm(length(params_to_estimate), 
                          allparvals[params_to_estimate], 
                          sd = 1) 
} 

# the mif2 routine needs numeric/starting conditions for 
# both fixed and variable parameters
fixed_params <- allparvals[!(names(allparvals) %in% params_to_estimate)] 

# specify settings for mif2 procedure
#mif_num_particles <- c(2000, 2000)  # two rounds of MIF
#mif_num_iterations <- c(100, 50)  # two rounds of MIF
mif_cooling_fracs <- c(0.9, 0.75)  # two rounds of MIF

mif_num_particles <- c(200, 200)  # two rounds of MIF
mif_num_iterations <- c(10, 10)  # two rounds of MIF


# For particle filter log likelihood estimation of MIF MLEs
pf_num_particles <- 2000
pf_reps <- 10


# Run MIF from different starting points ----------------------------------
# Run MIF not-parallel
if (parallel_run == FALSE)
{
  print('Starting MIF2 + pfilter non-parallel')
  out_mif = list()
  ll = list()
  for (i in 1:n_ini_cond) 
  {
    out_mif[[i]] <- run_mif(pomp_model,  
                            num_mif_iterations = mif_num_iterations, 
                            params = c(param_start[i,],fixed_params), 
                            num_particles = mif_num_particles, 
                            c_frac = mif_cooling_fracs, param_perts,
                            verbose = FALSE
                            )

    # Use particle filter to get the likelihood at the end of each MIF run
    pf <- replicate(n = pf_reps, pfilter(out_mif[[i]], 
                                         Np = pf_num_particles, 
                                         max.fail = Inf))
    ll1 <- sapply(pf, logLik)
    ll[[i]] <- logmeanexp(ll1, se = TRUE)
    }
} # end code section that does mif followed by pfilter for non-parallel setup

# Run MIF parallel
if (parallel_run == TRUE)
{
  print('Starting MIF2 + pfilter parallel')
  out_mif <- foreach(i=1:n_ini_cond, .packages = c("pomp")) %dopar% 
    {
      run_mif(pomp_model, num_mif_iterations = mif_num_iterations, 
              params = c(param_start[i,],fixed_params), 
              num_particles = mif_num_particles, 
              c_frac = mif_cooling_fracs, param_perts,
              verbose = FALSE
              )
    } #end dopar/foreach to compute mif

  # Use particle filter to get the likelihood at the end of MIF run
  ll <- foreach(mf = out_mif, .packages = c("pomp")) %dopar% {
    pf <- replicate(n = pf_reps, pfilter(mf, Np = pf_num_particles, max.fail = Inf))
    ll1 <- sapply(pf, logLik)
    ll1 <- logmeanexp(ll1, se = TRUE)
  }
  stopCluster(cl)
} # end code section that does mif followed by pfilter for parallel setup



# Summarize results -------------------------------------------------------
# get estimated values for all parameters that were estimated for each run 
mif_coefs <- data.frame(matrix(unlist(sapply(out_mif, coef)), 
                               nrow = length(out_mif), 
                               byrow = T))
colnames(mif_coefs) <- names(coef(out_mif[[1]]))  # names are the same for all mifs

# convert the list containing the log likelihoods for 
# each run stored in ll into a data frame
ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T))

# combine the ll_df and mif_coefs data frames. 
# Also do some cleaning/renaming
pf_logliks <- ll_df %>%
  dplyr::rename("LogLik" = X1,
         "LogLik_SE" = X2) %>%
  dplyr::mutate(MIF_ID = 1:n()) %>%
  dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
  bind_cols(mif_coefs) %>%
  dplyr::arrange(-LogLik)


# Save output -------------------------------------------------------------
# create a list of lists
mifRets <- list(mif_objects = out_mif, loglik_dfs = pf_logliks)
outfile <- here('output/mif-results.RDS')
saveRDS(object = mifRets, file = outfile)

