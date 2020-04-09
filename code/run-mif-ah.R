# run-mif.R
# Fits data using particle filtering
# can be used in parallel or not

# Clear the decks ---------------------------------------------------------
rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(dplyr)
library(pomp)
library(doParallel)
library(foreach)
library(here)

# Specify if we want to parallelize or not ---------------------------------------------------------------
parallel_run = TRUE
#parallel_run = FALSE

n_cores = 1 #if not parallel, this should be 1, otherwise for parallel specified below

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
filename <- here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)

# load values for model parameters and initial conditions ----------------------------------------------------
filename = here('output/parvals.RDS')
allparvals <- readRDS(filename)

# Specify the parameters we want to estimate (i.e., those to vary) --------------------
# is a subset of all parameters. Can also include initial conditions.
params_to_estimate <- c(
                        "log_beta_s", #rate of infection of symptomatic 
                        "trans_e", "trans_a", "trans_c", "trans_h", #parameter that determines relative infectiousness of E/Ia/C classes compared to Isu/Isd 
                        "beta_reduce",  #overall transmission reduction due to social distancing
                        "log_g_e", "log_g_a", "log_g_su", "log_g_sd", "log_g_c","log_g_h", #rate of movement through E/Ia/Isu/Isd/C/H compartments,
                        "log_diag_speedup", #factor by which movement through Isd happens faster (quicker diagnosis) 
                        "detect_0","detect_1", #determines fraction that get diagnosed before and after intervention
                        "frac_asym", #fraction asymptomatic
                        "frac_hosp", #fraction diagnosed that go into hospital
                        "frac_dead", #fraction hospitalized that die
                        "log_theta_cases","log_theta_hosps","log_theta_deaths",
                        "E1_0", "E2_0", "E3_0", "E4_0",  
                        "Ia1_0", "Ia2_0", "Ia3_0", "Ia4_0", 
                        "Isu1_0", "Isu2_0", "Isu3_0", "Isu4_0", 
                        "Isd1_0", "Isd2_0", "Isd3_0", "Isd4_0" 
                      )

# ------- parts below not working, goal is to replace the manual rw.sd() specification
#assign perturbations to parameters to be estimated, needed for estimation procedure
#in order of parameters above, hard-coded is not a good idea, but ok for now
pert_vals = c(rep(0.05,18),rep(0.1,3),rep(0.2,12),rep(0.1,4))
param_perts_string = paste(params_to_estimate,'=',pert_vals,collapse=', ')
#not working
#params_perts_2 <- rw.sd(param_perts_string)
# ------- end non-working parts

# what's the ivp() command doing? if possible, trying to replace by an easier way to supply that, see code snippets above.
params_perts <- rw.sd(log_beta_s = 0.05,
                      trans_e = 0.05,
                      trans_a = 0.05,
                      trans_c = 0.05,
                      trans_h = 0.05,
                      beta_reduce = 0.05,
                      log_g_e = 0.05,
                      log_g_a = 0.05,
                      log_g_su = 0.05,
                      log_g_sd = 0.05,
                      log_g_c = 0.05,
                      log_g_h = 0.05,
                      log_diag_speedup = 0.05,
                      detect_0 = 0.05,
                      detect_1 = 0.05,
                      frac_asym = 0.05,
                      frac_hosp = 0.05,
                      frac_dead = 0.05,
                      log_theta_cases = 0.1,
                      log_theta_hosps = 0.1,
                      log_theta_deaths = 0.1,
                      E1_0 = ivp(0.2),
                      E2_0 = ivp(0.2),
                      E3_0 = ivp(0.2),
                      E4_0 = ivp(0.2),
                      Ia1_0 = ivp(0.2),
                      Ia2_0 = ivp(0.2),
                      Ia3_0 = ivp(0.2),
                      Ia4_0 = ivp(0.2),
                      Isu1_0 = ivp(0.2),
                      Isu2_0 = ivp(0.2),
                      Isu3_0 = ivp(0.2),
                      Isu4_0 = ivp(0.2),
                      Isd1_0 = ivp(0.1),
                      Isd2_0 = ivp(0.1),
                      Isd3_0 = ivp(0.1),
                      Isd4_0 = ivp(0.1)
                      )

# Define function that runs the whole mif --------------------------
#function that runs the whole mif, either in parallel or not
#does it in 2 parts. Variables below contain settings for parts 1 and 2
num_particles <- c(200,200) #number of particles
num_mif_iterations <- c(10,20) #number of iterations
c_frac <- c(0.8,0.65) #cooling fraction
run_mif <- function(pomp_model, Nmif, params, num_particles, c_frac, param_perts, verbose) 
{
  out_mif <- pomp::mif2(pomp_model, #first part of mif run to converge
                        Nmif = num_mif_iterations[1], 
                        params = params, 
                        Np = num_particles[1], 
                        cooling.fraction.50 = c_frac[1], 
                        rw.sd = params_perts,
                        cooling.type = "geometric",
                        verbose = verbose
  )
  
  out_mif <- out_mif %>% pomp::continue( #2nd part of mif run to 'polish off' things
                              Nmif = num_mif_iterations[2],
                              Np = num_particles[2], 
                              cooling.fraction.50 = c_frac[2], 
                              cooling.type = "geometric",
                              verbose = verbose
  )
  return(out_mif)
}


# Compute different starting values for each run --------------------------
n_ini_cond = 3*n_cores #number of initial conditions to try, set to number of cores or multiple
param_start = matrix(0,nrow = n_ini_cond, ncol = length(params_to_estimate)) #set up matrix for starting values
colnames(param_start) = params_to_estimate
for (i in 1:nrow(param_start)) {param_start[i,] = rnorm(length(params_to_estimate), allparvals[params_to_estimate], sd = 1) }

fixed_params = allparvals[!(names(allparvals) %in% params_to_estimate)] #the mif2 routine needs numeric/starting conditions for both fixed and variable parameters

# Run MIF from different starting points ----------------------------------
if (parallel_run == FALSE)
{
  out_mif = list()
  ll = list()
  for (i in 1:n_ini_cond) 
  {
    out_mif[[i]] <- run_mif(pomp_model, Nmif = num_mif_iterations, 
                               params = c(param_start[i,],fixed_params), num_particles, 
                               c_frac, param_perts,
                               verbose = FALSE
                            )

    # Use particle filter to get the likelihood at the end of each MIF run ---------
    pf <- replicate(n = 10, pfilter(out_mif[[i]], Np = 500, max.fail = Inf))
    ll1 <- sapply(pf, logLik)
    ll[[i]] <- logmeanexp(ll1, se = TRUE)
    }
} #end code section that does mif followed by particle filter for non-parallel setup

#run the pMCMC algorithm in parallel
if (parallel_run == TRUE)
{
  out_mif <- foreach(i=1:n_ini_cond, .packages = c("pomp")) %dopar% 
    {
      run_mif(pomp_model, Nmif = num_mif_iterations, 
                              params = c(param_start[i,],fixed_params), num_particles, 
                              c_frac, param_perts,
                              verbose = FALSE
              )
    } #end dopar/foreach to compute mif

  # Use particle filter to get the likelihood at the end of MIF run ---------
  ll <- foreach(mf = out_mif, .packages = c("pomp")) %dopar% {
    pf <- replicate(n = 10, pfilter(mf, Np = 500, max.fail = Inf))
    ll1 <- sapply(pf, logLik)
    ll1 <- logmeanexp(ll1, se = TRUE)
  }
  stopCluster(cl)
} #end code section that does mif followed by particle filter for parallel setup


mif_coefs <- data.frame(matrix(unlist(sapply(out_mif, coef)), nrow = length(out_mif), byrow = T))
colnames(mif_coefs) <- names(coef(out_mif[[1]]))

#conver the list containing the log likelihoods for each run stored in ll into a data frame
ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T))

#combine the ll_df and mif_coefs data frames. Also do some cleaning/renaming
pf_logliks <- ll_df %>%
  dplyr::rename("LogLik" = X1,
         "LogLik_SE" = X2) %>%
  dplyr::mutate(MIF_ID = 1:n()) %>%
  dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
  bind_cols(mif_coefs) %>%
  dplyr::arrange(-LogLik)


# Save output -------------------------------------------------------------
#create a list of lists
mifRets <- list(mif_objects = out_mif, loglik_dfs = pf_logliks)
outfile <- here('output/mif-results.RDS')
saveRDS(object = mifRets, file = outfile)




# Cache -------------------------------------------------------------------

# mifs %>%
#   traces() %>%
#   melt() %>%
#   filter(variable %in% c("loglik", params_to_estimate)) %>%
#   # filter(iteration > 100) %>%
#   ggplot(aes(x=iteration,y=value,group=L1,color=as.factor(L1)))+
#   geom_line()+
#   facet_wrap(~variable,scales="free_y")+
#   scale_color_brewer(type = "qual") +
#   guides(color=FALSE)
# 
# sims <- pomp::simulate(mifs[[6]],
#                        nsim=1, format="data.frame",
#                        include.data=TRUE)
# 
# 
# sims %>%
#   dplyr::select(time, .id, cases, hosps, deaths) %>%
#   tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
#   ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y") +
#   guides(color = FALSE)
# 
# sims %>%
#   dplyr::select(time, .id, H1, hosps) %>%
#   tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
#   ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y") +
#   guides(color = FALSE)

# 
# 

# Extract and save best parameter set for MCMC ----------------------------
# 
# mf1 <- mifs[[which.max(pf1)]]
# theta_mif <- coef(mf1)
# saveRDS("../output/mif-mles.RDS")





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

