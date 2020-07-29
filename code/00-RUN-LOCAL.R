# Script to run the workflow locally for trouble-shooting

# --------------------------------------------------
# Load necessary libraries 
# --------------------------------------------------
# could be loaded here or inside functions
library('lubridate') #needs to loaded first so it doesn't mess with 'here'
library('readr')
library('doParallel')
library('foreach')
library('purrr')
library('ggplot2')
library('pomp')
library('dplyr')
library('tidyr')
library('here')
library('vctrs')

# Start with a clean workspace to avoid downstream errors -----------------
rm(list = ls(all.names = TRUE))


# --------------------------------------------------
# Source all needed functions/scripts
# --------------------------------------------------
source(here::here("code/model-setup/setparsvars.R")) #setting all parameters, specifying those that are  fitted
source(here::here("code/data-processing/loadcleandata.R")) #data processing function
source(here::here("code/data-processing/loadcleanucmobility.R")) #function that processes and retrieves covariate
source(here::here("code/model-fitting/runmif_allstates_local.R")) #runs mif fitting
source(here::here("code/result-exploration/exploremifresults.R")) #explore mif results
source(here::here("code/forward-simulations/simulate_trajectories.R"))
source(here::here("code/forward-simulations/runscenarios.R")) #run forward simulations for best fit mif results
source(here::here("code/model-setup/makepompmodel.R")) #function that generates the pomp model
source(here::here("code/forward-simulations/summarize_simulations.R"))


# --------------------------------------------------
# Set data source 
# --------------------------------------------------
datasource = c("COV") #one of CovidTracker (COV), NYT (NYT), JHU (JHU), USAFacts (USF)

tm <- .POSIXct(Sys.time(), "US/Eastern")  # time stamp with date, hours, minutes
timestamp <- paste(lubridate::date(tm),
                   stringr::str_pad(as.character(lubridate::hour(tm)), 
                                    width = 2, side = "left", pad = "0"),
                   stringr::str_pad(as.character(lubridate::minute(tm)), 
                                    width = 2, side = "left", pad = "0"),
                   sep='-')

# --------------------------------------------------
# specify which states to run as a vector 
# --------------------------------------------------
statevec <- c("Washington","Georgia")
state_pops <- readRDS(here::here("data/us_popsize.rds"))

# Run data cleaning script.
all_states_pomp_data <- loadcleandata(datasource = datasource, 
                                      locations = statevec, 
                                      timestamp = timestamp,
                                      smooth = FALSE)

all_states_pomp_covar <- loadcleanucmobility(
  location = statevec, 
  pomp_data = all_states_pomp_data, 
  timestamp = timestamp
) 


# --------------------------------------------------
# Loop over states  
# initial loop is serial and done to create the different bits needed for mif for each state 
# --------------------------------------------------

pomp_list <- vector("list",length(statevec)) #large list that will hold pomp model and other info for each state
ct = 1 #an indexer
for (dolocation in rev(statevec))
{
  print(sprintf('starting state %s',dolocation))
  
  # This will be appended to each saved file 
  filename_label <- dolocation
  
  # Get the state's population size
  population <- state_pops %>%
    filter(state_full == dolocation) %>%
    pull(total_pop)
  
  pomp_data <- all_states_pomp_data %>%
    filter(location == dolocation)
  
  n_knots <- round(nrow(pomp_data) / 10 )
  est_these_pars = c("log_sigma_dw", "min_frac_dead", "max_frac_dead", "log_half_dead",
                     "log_theta_cases", "log_theta_deaths")
  est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
  # est_these_inivals = ""  # to not estimate any initial values
  knot_coefs <-  paste0("b", 1:n_knots)
  est_these_pars <- c(est_these_pars, knot_coefs)
  
  # Set the parameter values and initial conditions
  par_var_list <- setparsvars(est_these_pars = est_these_pars, 
                              est_these_inivals = est_these_inivals,
                              population = population,
                              rnaught = 6)  # set R0 at beginning of epidemic
  
  # Get covariate 
  tmp_covar <- all_states_pomp_covar %>%
    filter(location == dolocation)
  
  covar = covariate_table(
    t = pomp_data$time,
    seas = bspline.basis(
      x=t,
      nbasis=n_knots,
      degree=3
    ),
    rel_beta_change = as.matrix(tmp_covar$rel_beta_change),
    trend_sim = as.matrix(rep(10, times = nrow(tmp_covar))),  # this is a placeholder only needed for simulation
    fit = 1,  # 1 = fitting; 0 = simulating
    times="t",
    order = "constant"
  )
  
  # Save all pieces for each state in a list
  # pomp_list[[ct]]$pomp_model = pomp_model 
  pomp_list[[ct]]$filename_label = filename_label
  pomp_list[[ct]]$pomp_data = pomp_data
  pomp_list[[ct]]$pomp_covar = covar
  pomp_list[[ct]]$location = dolocation
  pomp_list[[ct]]$par_var_list = par_var_list
  pomp_list[[ct]]$n_knots <- round(nrow(pomp_list[[ct]]$pomp_data) / 10 )
  
  # Make the pomp model
  pomp_model <- makepompmodel(par_var_list = pomp_list[[ct]]$par_var_list, 
                              pomp_data = pomp_list[[ct]]$pomp_data, 
                              pomp_covar = pomp_list[[ct]]$pomp_covar,
                              n_knots = pomp_list[[ct]]$n_knots)
  
  pomp_list[[ct]]$pomp_model <- pomp_model
  
  ct = ct + 1
} #done serial loop over all states that creates pomp object and other info 


### MODEL FITTING #######
# turn on parallel running or not
parallel_info = list()
parallel_info$parallel_run <- TRUE
parallel_info$num_cores <- 2  # on HPC - should ideally be M states * replicates mif runs (e.g. 10 states at a time, 20 mif runs, so 200) 

# --------------------------------------------------
# Specify settings for MIF fitting
# --------------------------------------------------
# two rounds of MIF are currently hard-coded into runmif
mif_settings = list()
mif_settings$mif_num_particles  <- c(200,200)
mif_settings$mif_num_iterations <- c(15,15)
mif_settings$pf_num_particles <- 50 #particles for filter run following mif
mif_settings$pf_reps <- 2#replicates for particle filter following mif
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$replicates <- 2 #number of different starting conditions - this is parallelized

# MIF 
# run all states
# each state is run in series
# for a given state, MIF runs are parallelized

pomp_res <- runmif_allstates_local(parallel_info = parallel_info, 
                                   mif_settings = mif_settings, 
                                   pomp_list = pomp_list, 
                                   par_var_list = par_var_list)


for (ct in 1:length(pomp_list))
{


  pomp_list[[ct]]$pomp_res = pomp_res #add mif results for each state to overall pomp object

  mif_explore <- exploremifresults(pomp_res = pomp_list[[ct]]$pomp_res, 
                                 par_var_list = pomp_list[[ct]]$par_var_list,
                                 n_knots = pomp_list[[ct]]$n_knots) #compute trace plot and best param tables for mif


  #add results computed to the pomp_res object
  pomp_res$traceplot = mif_explore$traceplot
  pomp_res$all_partable = mif_explore$all_partable
  pomp_res$est_partable = mif_explore$est_partable
  pomp_res$partable_natural = mif_explore$partable_natural
  
  # Simulate from the model at best MLE
  params <- pomp_res$all_partable %>%
    slice(1) %>%
    dplyr::select(-MIF_ID, -LogLik, -LogLik_SE) %>%
    gather() %>%
    tibble::deframe()
  sim <- pomp::simulate(pomp_res$pomp_model, params = params, 
                        nsim = 100, format = "data.frame")
  pomp_res$sims <- sim 
  
  #save the completed analysis for each state to a file with time-stamp
  #this file could be large, needs checking
  filename = paste0('output-local/', pomp_res$filename_label, '_results.rds')
  saveRDS(object = pomp_res, file = filename)
  
  # Run scenarios
  pomp_res$scenarios <- runscenarios(pomp_res, par_var_list = pomp_res$par_var_list)
  filename = paste0('output-local/', pomp_res$filename_label, '_results.rds')
  saveRDS(object = pomp_res, file = filename)  # resave/overwrite...
  
  # Summarize results
  res_summary <- summarize_simulations(sims_out = pomp_res$scenarios, 
                                       pomp_data = pomp_res$pomp_data,
                                       pomp_covar = pomp_res$pomp_covar, 
                                       location = pomp_res$location,
                                       mle_sim = pomp_res$sims)
  
  outfile <- paste0("output-local/", pomp_res$filename_label, '.csv')
  write.csv(res_summary, outfile, row.names = FALSE)
} #end loop over states