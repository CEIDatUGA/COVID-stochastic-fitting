# 00-MASTER-LOCAL-CLUSTER.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 model and for producing
# estimates of observed states and forecasts of states. 

# --------------------------------------------------
# --------------------------------------------------
# General setup
# --------------------------------------------------
# --------------------------------------------------


# Start with a clean workspace to avoid downstream errors -----------------
rm(list = ls(all.names = TRUE))

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
# library('snow')
# library('doSNOW')
# library('Rmpi')


# --------------------------------------------------
# Source all needed functions/scripts
# --------------------------------------------------
source(here("code/model-setup/setparsvars.R")) #setting all parameters, specifying those that are  fitted
source(here("code/data-processing/loadcleandata.R")) #data processing function
source(here("code/data-processing/loadcleanapplemobility.R")) #function that processes and retrieves covariate
source(here("code/model-setup/makepompmodel.R")) #function that generates the pomp model
source(here("code/model-fitting/runmif_allstates.R")) #runs mif fitting
source(here("code/result-exploration/exploremifresults.R")) #explore mif results
source(here("code/forward-simulations/runscenarios.R")) #run forward simulations for best fit mif results

# --------------------------------------------------
# Set data source 
# --------------------------------------------------
datasource = c("COV") #one of CovidTracker (COV), NYT (NYT), JHU (JHU), USAFacts (USF)

# --------------------------------------------------
# Define parameter and variable names that are being estimated
# this is currently set up such that they are the same for each state
# if one wanted to change this it would require a bit of re-coding
# --------------------------------------------------
#This is passed to setparsvars function. 
#If set to "all", all params are estimated
est_these_pars = c("log_beta_s", 
                   "frac_hosp", "frac_dead", 
                   "max_detect_par", "log_detect_inc_rate", "log_half_detect",
                   "log_sigma_dw", 
                   "log_theta_cases", "log_theta_hosps", "log_theta_deaths")
# est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
est_these_inivals = ""


# --------------------------------------------------
# Specify if functions that are able to run in parallel will do so
# if yes, set parallel options
# --------------------------------------------------

# turn on parallel running or not
parallel_info = list()
parallel_info$parallel_run <- TRUE
#parallel_info$num_cores <- parallel::detectCores() - 1  # alter as needed
# Add one extra core for the master process
parallel_info$num_cores <- 20  # on HPC - should ideally be M states * replicates mif runs (e.g. 10 states at a time, 20 mif runs, so 200) 

#to estimate run-time: 
#run interactively non-parallel with planned MIF settings (possibly lower MIF replicates)
#watch output to see how long 1 state takes
#main loop is done 5x, 10 states each. 
#so if core number is 10 x mif runs (replicates), speed is 5x single state (max speed)
#for less cores or more/less MIF replicates, multiply by the appropriate factor
#on my machine,  2000 particles, 150 iterations and 5 reps on 50 cores takes 75min per 
#batch of states, so total 5 x 75
#if we do 20 mif reps on 200 cores on cluster, should be same duration

# --------------------------------------------------
# Specify settings for MIF fitting
# --------------------------------------------------
# two rounds of MIF are currently hard-coded into runmif
mif_settings = list()
#mif_settings$mif_num_particles  <- c(20,20)
#mif_settings$mif_num_iterations <- c(10,10)
#mif_settings$pf_num_particles <- 200 #particles for filter run following mif
#mif_settings$pf_reps <- 5 #replicates for particle filter following mif
mif_settings$mif_num_particles  <- c(200,200)
mif_settings$mif_num_iterations <- c(10,10)
mif_settings$pf_num_particles <- 500 #particles for filter run following mif
mif_settings$pf_reps <- 2 #replicates for particle filter following mif
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$replicates <- 2 #number of different starting conditions - this is parallelized

# --------------------------------------------------
# Create a time-stamp variable
# Will be applied to saved results
# defined outside loop so it's the same for each state
# --------------------------------------------------
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

statevec = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
# 'District of Columbia',  'Puerto Rico', 'Guam', 'American Samoa', 'Mariana Islands', 'Virgin Islands' #could include those




# --------------------------------------------------
# --------------------------------------------------
# Run the functions sourced above 
# --------------------------------------------------
# --------------------------------------------------

# run function that sets variables and parameters 
par_var_list <- setparsvars(est_these_pars = est_these_pars, est_these_inivals = est_these_inivals)

# --------------------------------------------------
# Loop over states  
# initial loop is serial and done to create the different bits needed for mif for each state 
# --------------------------------------------------

pomp_list <- vector("list",length(statevec)) #large list that will hold pomp model and other info for each state
ct = 1 #an indexer
for (location in statevec)
{
  print(sprintf('starting state %s',location))
  
  # This will be appended to each saved file 
  filename_label <- paste(location,datasource,timestamp,sep="-") 
  
  # Run data cleaning script.
  pomp_data <- loadcleandata(datasource = datasource, location = location, timestamp = timestamp) 
  
  # Get covariate 
  pomp_covar <- loadcleanapplemobility(location = location, startdate = min(pomp_data$date), enddate = max(pomp_data$date), timestamp = timestamp) 
  
  # Make a pomp model 
  pomp_model <- makepompmodel(par_var_list = par_var_list, pomp_data = pomp_data, pomp_covar = pomp_covar)

  # Save all pieces for each state in a list
  pomp_list[[ct]]$pomp_model = pomp_model 
  pomp_list[[ct]]$filename_label = filename_label
  pomp_list[[ct]]$pomp_data = pomp_data
  pomp_list[[ct]]$pomp_covar = pomp_covar
  pomp_list[[ct]]$location = location
  
  ct = ct + 1
  
} #done serial loop over all states that creates pomp object and other info 

  
# --------------------------------------------------
# Loop over states  
# done in parallel and batches of 10 states each
# --------------------------------------------------

mif_res = vector("list", length = 5) #hard coded way of doing 5 batches of 10 states each. 
for (n in 1:5)
{
  
  now_list = pomp_list[(10*n-9):(10*n)]
  
  # --------------------------------------------------
  # Run the mif fitting routine 
  # this is parallel over M states with N mif fits for each state
  # the 
  # --------------------------------------------------
  mif_res[[n]] <- runmif_allstates(parallel_info = parallel_info, 
                     mif_settings = mif_settings, 
                     pomp_list = now_list, 
                     par_var_list = par_var_list)

} #end parallel MIF loop  
  
#mif_res returned by above function is a complicated list
#main level is the n repeats over batches of states
#level below is a list with M states
#flatten to make it list of all states
mif_flat = purrr::flatten(mif_res)

#mif_flat is a list with each state at the top level
#each state is a list containing sub-list of each replicate, which contains 2 elements, out_mif and pfs
#E.g.: mif_fit results from for state 3 and replicate 2 would be mif_flat[[3]][[2]]$out_mif


# --------------------------------------------------
# Another serial loop over states  
# performs mif result analysis, forecasting and data processing for each state
# --------------------------------------------------
all_df = NULL #will contain results for all states to be fed to shiny 
# loop over all states
for (ct in 1:length(statevec))
{
  all_scenarios = NULL #will contain all scenarios for a state as a data frame
  print(sprintf('starting state %s',statevec[ct]))
  
  pomp_res = pomp_list[[ct]] #current state
  pomp_res$mif_res = mif_flat[[ct]] #add mif results for each state to overall pomp object
  
  mif_explore <- exploremifresults(pomp_res = pomp_res, par_var_list = par_var_list) #compute trace plot and best param tables for mif
  #add resutls computed to the pomp_res object
  pomp_res$traceplot = mif_explore$traceplot
  pomp_res$all_partable = mif_explore$all_partable
  pomp_res$est_partable = mif_explore$est_partable
  pomp_res$partable_natural = mif_explore$partable_natural
  
  #run simulations/forecasts based on mif fits for each state
  scenario_res <- runscenarios(pomp_res = pomp_res, par_var_list = par_var_list, forecast_horizon_days = 37, nsim = 114)
  
  #to save space, one can delete the full scenario simulation  results before saving
  #als place results for all scenarios into a large dataframe
  
  for (n in 1:length(scenario_res)) 
  {
    scenario_res[[n]]$sims <- NULL
    all_scenarios = rbind(all_scenarios, scenario_res[[n]]$scenario_df)
  }
  #add forward simulation results to pomp_res object
  pomp_res$scenario_res = scenario_res
  
  pomp_res$mif_res <- NULL #to save space, one can delete the full mif results before saving
  
  #save the completed analysis for each state to a file with time-stamp
  #this file could be large, needs checking
  filename = here('output',paste0(pomp_res$filename_label,'_results.rds'))
  saveRDS(object = pomp_res, file = filename)
  
  all_df = rbind(all_df, all_scenarios) #add all states together into a long data frame, will be saved below and used by shiny
}

#this is what shiny will use
filename = here('output','results_for_shiny.rds')
saveRDS(all_df,filename)  
