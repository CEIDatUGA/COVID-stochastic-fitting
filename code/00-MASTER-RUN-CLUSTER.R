# 00-MASTER-LOCAL-CLUSTER.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 model and for producing
# estimates of observed states and forecasts of states. 

# Start with a clean workspace to avoid downstream errors -----------------
rm(list = ls(all.names = TRUE))

# --------------------------------------------------
# Load necessary libraries -------------------------
# --------------------------------------------------
# could be loaded here or inside functions
library(lubridate) #needs to loaded first so it doesn't mess with 'here'
library(dplyr)
library(tidyr)
library('readr')
library(pomp)  # must be at least version 2.x
library(doParallel)
library(foreach)
library(here)
library(purrr)
library(ggplot2)

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
source(here("code/result-exploration/processscenarios.R")) #process results from all runs to get them into form for shiny

# --------------------------------------------------
# Set data source 
# --------------------------------------------------
datasource = c("COV") #one of CovidTracker (COV), NYT (NYT), JHU (JHU), USAFacts (USF)

# --------------------------------------------------
# Define parameter and variable names that are being estimated
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
parallel_info$parallel_run <- FALSE
#parallel_info$num_cores <- parallel::detectCores() - 1  # alter as needed
parallel_info$num_cores <- 4  # on HPC - should ideally be M states * N mif runs (e.g. 10 states at a time, 20 mif runs, so 200) 


# --------------------------------------------------
# Specify settings for MIF fitting
# --------------------------------------------------
# two rounds of MIF are currently hard-coded into runmif
mif_settings = list()
mif_settings$mif_num_particles  <- c(20,20)
mif_settings$mif_num_iterations <- c(10,10)
mif_settings$pf_num_particles <- 200 #particles for filter run following mif
mif_settings$pf_reps <- 5 #replicates for particle filter following mif
#mif_settings$mif_num_particles  <- c(2000,2000)
#mif_settings$mif_num_iterations <- c(100,100)
#mif_settings$pf_num_particles <- 2000 #particles for filter run following mif
#mif_settings$pf_reps <- 50 #replicates for particle filter following mif
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$replicates <- 5 #number of different starting conditions

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



# run function that sets variables and parameters 
par_var_list <- setparsvars(est_these_pars = est_these_pars, est_these_inivals = est_these_inivals)

# --------------------------------------------------
# vector with states to process
statevec = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
# 'District of Columbia',  'Puerto Rico', 'Guam', 'American Samoa', 'Mariana Islands', 'Virgin Islands' #could include those



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
  

browser()


# --------------------------------------------------
# Another serial loop over states  
# performs mif result analysis, forecasting and data processing for each state
# --------------------------------------------------
for (ct in 1:length(statevec))
{
  
  mif_explore <- exploremifresults(pomp_res = pomp_list[[ct]]) #compute trace plot and best param tables for mif

  pomp_list[[ct]]$traceplot = mif_explore$traceplot
  pomp_list[[ct]]$all_partable = mif_explore$all_partable
  pomp_list[[ct]]$est_partable = mif_explore$est_partable
  pomp_list[[ct]]$partable_natural = mif_explore$partable_natural
  
  #run simulations/forecasts based on mif fits for each state
  scenario_res <- runscenarios(mif_res = pomp_list[[ct]], forecast_horizon_days = 37, nsim = 514)
  
  pomp_list[[ct]]$scenario_res = scenario_res
  
  #save the completed analysis for each state to a file with time-stamp
  #this file could be large, needs checking
  #filename = here('output',paste0(filename_label,'_results.rds'))
  #saveRDS(object = pomp_list, file = filename)
  
}

    
  


# script below is run once all state results are processed
# process all scenario runs so data is in a form useful for shiny
processscenarios()





