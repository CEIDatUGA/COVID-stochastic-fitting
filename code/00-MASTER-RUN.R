# 00-MASTER-LOCAL-RUN.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 SEIR model and for producing
# estimates of observed states and forecasts of states. Many of the scripts
# rely on parallel processing, the settings for which are defined in this
# master script.

# Start with a clean workspace to avoid downstream errors -----------------
rm(list = ls(all.names = TRUE))

# Necessary libraries --------------------------------------------
# We only libraries needed for this script, others are loaded in each code
# so we can run various scripts independently 
# these libraries are needed by various scripts
#library(lubridate) #needs to be present/is used below, but don't load since it messes with 'here'
library(here)
#library(dplyr)
#library(tidyr)
#library(pomp)  # must be at least version 2.x
#library(doParallel)
#library(foreach)
#library(purrr)
#library(ggplot2)


# --------------------------------------------------
# Set data source 
# --------------------------------------------------
datasource = c("COV") #one of CovidTracker (COV), NYT (NYT), JHU (JHU), USAFacts (USF)


# --------------------------------------------------
# Loop over states  
# --------------------------------------------------

statevec = c("Georgia","Texas","Washington")

for (location in statevec)
{
  print(sprintf('starting state %s',location))
  
  # --------------------------------------------------
  # Create a time-stamp variable
  # --------------------------------------------------
  # Time stamp for results and output
  tm <- .POSIXct(Sys.time(), "US/Eastern")  # time stamp with date, hours, minutes
  stamp <- paste(lubridate::date(tm),
                 stringr::str_pad(as.character(lubridate::hour(tm)), 
                                  width = 2, side = "left", pad = "0"),
                 stringr::str_pad(as.character(lubridate::minute(tm)), 
                                  width = 2, side = "left", pad = "0"),
                 sep='-')
  
  # This will be appended to each saved file 
  filename_label <- paste(location,datasource,stamp,sep="-") 
  
  
  # --------------------------------------------------
  # Define parameter and variable names --------------------------------
  # --------------------------------------------------
  #define parameters to be estimated
  #is passed to setparsvars function. 
  #If set to "all", all params are estimated
  est_these_pars = c("log_beta_s", 
                     "frac_hosp", "frac_dead", 
                     "max_detect_par", "log_detect_inc_rate", "log_half_detect",
                     "log_sigma_dw", 
                     "log_theta_cases", "log_theta_hosps", "log_theta_deaths")
  # est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
  est_these_inivals = ""
  
  # source function which assigns values to variables and initial conditions
  # specifies parameters that are being fitted
  source(here("code/model-setup/setparsvars.R"))
  
  # run function that sets variables and parameters 
  # functions doesn't return anything, results are written to file
  par_var_list <- setparsvars(est_these_pars = est_these_pars, est_these_inivals = est_these_inivals)
  
  # --------------------------------------------------
  # Set priors --------------------------
  # --------------------------------------------------
  # needs results from setparsvars 
  #source(here("code/model-setup/setpriors.R"))
  #prior_dens <- setpriors(par_var_list)
  
  
  # --------------------------------------------------
  # Run data cleaning script. Return data ready for pomp --------------------------------------------
  # specify which source and location (state)
  # is done above
  # --------------------------------------------------
  source(here("code/data-processing/loadcleandata.R"))
  pomp_data <- loadcleandata(datasource = datasource, location = location) 
  
  
  # --------------------------------------------------
  #get covariate for transmission model parameter
  #supply start and end dates so it agrees with data
  #if covariate does not span data range, the missing is filled by repeating 1st/last value
  #this is Apple source 
  #https://www.apple.com/covid19/mobility
  # --------------------------------------------------
  source(here("code/data-processing/loadcleanapplemobility.R"))
  covar_table <- loadcleanapplemobility(location = location, startdate = min(pomp_data$date), enddate = max(pomp_data$date)) 
  
  
  # --------------------------------------------------
  #get covariate for transmission model parameter
  #this is unacast source 
  # --------------------------------------------------
  #covar_table <- readRDS(here("output/rel-beta-change-covar.RDS"))
  
  
  
  # --------------------------------------------------
  # Make a pomp model -----------------------------------------------------
  # --------------------------------------------------
  # use data, covariate and parameter information to make a pomp model that's ready for fitting
  source(here("code/model-setup/makepompmodel.R"))
  pomp_model <- makepompmodel(par_var_list = par_var_list, pomp_data = pomp_data, covar_table = covar_table)
  
  
  # --------------------------------------------------
  # Run the mif fitting routine -----------------------------------------------------
  # --------------------------------------------------
  
  # turn on parallel running or not
  parallel_info = list()
  parallel_info$parallel_run <- TRUE
  parallel_info$num_cores <- parallel::detectCores() - 1  # alter as needed
  #parallel_info$num_cores <- 40  # on HPC
  
  # specify settings for mif2 procedure
  # two rounds of MIF
  # these 2 rounds are currently hard-coded into runmif
  mif_settings = list()
  mif_settings$mif_num_particles  <- c(200,200)
  #mif_settings$mif_num_particles  <- c(2000,2000)
  mif_settings$mif_num_iterations <- c(30,20)
  #mif_settings$mif_num_iterations <- c(100,100)
  mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
  mif_settings$pf_num_particles <- 2000
  mif_settings$pf_reps <- 10
  
  # source the mif function
  source(here("code/model-fitting/runmif.R"))
  #supply all info to mif and run it 
  #output is list containing an object of mif runs and an object of pfilter runs for each mif
  mif_res <- runmif(parallel_info = parallel_info, 
                     mif_settings = mif_settings, 
                     pomp_model = pomp_model, 
                     par_var_list = par_var_list)
  
  #add all parts used for mif result to this list
  #this now includes the complete information for a given mif run
  #not saving the prior object since it's not used by mif
  
  mif_res$pomp_model = pomp_model 
  mif_res$pomp_data = pomp_data 
  mif_res$par_var_list = par_var_list 
  mif_res$location = location 
  mif_res$covar_table = covar_table 
  mif_res$datasource = datasource
  mif_res$filename_label = filename_label
  
  # Does post processing and exploration on the best fit mif results -----------------------------------------------------
  #currently returns a trace plot figure (as ggplot object)
  #and 2 parameter tables. optional if turned on a likelihood slice plot
  
  source(here("code/result-exploration/exploremifresults.R"))
  mif_explore <- exploremifresults(mif_res = mif_res)
  #add results from mif exploration to mif_res object
  #trace plot, table with estimated parameters, table with all paramters, table with all parameters in natural units (as used in model)
  mif_res$traceplot = mif_explore$traceplot
  mif_res$all_partable = mif_explore$all_partable
  mif_res$est_partable = mif_explore$est_partable
  mif_res$partable_natural = mif_explore$partable_natural
  
  #save the complete mif object and all information used to create it to a file
  #saved in a permanent file with time-stamp
  filename = here('output',paste0(filename_label,'_mif.rds'))
  saveRDS(object = mif_res, file = filename)
  #also saved into a file with generic name, so it can easily be loaded by all
  #downstream scripts
  filename_temp = here('output','output_mif.rds')
  saveRDS(object = mif_res, file = filename_temp)
  
  
  # Simulate the model to predict -----------------------------------------------------
  # mif_res = readRDS(here("output",'output_mif.rds'))
  
  # Source the script run the scenarios -- saves a file this time
  source(here("code/forward-simulations/runscenarios.R"))
  scenario_res <- runscenarios(mif_res = mif_res, forecast_horizon_days = 37, nsim = 98)
}

# script below is run once all state results are processed
# process all scenario runs so data is in a form useful for shiny
source(here("code/result-exploration/processscenarios.R"))
processscenarios()





