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
library(here)
#library(dplyr)
#library(tidyr)
#library(pomp)  # must be at least version 2.x
#library(doParallel)
#library(foreach)
#library(purrr)
#library(ggplot2)

# --------------------------------------------------
# Set state, data source and a time-stamp variable
# --------------------------------------------------
location = c("Georgia")
datasource = c("COV") #one of CovidTracker (COV), Ga DPH (GAD), NYT (NYT), JHU (JHU)
stamp = Sys.Date() #might need to include time if we want finer resolution

filename_label = paste(location,datasource,stamp,sep="_") #this will be appended to each saved file 


# Define parameter and variable names --------------------------------
# assigns values to variables and initial conditions
# specifies parameters that are being fitted
# results are written into RDS file and loaded by later scripts
# source function
source(here("code/model-setup/setparsvars.R"))

#define parameters to be estimated
#is passed to setparsvars function. 
#If set to "all", all params are estimated
est_these_pars = c("log_beta_s", "max_detect_par", "frac_hosp", 
                   "frac_dead", "log_theta_cases", "log_sigma_dw",
                   "log_theta_hosps", "log_theta_deaths")
# est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
est_these_inivals = ""

# run function that sets variables and parameters 
# functions doesn't return anything, results are written to file
par_var_list <- setparsvars(est_these_pars = est_these_pars, est_these_inivals = est_these_inivals)

# Set priors --------------------------
# needs results from setparsvars 
source(here("code/model-setup/setpriors.R"))
prior_dens <- setpriors(par_var_list)


# Run data cleaning script. Return data ready for pomp --------------------------------------------
source(here("code/data-processing/loadcleanCTdata.R"))
source(here("code/data-processing/loadcleanGDPHdata.R"))

if (datasource == "COV")
{
  pomp_data <- loadcleanCTdata(use_these_locations = location, start_date = "2020-03-01")
}
if (datasource == "GAD")
{  
  pomp_data <- loadcleanGDPHdata(start_date = "2020-03-01")
}


# ANDREW: NEED TO MAKE THIS SO WE CAN PASS STATE TO FUNCTION AND 
# GET UNACAST COVARIATE FOR SPECIFIED STATE
# Make the unacast covariate table ----------------------------------------
# results are saved to data folder with time stamp in name
# results will be loaded by later files
# thefiles <- list.files(path = here("data/"), pattern = "ga_state_raw")
# if(length(thefiles) != 0) {
#   source(here("code/model-setup/modelbetareduction.R"))
# covar_table <-  modelbetareduction() #run function. No return, saves results to file
# }
covar_table <- readRDS(here("output/rel-beta-change-covar.RDS"))


# Make a pomp model -----------------------------------------------------
# use data, covariate and parameter information to make a pomp model that's ready for fitting
source(here("code/model-setup/makepompmodel.R"))
pomp_model <- makepompmodel(par_var_list = par_var_list, pomp_data = pomp_data, covar_table = covar_table)


# Run the mif fitting routine -----------------------------------------------------

# turn on parallel running or not
parallel_info = list()
parallel_info$parallel_run <- TRUE
# parallel_info$num_cores <- parallel::detectCores() - 4  # alter as needed
parallel_info$num_cores <- 30  # on HPC

# specify settings for mif2 procedure
# two rounds of MIF
# these 2 rounds are currently hard-coded into runmif
mif_settings = list()
mif_settings$mif_num_particles  <- c(2000,2000)
mif_settings$mif_num_iterations <- c(100,100)
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$pf_num_particles <- 2000
mif_settings$pf_reps <- 10

# loads the previously generated pomp model 
source(here("code/model-fitting/runmif.R"))

#supply all info to mif and run it 
#output is list containing an object of mif runs and an object of pfilter runs for each mif
mif_list <- runmif(parallel_info = parallel_info, 
                   mif_settings = mif_settings, 
                   pomp_model = pomp_model, 
                   par_var_list = par_var_list)

saveRDS(object = mif_list, file = paste0(filename_label,'_mif.rds'))

# Does post processing and exploration on the best fit mif results -----------------------------------------------------
# all result figures are saved into the /output/figures/ and /output/tables/ folders
# source(here("code/result-exploration/explore-mif-results.R"))
# 
# 
# # Simulate the model to predict -----------------------------------------------------
# # loads the previously generated pomp model 
# # if one wants to run simulations based on best fit
# # one needs to set those in the script and also make sure run-mif
# # as well as explore-mif (so the table with best fit parameters is generated)
# 
# source(here("code/forward-simulations/simulatepompmodel.R"))
# #set one of these to designate where the parameter values for the simulation
# #should come from
# #parsource = "base"
# parsource = "mif"
# #parsource = "pmcmc"
# #parsource = "manual"
# 
# simulatepompmodel(parsource = parsource)
# 
# # Explore simulation results -----------------------------------------------------
# # loads the previously generated forward simulations 
# # all result figures are saved into the appropriate /output/ sub-folders
# source(here("code/result-exploration/explore-simulation-results.R"))


# Run the ABC-MCMC --------------------------------------------------------
# source(here("code/model-fitting/run-abc.R"))




