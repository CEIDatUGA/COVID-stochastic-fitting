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
                   "frac_dead", "log_sigma_dw", "log_theta_cases",
                   "log_theta_hosps", "log_theta_deaths")
# est_these_inivals = c("E1_0")
est_these_inivals = ""

# run function that sets variables and parameters 
# functions doesn't return anything, results are written to file
setparsvars(est_these_pars = est_these_pars, est_these_inivals = est_these_inivals)
 

# Set priors --------------------------
# needs results from setparsvars 
# results are written into RDS file and loaded by later scripts
source(here("code/model-setup/setpriors.R"))
setpriors()

# Run data cleaning script. Then load cleaned data ready for pomp --------------------------------------------
# results saved to data folder with time stamp in name
# results will be loaded by later files
source(here("code/data-processing/loadcleanCTdata.R"))
# define which location(s) (states) one wants to use. Needs to agree with Location variable in the data
use_these_locations = c("Georgia")
loadcleanCTdata(use_these_locations = use_these_locations)

# ANDREW: NEED TO MAKE THIS SO WE CAN CHANGE STATES
# Make the unacast covariate table ----------------------------------------
# results are saved to data folder with time stamp in name
# results will be loaded by later files
thefiles <- list.files(path = here("data/"), pattern = "ga_state_raw")
if(length(thefiles) != 0) {
  source(here("code/model-setup/modelbetareduction.R"))
  modelbetareduction() #run function. No return, saves results to file
}

# Make a pomp model 
# loads all the previously generated RDS files and generates a pomp model that's ready for fitting
# the resulting pomp object is saved as RDS file into the output folder
source(here("code/model-setup/makepompmodel.R"))
makepompmodel()


# Run the mif fitting routine -----------------------------------------------------

# turn on parallel running or not
parallel_info = list()
parallel_info$parallel_run <- TRUE
parallel_info$num_cores <- parallel::detectCores() - 4  # alter as needed
# parallel_info$num_cores <- 30  # on HPC

# specify settings for mif2 procedure
# two rounds of MIF
# these 2 rounds are currently hard-coded into runmif
mif_settings = list()
mif_settings$mif_num_particles  <- c(100,100)
mif_settings$mif_num_iterations <- c(10,10)
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$pf_num_particles <- 20
mif_settings$pf_reps <- 2

# loads the previously generated pomp model 
source(here("code/model-fitting/runmif.R"))

#supply 2 lists containing info for parallel and mif settings
runmif(parallel_info = parallel_info, mif_settings = mif_settings) 

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




