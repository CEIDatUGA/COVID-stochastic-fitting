# 00-MASTER-RUN-ANALYSIS.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 SEIR model and for producing
# estimates of observed states and forecasts of states. Many of the scripts
# rely on parallel processing, the settings for which are defined in this
# master script.

# Start with a clean workspace to avoid downstream errors -----------------
rm(list = ls(all.names = TRUE))


# Load necessary libraries --------------------------------------------
library(here)
# Only libraries needed for this script, others are loaded in each code
# so we can run various scripts independently 
# these libraries are needed by various scripts
#library(dplyr)
#library(tidyr)
#library(pomp)  # must be at least version 2.x
#library(doParallel)
#library(foreach)
#library(purrr)
#library(ggplot2)


# Run script that defines parameter and variable names
# specifies parameters that are being fitted
# adjust parameters to be fitted at the bottom of this script
# this will then be automatically passed through all the fitting scripts
# assigns values to variables and initial conditions
# results are written into RDS file and loaded by later scripts
source(here("code/model-setup/set-pars-and-vars.R"))

# Run script to set priors
# needs results from set-pars-and-vars.R 
# results are written into RDS file and loaded by later scripts
# source(here("code/model-setup/set-priors.R"))

# Run data cleaning script. Then load cleaned data ready for pomp --------------------------------------------
# results saved to data folder with time stamp in name
# results will be loaded by later files
source(here("code/data-processing/load-clean-CT-data.R"))

# Make the unacast covariate table ----------------------------------------
# results are saved to data folder with time stamp in name
# results will be loaded by later files
thefiles <- list.files(path = here("data/"), pattern = "ga_state_raw")
if(length(thefiles) != 0) {
  source(here("code/model-setup/model-beta-reduction.R"))
}

# Make a pomp model 
# loads all the previously generated RDS files and generates a pomp model that's ready for fitting
# the resulting pomp object is saved as RDS file into the output folder
source(here("code/model-setup/make-pomp-model.R"))


# Run the mif fitting routine -----------------------------------------------------
# loads the previously generated pomp model 
source(here("code/model-fitting/run-mif.R"))

# Does post processing and exploration on the best fit mif results -----------------------------------------------------
# all result figures are saved into the /output/figures/ and /output/tables/ folders
source(here("code/result-exploration/explore-mif-results.R"))


# Simulate the model to predict -----------------------------------------------------
# loads the previously generated pomp model 
# if one wants to run simulations based on best fit
# one needs to set those in the script and also make sure run-mif
# as well as explore-mif (so the table with best fit parameters is generated)
source(here("code/forward-simulations/simulate-pomp-model.R"))

# Explore simulation results -----------------------------------------------------
# loads the previously generated forward simulations 
# all result figures are saved into the appropriate /output/ sub-folders
source(here("code/result-exploration/explore-simulation-results.R"))


# Run the ABC-MCMC --------------------------------------------------------
# source(here("code/model-fitting/run-abc.R"))




