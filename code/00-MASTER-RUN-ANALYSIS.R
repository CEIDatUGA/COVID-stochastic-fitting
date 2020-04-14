# 00-MASTER-RUN-ANALYSIS.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 SEIR model and for producing
# estimates of observed states and forecasts of states. Many of the scripts
# rely on parallel processing, the settings for which are defined in this
# master script.


# Start with a clean workspace to avoid downstream errors -----------------

rm(list = ls(all.names = TRUE))


# Load all necessary libraries --------------------------------------------
# moved back into individual scripts
library(tidyverse)
#library(pomp)  # must be at least version 2.x
library(here)
#library(doParallel)
#library(foreach)

# Define global variables -------------------------------------------------

# For parallel processing
# moved back to individual files  
#parallel_run <- FALSE
#num_cores <- parallel::detectCores() - 2  # alter as needed

# Run script that defines parameter and variable names
# specifies parameters that are being fitted
# assigns values to variables and initial conditions
# results are written into RDS file and loaded by later scripts
source(here("code/set-pars-and-vars.R"))

# Run script to set priors
# needs results from set-pars-and-vars.R 
# results are written into RDS file and loaded by later scripts
source(here("code/set-priors.R"))

# Run data cleaning script. Then load cleaned data ready for pomp --------------------------------------------
# results saved to data folder with time stamp in name
# results will be loaded by later files
source(here("code/data-processing/load-clean-CT-data.R"))

# Make the unacast covariate table ----------------------------------------
# results are saved to data folder with time stamp in name
# results will be loaded by later files
thefiles <- list.files(path = here("data/"), pattern = "ga_state_raw")
if(length(thefiles) != 0) {
  source(here("code/model-beta-reduction.R"))
}

# Make a pomp model 
# loads all the previously generated RDS files and generates a pomp model that's ready for fitting
# the resulting pomp object is saved as RDS file into the output folder
source(here("code/make-pomp-model.R"))


# Run the mif fitting routine -----------------------------------------------------
# loads the previously generated pomp model 
source(here("code/run-mif.R"))


# Run the ABC-MCMC --------------------------------------------------------
source(here("code/run-abc.R"))




