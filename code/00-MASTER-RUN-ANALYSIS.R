# 00-MASTER-RUN-ANALYSIS.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 SEIR model and for producing
# estimates of observed states and forecasts of states. Many of the scripts
# rely on parallel processing, the settings for which are defined in this
# master script.


# Start with a clean workspace to avoid downstream errors -----------------

rm(list = ls(all.names = TRUE))


# Load all necessary libraries --------------------------------------------

library(tidyverse)
library(pomp)  # must be at least version 2.x
library(here)
library(doParallel)
library(foreach)


# Define global variables -------------------------------------------------

# For parallel processing
num_cores <- parallel::detectCores() - 2  # alter as needed

# For mif2
mif_num_particles <- 20
mif_num_iterations1 <- 15
mif_num_iterations2 <- 10

# For particle filter
pf_num_particles <- 1000
pf_reps <- 10

# For ABC-MCMC
abc_num_mcmc <- 20000
abc_num_burn <- abc_num_mcmc/2
abc_num_thin <- (abc_num_mcmc - abc_num_burn) * 0.0004


# Download new data and format --------------------------------------------

source(here("code/load-clean-CT-data.R"))


# Make a pomp model with newest data --------------------------------------

source(here("code/make-pomp-model.R"))


# Run the mif routine -----------------------------------------------------

source(here("code/run-mif.R"))


# Make the prior Csnippet -------------------------------------------------

source(here("code/set-par-vals.R"))
source(here("code/set-priors.R"))


# Run the ABC-MCMC --------------------------------------------------------

source(here("code/run-abc.R"))




