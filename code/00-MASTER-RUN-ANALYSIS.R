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
parallel_run <- TRUE
num_cores <- parallel::detectCores() - 2  # alter as needed

# For mif2
mif_num_particles <- c(1000, 1000)  # two rounds of MIF
mif_num_iterations <- c(30, 20)  # two rounds of MIF
mif_cooling_fracs <- c(0.9, 0.75)  # two rounds of MIF

# For particle filter log likelihood estimation of MIF MLEs
pf_num_particles <- 200
pf_reps <- 2

# For ABC-MCMC
abc_num_mcmc <- 2000
abc_num_burn <- abc_num_mcmc/2
abc_num_thin <- (abc_num_mcmc - abc_num_burn) * 0.0004


# Download new data and format --------------------------------------------
# results saved to data folder with time stamp in name
source(here("code/data-processing/load-clean-CT-data.R"))

filename = here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))
dat <- readRDS(filename)
pomp_data <- dat %>%
  dplyr::filter(Location == "Georgia") %>%
  dplyr::select(Date, cases, hosps, deaths) %>%
  dplyr::arrange(Date)

# Create full time series of NAs to make sure pomp_data
# starts at the time of model initialization
pseudo_data <- data.frame(
  Date = seq.Date(from = as.Date("2020-03-01"), to = Sys.Date(), by = "day"),
  hold = NA)

# Merge in the NAs to complete the time series
pomp_data <- pomp_data %>%
  right_join(pseudo_data, by = "Date") %>%
  dplyr::select(-hold) %>%
  mutate(time = 1:n()) %>%
  dplyr::select(time, cases, hosps, deaths)


# Make a pomp model with newest data --------------------------------------

source(here("code/make-pomp-model.R"))


# Run the mif routine -----------------------------------------------------

source(here("code/run-mif-ah.R"))


# Make the prior Csnippet -------------------------------------------------

source(here("code/set-par-vals.R"))
source(here("code/set-priors.R"))


# Run the ABC-MCMC --------------------------------------------------------

source(here("code/run-abc.R"))




