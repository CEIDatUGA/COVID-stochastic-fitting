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
mif_num_particles <- c(2000, 2000)  # two rounds of MIF
mif_num_iterations <- c(50, 50)  # two rounds of MIF
mif_cooling_fracs <- c(0.9, 0.75)  # two rounds of MIF

# For particle filter log likelihood estimation of MIF MLEs
pf_num_particles <- 2000
pf_reps <- 10

# For ABC-MCMC
abc_num_mcmc <- 5000
abc_num_burn <- abc_num_mcmc/2
abc_num_thin <- (abc_num_mcmc - abc_num_burn) * 0.0004
abc_num_thin <- 1


# Download new data and format --------------------------------------------
# results saved to data folder with time stamp in name
source(here("code/data-processing/load-clean-CT-data.R"))

filename = here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))
dat <- readRDS(filename)
pomp_data <- dat %>%
  dplyr::filter(Location == "Georgia") %>%
  dplyr::select(Date, cases, hosps, deaths) %>%
  dplyr::arrange(Date)
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}
pomp_data$cases <- ceiling(ma(pomp_data$cases))
pomp_data$hosps <- ceiling(ma(pomp_data$hosps))
pomp_data$deaths <- ceiling(ma(pomp_data$deaths))

# decycle <- function(y, wday) {
#   sy <- predict(smooth.spline(seq_along(y), y, spar = 0.75))$y
#   sy[sy < 0] <- 0
#   df <- data.frame(w = wday, y = y, sy = sy) %>%
#     mutate(rd = y / sy) %>%
#     filter(y > 0 & sy > 0) %>%
#     group_by(w) %>%
#     summarise(mrd = mean(rd))
# }
# 
# wday_data <- as.matrix(pomp_data[ , -c(1,5)])
# wday_data[which(is.na(wday_data))] <- 0
# wday_deviations <- apply(wday_data, MARGIN = 2, decycle, 
#                          wday = pomp_data$weekday)
# 
# cases <- pomp_data %>% 
#   dplyr::select(Date, weekday, cases) %>%
#   left_join(wday_deviations$cases, by = c("weekday" = "w")) %>%
#   mutate(cases_adj = round(cases / mrd)) %>%
#   dplyr::select(Date, cases_adj)
# hosps <- pomp_data %>% 
#   dplyr::select(Date, weekday, hosps) %>%
#   left_join(wday_deviations$hosps, by = c("weekday" = "w")) %>%
#   mutate(hosps_adj = round(hosps / mrd)) %>%
#   dplyr::select(Date, hosps_adj)
# deaths <- pomp_data %>% 
#   dplyr::select(Date, weekday, deaths) %>%
#   left_join(wday_deviations$deaths, by = c("weekday" = "w")) %>%
#   mutate(deaths_adj = round(deaths / mrd)) %>%
#   dplyr::select(Date, deaths_adj)
# 
# pomp_data <- cases %>%
#   left_join(hosps, by = "Date") %>%
#   left_join(deaths, by = "Date") %>%
#   rename("cases" = cases_adj,
#          "hosps" = hosps_adj,
#          "deaths" = deaths_adj)

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


# Make the unacast covariate table ----------------------------------------

thefiles <- list.files(path = here("data/"), pattern = "ga_state_raw")
thefile <- paste0("data/", tail(thefiles))
if(file.exists(here(thefile))) {
  source(here("code/model-beta-reduction.R"))
}

covar_table <- readRDS(here("output/rel-beta-change-covar.RDS"))


# Make a pomp model with newest data --------------------------------------

source(here("code/make-pomp-model.R"))


# Run the mif routine -----------------------------------------------------

source(here("code/run-mif.R"))


# Make the prior Csnippet -------------------------------------------------

source(here("code/set-par-vals.R"))
source(here("code/set-priors.R"))


# Run the ABC-MCMC --------------------------------------------------------

source(here("code/run-abc.R"))




