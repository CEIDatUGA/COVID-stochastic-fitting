# simulate-pomp-model.R
#
# This script runs the previously generated pomp model
# it does not do fitting, but can be used for exploration 
# and to generate generate synthetic data

rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(pomp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here) #to simplify loading/saving into different folders

# Load the pomp object ----------------------------------------------------
filename <- here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)

# load values for model parameters and initial conditions -----------------
filename = here('output/var-par-definitions.RDS')
par_var_list <- readRDS(filename) 
allparvals <- par_var_list$allparvals
params_to_estimate = par_var_list$params_to_estimate
inivals_to_estimate = par_var_list$inivals_to_estimate


#set one of these to designate where the parameter values for the simulation
#should come from
#parsource = "base"
parsource = "mif"
#parsource = "pmcmc"

if (parsource == "base")
{
  simparvals = allparvals #this runs the simulation with the default parameter settings
}
if (parsource == "mif")
{
  # load table of best fit estimates produced by mif fitting ----------------------------------------------------
  filename = here('output/tables/par-table.RDS')
  mif_result_df = readRDS(filename)
  new_pars = mif_result_df[1,-(1:3)] #thise are the fitted paramters, first 3 are ID and LL/SE
  newparvals = allparvals
  newparvals[names(new_pars)] = as.numeric(new_pars) #update parameter values based on best fit
  simparvals = newparvals #this runs the simulation with the parameter settings obtained from the MIF with the lowest LL
}
if (parsource == "pmcmc")
{
  #needs to be written
}


# extend pomp model to include information beyond the last data point
horizon <- 7*6 #length of time (days to extend covariate)
# we need to supply any covariates into the future
covars <- pomp_model@covar@table
covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
covars <- as.data.frame(covars) %>%
  mutate(time = 1:n()) %>%
  rename("rel_beta_change" = covars)
# covars$rel_beta_change <- 1

#update pomp model
M2 <- pomp(pomp_model, 
           time = c(time(pomp_model), max(time(pomp_model))+seq_len(horizon)), #update time of pomp object 
           covar = covariate_table(covars, times = "time", order = "constant"), #update covariate
           )

#run simulation a number of times
sim_out <- pomp::simulate(M2, 
                       params=simparvals, #set parameter values to be used for simulation 
                       nsim=50, format="data.frame", 
                       include.data=TRUE)

# pomp runs with internal time units, add real time to results
start_date <- as.Date("2020-03-01")
end_date <- start_date + max(sim_out$time) - 1
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

#combine pomp simulations with dates
sims <- sim_out %>% left_join(dates_df)

#compute totals for each compartment and add to data frame
sims <- sims %>% mutate(Etot = E1+E2+E3+E4, Iatot = Ia1+Ia2+Ia3+Ia4, Isutot = Isu1+Isu2+Isu3+Isu4, Isdtot = Isd1+Isd2+Isd3+Isd4,Ctot = C1+C2+C3+C4, Htot = H1+H2+H3+H4)

filename = here('output/model-predictions.RDS')
saveRDS(sims,filename)

  
