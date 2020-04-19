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


# load simulation/prediction runs with model parameters specified in the simulate-pomp script -----------------
filename = here('output/model-predictions.RDS')
sims <- readRDS(filename) 

#prep data for plotting
plot_dat <- sims %>%
  dplyr::select(Date, .id, S, Iatot, Isutot, Isdtot, Ctot, Htot, R, D, C_new, H_new, D_new, cases, hosps, deaths) %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id)  #change to long format

#plot all model compartments and data
pl <- plot_dat %>%
  ggplot(aes(x = Date, y = value, group = .id, color=.id=="data",
             size = .id=="data", alpha = .id == "data")) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_size_manual(values = c(0.5, 1)) +
  scale_alpha_manual(values = c(0.1, 1)) +
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") 

plot(pl)


# filename = here('output/model-predictions.RDS')
# saveRDS(sims,filename)

  
