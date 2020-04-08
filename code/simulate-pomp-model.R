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

# Load pomp simulator object ---------------------------------------------------------
filename = here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)


#load values for model parameters and initial conditions
filename = here('output/parvals.RDS')
allparvals <- readRDS(filename)


#run simulation a number of times
sims <- pomp::simulate(pomp_model, 
                       params=allparvals, 
                       nsim=10, format="data.frame", 
                       include.data=TRUE)

# filename = here('output/model-predictions.RDS')
# saveRDS(sims,filename)

pl <- sims %>%
  dplyr::select(time, .id, cases, hosps, deaths) %>%
  tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
  ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
  geom_line() +
  facet_wrap(~variable) +
  guides(color = FALSE)

plot(pl)


# Plot H1 and hosps

# sims %>%
#   dplyr::select(time, .id, hosps, H1) %>%
#   filter(.id != "data") %>%
#   tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
#   group_by(time, variable) %>%
#   summarise(MeanLine = mean(value)) -> meantraj
# 
# sims %>%
#   dplyr::select(time, .id, hosps, H1) %>%
#   filter(.id != "data") %>%
#   tidyr::gather(key = "variable", value = "value", -time, -.id) -> thesims
# 
# ggplot() +
#   geom_line(data = thesims, aes(x = time, y = value, group = .id), alpha = 0.05) +
#   geom_line(data = meantraj, aes(x = time, y = MeanLine), size = 1, color = "red") +
#   facet_wrap(~variable) +
#   guides(color = FALSE)





  
