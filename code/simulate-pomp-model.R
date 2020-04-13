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

# allparvals <- coef(readRDS(here("output/2020-04-09-forecasts/pmcmc-output.RDS"))) %>%
#   as.data.frame() %>%
#   t() %>%
#   colMeans()
# 
lls <- readRDS(here("output/mif-results.RDS"))[[2]]
allparvals <- coef(readRDS(here("output/mif-results.RDS"))[[1]][[4]])

M2 <- pomp_model
horizon <- 7*20
time(M2) <- c(time(pomp_model), max(time(pomp_model))+seq_len(horizon))
covars <- pomp_model@covar@table

#run simulation a number of times
# allparvals["beta_reduce"] <- 1
# allparvals["log_beta_s"] <- -16
# allparvals["t_int2"] <- 3
sims <- pomp::simulate(M2, 
                       params=allparvals, 
                       nsim=1, format="data.frame", 
                       include.data=TRUE)

# filename = here('output/model-predictions.RDS')
# saveRDS(sims,filename)
start_date <- as.Date("2020-03-01")
end_date <- start_date + max(sims$time) - 1
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

pl <- sims %>%
  left_join(dates_df) %>%
  dplyr::select(Date, .id, C_new, H_new, D_new, cases, hosps, deaths) %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id) %>%
  ggplot(aes(x = Date, y = value, group = .id, color=.id=="data")) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
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





  
