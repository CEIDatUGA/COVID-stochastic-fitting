# simulate-mif-fitted-model.R
#
# This script takes the results from the run-mif fitting 
# it runs forward to simulate future trajectories basd on the best-fit model
# this script does not do fitting, but can be used for exploration/prediction 

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

# load results produced by mif fitting ----------------------------------------------------
# this is a list of mif objects for each initial condition 
# followed by pfilter objects run a specified number of times after each mif is run
filename = here('output/mif-results.RDS')
mif_res_list <- readRDS(filename)
mifs = mif_res_list$mif_runs
pfs = mif_res_list$pf_runs

# load values for model parameters and initial conditions -----------------
filename = here('output/var-par-definitions.RDS')
par_var_list <- readRDS(filename) 
allparvals <- par_var_list$allparvals
params_to_estimate = par_var_list$params_to_estimate
inivals_to_estimate = par_var_list$inivals_to_estimate




# allparvals <- coef(readRDS(here("output/2020-04-09-forecasts/pmcmc-output.RDS"))) %>%
#   as.data.frame() %>%
#   t() %>%
#   colMeans()
lls <- readRDS(here("output/mif-results.RDS"))[[2]]
allparvals <- coef(readRDS(here("output/mif-results.RDS"))[[1]][[2]])


M2 <- pomp_model
horizon <- 7*6
time(M2) <- c(time(pomp_model), max(time(pomp_model))+seq_len(horizon))
covars <- pomp_model@covar@table
covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
covars <- as.data.frame(covars) %>%
  mutate(time = 1:n()) %>%
  rename("rel_beta_change" = covars)
# covars$rel_beta_change <- 1
M2 <- pomp(M2, covar = covariate_table(covars, times = "time", order = "constant"))

#run simulation a number of times
sims <- pomp::simulate(M2, 
                       params=allparvals, 
                       nsim=100, format="data.frame", 
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
  mutate(.id = ifelse(.id == "data", "ZZZ", .id)) %>%
  ggplot(aes(x = Date, y = value, group = .id, color=.id=="ZZZ",
             size = .id=="ZZZ", alpha = .id == "ZZZ")) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_size_manual(values = c(0.5, 1)) +
  scale_alpha_manual(values = c(0.1, 1)) +
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") 

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


# # Compute some results -------------------------------------------------------
# sims <- pomp::simulate(mifs[[1]],
#                        nsim=1, format="data.frame",
#                        include.data=TRUE)
# 
# sims %>%
#   dplyr::select(time, .id, cases, hosps, deaths) %>%
#   tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
#   ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y") +
#   guides(color = FALSE)



  
