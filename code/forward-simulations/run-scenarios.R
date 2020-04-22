# run-scenarios.R

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(pomp)
library(tidyverse)
library(here)
source(here("code/forward-simulations/simulate_trajectories.R"))


# Load pomp model and MLEs ------------------------------------------------

pomp_model <- readRDS(here("output/pomp-model.RDS"))
pfs <- readRDS(here("output/mif-results.RDS"))$pf_runs
mifs <- readRDS(here("output/mif-results.RDS"))$mif_runs

n_ini_cond = length(pfs)
ll = list()
for (i in 1:n_ini_cond)
{
  ll1 <- sapply(pfs[[i]], logLik)
  ll[[i]] <- logmeanexp(ll1, se = TRUE)
}

# get estimated values for all parameters that were estimated for each run 
mif_coefs <- data.frame(matrix(unlist(sapply(mifs, coef)), 
                               nrow = length(mifs), 
                               byrow = T))
colnames(mif_coefs) <- names(coef(mifs[[1]]))  # names are the same for all mifs

# convert the list containing the log likelihoods for 
# each run stored in ll into a data frame
ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T))

# combine the ll_df and mif_coefs data frames. 
# Also do some cleaning/renaming
pf_logliks <- ll_df %>%
  dplyr::rename("LogLik" = X1,
                "LogLik_SE" = X2) %>%
  dplyr::mutate(MIF_ID = 1:n()) %>%
  dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
  bind_cols(mif_coefs) %>%
  dplyr::arrange(-LogLik)

all_mles <- pf_logliks %>%
  filter(LogLik > (max(LogLik)-2)) %>%
  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)


# Run simulations ---------------------------------------------------------
weeks_ahead <- 6
num_sims <- 10

out_sims <- tibble()
for(i in 1:nrow(all_mles)){
# for(i in 1:1){
  mles <- all_mles[i, ]
  
  sim_sq <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "status_quo", param_vals = mles,
                                  forecast_horizon_wks = weeks_ahead, 
                                  nsims = num_sims) %>%
    mutate(SimType = "status_quo")
  
  sim_na <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "no_intervention", 
                                  covar_no_action = 1,
                                  param_vals = mles,
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims) %>%
    mutate(SimType = "no_intervention")
  
  sim_msd <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "more_sd",
                                  param_vals = mles, 
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims) %>%
    mutate(SimType = "linear_increase_sd")
  
  sim_lsd <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "less_sd",
                                  param_vals = mles, 
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims) %>%
    mutate(SimType = "linear_decrease_sd")
  
  sim_nor <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                   covar_action = "normal",
                                   param_vals = mles, 
                                   forecast_horizon_wks = weeks_ahead,
                                   nsims = num_sims) %>%
    mutate(SimType = "return_normal")
  
  all_sims <- bind_rows(sim_sq, sim_na, sim_msd, sim_lsd, sim_nor) %>%
    mutate(mle_id = i,
           rep_id = paste0(.id, mle_id))
  out_sims <- bind_rows(out_sims, all_sims)
}

 sim_summs <- out_sims %>%
  dplyr::select(SimType, Period, Date, cases, hosps, deaths) %>%
   rename("Acases" = cases,
          "Bhosps" = hosps,
          "Cdeaths" = deaths) %>%
  gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
  group_by(SimType, Period, Date, Variable) %>%
  summarise(lower = ceiling(quantile(Value, 0.025)),
            ptvalue = ceiling(quantile(Value, 0.5)),
            upper = ceiling(quantile(Value, 0.975))) %>%
   ungroup()
 
 cumulative_summs <- out_sims %>%
   dplyr::select(SimType, Date, cases, hosps, deaths, rep_id) %>%
   rename("Acases" = cases,
          "Bhosps" = hosps,
          "Cdeaths" = deaths) %>%
   gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
   group_by(SimType, Variable, rep_id) %>%
   mutate(Value = cumsum(Value)) %>%
   group_by(SimType, Variable, Date) %>%
   summarise(min = min(Value),
             ptvalue = ceiling(quantile(Value, 0.5)),
             max = max(Value)) %>%
   ungroup() %>%
   filter(Date == max(Date)) %>%
   mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
          SimType2 = ifelse(SimType == "no_intervention", "5No intervention", SimType2),
          SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
          SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
          SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
   mutate(SimType = SimType2) %>%
   dplyr::select(-SimType2)


# Make a data data frame for plotting -------------------------------------

filename <- tail(list.files(path = here("data/"), pattern = "us-ct-clean"), 1)
fullpath <- paste0("data/", filename)
pomp_data <- readRDS(fullpath) %>%
  dplyr::select(-Location, -time) %>%
  rename("Acases" = cases,
         "Bhosps" = hosps,
         "Cdeaths" = deaths) %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  mutate(SimType = "obs", Period = "Calibration")



# Make the plots ----------------------------------------------------------

variable_names <- c(
  "Acases" = 'New cases',
  "Bhosps" ='New hospitalizations',
  "Cdeaths" = 'New deaths'
)

variable_names_cum <- c(
  "Acases" = 'Total cases',
  "Bhosps" ='Total hospitalizations',
  "Cdeaths" = 'Total deaths'
)

# Fits to data
fits <- sim_summs %>%
  filter(SimType == "status_quo") %>%
  filter(Period == "Calibration")
ggplot(fits, aes(x = Date)) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  geom_point(data = pomp_data, aes(x = Date, y = Value)) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()
ggsave(filename = here("output/figures/fits-to-data.png"), width = 8.5, height = 3, 
       units = "in", dpi = 300)


# Cumulative min/maxes 6 weeks out
scen_labs <- c("Increased social distancing",
               "Status quo",
               "Relax social distancing",
               "Return to normal",
               "No intervention")
title <- paste("Range of projections by", max(cumulative_summs$Date))
ggplot(cumulative_summs, aes(x = SimType, color = SimType)) +
  geom_segment(aes(xend = SimType, y = min, yend = max)) +
  geom_point(aes(y=min)) +
  geom_point(aes(y=max)) +
  facet_wrap(~Variable) +
  scale_colour_viridis_d(end = 0.8) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names_cum)) +
  ylab("Number of persons") +
  xlab("Scenario") +
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = scen_labs) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  ggtitle(title, subtitle = "scenarios are described above")
ggsave(filename = here("output/figures/cumulative-forecasts.png"), width = 8.5, height = 4, 
       units = "in", dpi = 300)


# Increase social distancing trajectory
ggplot(sim_summs %>%
         filter(SimType == "linear_increase_sd"), 
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  theme_minimal() +
  ggtitle("1. Increased social distancing")
ggsave("./output/figures/increased-sd-traj.png", width = 8.5, height = 3, 
       units = "in", dpi = 300)

# Status quo trajectory
ggplot(sim_summs %>%
         filter(SimType == "status_quo"), 
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  ylab("Number of persons") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  ggtitle("2. Status quo")
ggsave("./output/figures/status-quo-traj.png", width = 8.5, height = 3, 
       units = "in", dpi = 300)

# Relax social distancing trajectory
ggplot(sim_summs %>%
         filter(SimType == "linear_decrease_sd"), 
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  ylab("Number of persons") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  ggtitle("3. Relax social distancing")
ggsave("./output/figures/relax-sd-traj.png", width = 8.5, height = 3, 
       units = "in", dpi = 300)

# Return to normal
ggplot(sim_summs %>%
         filter(SimType == "return_normal"), 
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  ylab("Number of persons") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  ggtitle("4. Return to normal")
ggsave("./output/figures/return-normal-traj.png", width = 8.5, height = 3, 
       units = "in", dpi = 300)

# No intervention
ggplot(sim_summs %>%
         filter(SimType == "no_intervention"), 
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  ylab("Number of persons") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  ggtitle("5. No intervention")
ggsave("./output/figures/no-intervention-traj.png", width = 8.5, height = 3, 
       units = "in", dpi = 300)


# nada <- sim_summs %>%
#   filter(SimType == "no_intervention") %>%
#   filter(Variable == "cases")
# 
# par(mfrow = c(1,2))
# plot(cumsum(ptvalue)~Date, data = nada, type = "h", ylab = "Cumulative cases")
# plot(ptvalue~Date, data = nada, type = "l", ylab = "New cases")


# rel_beta_change = seq(0.1, 2, by = 0.01)
# log_beta_s <- -16.9
# Isd_tot = 14*4
# Isu_tot = 90*4
# E_tot = 40*4
# Ia_tot = 22*4
# C_tot = 2*4
# H_tot = 2*4
# trans_e = 2
# trans_a = 0
# trans_c = 1
# trans_h = 10
# foi = rel_beta_change * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
# plot(foi)

