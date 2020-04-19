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


# basic plot of all variables and data  -----------------
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
filename = here('output/figures/traj-plot.png')
ggsave(filename,pl)


# make a data frame containing data only -----------------
# used in plots below
datadf <- sims %>%
  as_tibble() %>%
  dplyr::select(Date, .id, hosps, cases, deaths) %>%
  filter(.id == "data") %>%
  rename("New Cases" = cases, "New Deaths" = deaths) %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id) %>%
  mutate(Period = "Calibration")



# nice plot showing cases and deaths  -----------------
plot_dat <- sims %>%
  as_tibble() %>%
  mutate(Period = ifelse(Date < Sys.Date(), "Calibration", "Forecast")) %>%
  dplyr::select(Date, Period, .id, cases, deaths) %>%
  rename("New Cases" = cases,
         "New Deaths" = deaths) %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id, -Period) %>%
  filter(.id != "data") %>%
  group_by(Date, variable, Period) %>%
  summarise(lower = ceiling(quantile(value, 0.1)),
            medvalue = ceiling(mean(value)),
            upper = ceiling(quantile(value, 0.9))) %>%
  mutate(Scenario = "ZNSD") %>%
  ungroup()


pl_cd <- plot_dat %>% ggplot(aes(x = Date, y = medvalue)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Scenario), color = NA, alpha = 0.2) +
  geom_line(size = 1, aes(color = Scenario, linetype = Period)) +
  geom_point(data = filter(datadf, variable != "hosps"), aes(x = Date, y = value), color = "grey") +
  facet_wrap(~variable, scales = "free_y") +
  # scale_color_brewer(type = "qual") +
  # scale_fill_brewer(type = "qual") +
  scale_color_manual(values = c("#beaed4"))+
  scale_fill_manual(values = c("#beaed4"))+
  theme_minimal() +
  guides(color = FALSE, fill = FALSE, linetype = FALSE) +
  ylab("")

plot(pl_cd)
filename = here('output/figures/cases-deaths-plot.png')
ggsave(filename,pl_cd)


# nice plot showing hospitalizations  -----------------
plot_dat_hosp <- sims %>%
  as_tibble() %>%
  mutate(Period = ifelse(Date < Sys.Date(), "Calibration", "Forecast")) %>%
  dplyr::select(Date, Period, .id, hosps) %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id, -Period) %>%
  filter(.id != "data") %>%
  group_by(Date, variable, Period) %>%
  summarise(lower = ceiling(quantile(value, 0.025)),
            medvalue = ceiling(mean(value)),
            upper = ceiling(quantile(value, 0.975))) %>%
  ungroup()


plh <- plot_dat_hosp %>% ggplot(aes(x = Date, y = medvalue, color = Period, fill = Period)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line() +
  geom_point(data = filter(datadf, variable == "hosps"), aes(x = Date, y = value), color = "grey35") +
  ylab("New Hospitalizations")

plot(plh)
filename = here('output/figures/hosp-plot.png')
ggsave(filename,plh)





# filename = here('output/model-predictions.RDS')
# saveRDS(sims,filename)

  
