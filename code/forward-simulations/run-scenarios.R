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

mles <- pf_logliks %>%
  # filter(LogLik == max(LogLik)) %>%
  slice(1) %>%
  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)


# Run simulations ---------------------------------------------------------
weeks_ahead <- 4
num_sims <- 100

sim_sq <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                covar_action = "status_quo", param_vals = mles,
                                forecast_horizon_wks = weeks_ahead, 
                                nsims = num_sims) %>%
  mutate(SimType = "status_quo")

sim_na <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                covar_action = "no_intervention", 
                                param_vals = mles,
                                forecast_horizon_wks = weeks_ahead,
                                nsims = num_sims) %>%
  mutate(SimType = "no_intervention")

sim_sd <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                covar_action = "linear", max_beta_change = 0.3,
                                param_vals = mles, 
                                forecast_horizon_wks = weeks_ahead,
                                nsims = num_sims) %>%
  mutate(SimType = "linear_increase_sd")



# Combine the simulations -------------------------------------------------

all_sims <- bind_rows(sim_sq, sim_na, sim_sd) %>%
  dplyr::select(SimType, Period, Date, cases, hosps, deaths) %>%
  filter(SimType != "no_intervention") %>%
  gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
  group_by(SimType, Period, Date, Variable) %>%
  summarise(lower = quantile(Value, 0.025),
            ptvalue = quantile(Value, 0.5),
            upper = quantile(Value, 0.975))


# Make a data data frame for plotting -------------------------------------

filename <- here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))
pomp_data <- readRDS(filename) %>%
  dplyr::select(-Location, -time) %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  mutate(SimType = "obs", Period = "Calibration")



# Make the plots ----------------------------------------------------------

ggplot(all_sims, aes(x = Date, color = SimType, 
                     fill = SimType, linetype = Period)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue), size = 1) +
  facet_wrap(~Variable, scales = "free", ncol = 1) +
  scale_linetype_manual(values = c(1,1)) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  ylab("Number of persons") +
  theme_minimal()
ggsave("./output/figures/scenario-sims.png")
