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
weeks_ahead <- 7
num_sims <- 10

out_sims <- tibble()
for(i in 1:nrow(all_mles)){
  mles <- all_mles[i, ]
  
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
  
  all_sims <- bind_rows(sim_sq, sim_na, sim_msd, sim_lsd) %>%
    mutate(mle_id = i)
  out_sims <- bind_rows(out_sims, all_sims)
}

 sim_summs <- out_sims %>%
  dplyr::select(SimType, Period, Date, cases, hosps, deaths) %>%
  gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
  group_by(SimType, Period, Date, Variable) %>%
  summarise(lower = ceiling(quantile(Value, 0.025)),
            ptvalue = ceiling(quantile(Value, 0.5)),
            upper = ceiling(quantile(Value, 0.975))) %>%
   ungroup()
 
 cumulative_summs <- out_sims %>%
   dplyr::select(SimType, Date, cases, hosps, deaths) %>%
   gather(key = "Variable", value = "Value", -SimType, -Date) %>%
   group_by(SimType, Date, Variable) %>%
   mutate(Value = cumsum(Value)) %>%
   summarise(lower = ceiling(quantile(Value, 0.025)),
             ptvalue = ceiling(quantile(Value, 0.5)),
             upper = ceiling(quantile(Value, 0.975))) %>%
   ungroup() %>%
   filter(Date == max(Date)) %>%
   filter(Variable == "cases")


# Make a data data frame for plotting -------------------------------------

filename <- here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))
pomp_data <- readRDS(filename) %>%
  dplyr::select(-Location, -time) %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  mutate(SimType = "obs", Period = "Calibration")



# Make the plots ----------------------------------------------------------

ggplot(sim_summs, aes(x = Date, color = SimType, 
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





rel_beta_change = seq(0.1, 2, by = 0.01)
log_beta_s <- -16.9
Isd_tot = 14*4
Isu_tot = 90*4
E_tot = 40*4
Ia_tot = 22*4
C_tot = 2*4
H_tot = 2*4
trans_e = 2
trans_a = 0
trans_c = 1
trans_h = 10
foi = rel_beta_change * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
plot(foi)

