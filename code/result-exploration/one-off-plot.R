# post-mif-sim-plot.R
# This is a potentially one-off for John.

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


filename = here('output/mif-results.RDS')
mif_res_list <- readRDS(filename)
mifs = mif_res_list$mif_runs
pfs = mif_res_list$pf_runs


# Compute some results -------------------------------------------------------
# for each initial condition, take the pf runs and compute mean log likelihood
n_ini_cond = length(mifs)
ll = list()
for (i in 1:n_ini_cond) #do last part not in parallel
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

print(pf_logliks)

allparvals <- pf_logliks[1, -c(1,2,3)]

# Simulate current social distancing
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

start_date <- as.Date("2020-03-01")
end_date <- start_date + max(sims$time) - 1
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

datadf <- sims %>%
  as_tibble() %>%
  left_join(dates_df) %>%
  dplyr::select(Date, .id, cases, deaths) %>%
  rename("New Cases" = cases,
         "New Deaths" = deaths) %>%
  filter(.id == "data") %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id) %>%
  mutate(Period = "Calibration")

p1 <- sims %>%
  as_tibble() %>%
  left_join(dates_df) %>%
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
  mutate(Scenario = "SD") %>%
  ungroup()

# %>%
#   ggplot(aes(x = Date, y = medvalue, fill = Period, color = Period)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
#   geom_line(size = 1) +
#   geom_point(data = datadf, aes(x = Date, y = value)) +
#   facet_wrap(~variable, scales = "free_y") +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   ggtitle("With social distancing")


# Simulate with no social distancing
M2 <- pomp_model
horizon <- 7*6
time(M2) <- c(time(pomp_model), max(time(pomp_model))+seq_len(horizon))
covars <- pomp_model@covar@table
covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
covars <- as.data.frame(covars) %>%
  mutate(time = 1:n()) %>%
  rename("rel_beta_change" = covars)
covars$rel_beta_change <- 1
M2 <- pomp(M2, covar = covariate_table(covars, times = "time", order = "constant"))

#run simulation a number of times
sims <- pomp::simulate(M2, 
                       params=allparvals, 
                       nsim=100, format="data.frame", 
                       include.data=TRUE)

start_date <- as.Date("2020-03-01")
end_date <- start_date + max(sims$time) - 1
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

datadf <- sims %>%
  as_tibble() %>%
  left_join(dates_df) %>%
  dplyr::select(Date, .id, cases, deaths) %>%
  rename("New Cases" = cases,
         "New Deaths" = deaths) %>%
  filter(.id == "data") %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id) %>%
  mutate(Period = "Calibration")

p2 <- sims %>%
  as_tibble() %>%
  left_join(dates_df) %>%
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
  mutate(Scenario = "NSD") %>%
  ungroup()

simsc <- bind_rows(p1, p2) %>%
  mutate(gr = paste0(Period, Scenario))

ggplot(data = simsc, aes(x = Date, y = medvalue)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Scenario), color = NA, alpha = 0.2) +
  geom_line(size = 1, aes(color = Scenario, linetype = Period)) +
  geom_point(data = datadf, aes(x = Date, y = value), color = "grey") +
  facet_wrap(~variable, scales = "free_y") +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  guides(color = FALSE, fill = FALSE, linetype = FALSE) +
  ylab("") 

simscum <- simsc %>%
  dplyr::select(Date, variable, Period, Scenario, gr, medvalue) %>%
  spread(key = "variable", value = medvalue) %>%
  group_by(Scenario) %>%
  mutate(`New Cases` = cumsum(`New Cases`),
         `New Deaths` = cumsum(`New Deaths`)) %>%
  gather(key = "variable", value = "medvalue", -Date, -Period, -Scenario, -gr)

ggplot(simscum, aes(x = Date, y = medvalue, color = Scenario, linetype = Period)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")


# pout <- cowplot::plot_grid(p1, p2, nrow = 2, align = "v")
# pout
# ggsave("../../Desktop/sim-plots-social-effect.pdf", pout, width = 8.5, 
#        height = 5, units = "in")
