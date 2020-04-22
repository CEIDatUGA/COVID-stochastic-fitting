

simulate_trajectories <- function(
  pomp_model,
  start_date = "2020-03-01",
  covar_action = "status_quo",
  covar_no_action = NULL,
  param_vals, 
  forecast_horizon_wks = 6,
  nsims = 100) {
  
  # Number of days to project into the future
  horizon <- 7*forecast_horizon_wks # length of time (days to extend covariate)
  
  # Update pomp covariate table
  if(covar_action == "status_quo") {
    covars <- pomp_model@covar@table
    covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
    covars <- as.data.frame(covars) %>%
      mutate(time = 1:n()) %>%
      rename("rel_beta_change" = covars)
  }
  
  if(covar_action == "no_intervention") {
    covars <- pomp_model@covar@table
    maxval <- covars[1]
    covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
    covars <- as.data.frame(covars) %>%
      mutate(time = 1:n()) %>%
      rename("rel_beta_change" = covars)
    covars$rel_beta_change <- covar_no_action
  }
  
  if(covar_action == "more_sd") {
    covars <- pomp_model@covar@table
    lastval <- as.numeric(tail(t(covars), 1))
    minval <- min(covars)
    dec <- seq(lastval, minval, length.out = 7)
    final <- rep(minval, times = (horizon - length(dec)))
    covars <- c(covars, dec, final)
    covars <- as.data.frame(covars) %>%
      mutate(time = 1:n()) %>%
      rename("rel_beta_change" = covars)
  }
  
  if(covar_action == "less_sd") {
    covars <- pomp_model@covar@table
    lastval <- as.numeric(tail(t(covars), 1))
    maxval <- 0.8
    inc <- seq(lastval, maxval, length.out = 7)
    final <- rep(maxval, times = (horizon - length(inc)))
    covars <- c(covars, inc, final)
    covars <- as.data.frame(covars) %>%
      mutate(time = 1:n()) %>%
      rename("rel_beta_change" = covars)
  }
  
  # Update the pomp model with new covariates
  newtimes <- c(time(pomp_model), max(time(pomp_model))+seq_len(horizon))
  M2 <- pomp(
    pomp_model,
    time = newtimes, # update time of pomp object 
    covar = covariate_table(covars, 
                            times = "time",
                            order = "constant") # update covariate
  )
  
  # Run the simulations
  sim_out <- pomp::simulate(M2, 
                            params = param_vals,
                            nsim = nsims, 
                            format="data.frame")
  
  # pomp runs with internal time units, add real time to results
  end_date <- as.Date(start_date) + max(sim_out$time) - 1
  dates <- seq.Date(as.Date(start_date), end_date, "days") 
  dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
  
  # Combine pomp simulations with dates
  sims <- sim_out %>% 
    left_join(dates_df, by = "time") %>%
    mutate(Period = ifelse(Date > Sys.Date(), "Projection", "Calibration"))
  
  return(sims)
}