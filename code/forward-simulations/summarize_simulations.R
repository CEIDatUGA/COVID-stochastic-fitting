summarize_simulations <- function(sims_out, pomp_data, pomp_covar, location, mle_sim) {
  
  sims <- sims_out$sims
  sim_covars <- sims_out$covars
  
  last_obs_date <- sims %>%
    filter(Period == "Past") %>%
    pull(Date) %>%
    max()
  
  # Summarize daily cases and deaths across simulation reps
  incidence_summaries <- sims %>%
    dplyr::select(SimType, Period, Date, cases, deaths, H_new) %>%
    rename("daily_cases" = cases,
           "daily_deaths" = deaths,
           "daily_hosps" = H_new) %>%
    gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
    group_by(SimType, Period, Date, Variable) %>%
    summarise(lower_95 = ceiling(quantile(Value, 0.025, na.rm = TRUE)),
              lower_90 = ceiling(quantile(Value, 0.05, na.rm = TRUE)),
              lower_80 = ceiling(quantile(Value, 0.1, na.rm = TRUE)),
              mean_value = ceiling(mean(Value, na.rm = TRUE)),
              median_value = ceiling(median(Value, na.rm = TRUE)),
              upper_80 = ceiling(quantile(Value, 0.9, na.rm = TRUE)),
              upper_90 = ceiling(quantile(Value, 0.95, na.rm = TRUE)),
              upper_95 = ceiling(quantile(Value, 0.975, na.rm = TRUE))) %>%
    ungroup() %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)
  
  # Summarize cumulative cases and deaths across simulation reps
  cumulative_summaries <- sims %>%
    dplyr::select(SimType, Period, Date, rep_id, mle_id, cases, deaths, H_new) %>%
    rename("cumulative_cases" = cases,
           "cumulative_deaths" = deaths,
           "cumulative_hosps" = H_new) %>%
    gather(key = "Variable", value = "Value", -SimType, -Period, -Date, -rep_id, -mle_id) %>%
    group_by(SimType, Variable, rep_id) %>%
    arrange(SimType, Variable, rep_id, Date) %>%
    group_by(SimType, Variable, rep_id) %>%
    mutate(Value = cumsum(Value)) %>%
    ungroup() %>%
    group_by(SimType, Period, Date, Variable) %>%
    summarise(lower_95 = ceiling(quantile(Value, 0.025, na.rm = TRUE)),
              lower_90 = ceiling(quantile(Value, 0.05, na.rm = TRUE)),
              lower_80 = ceiling(quantile(Value, 0.1, na.rm = TRUE)),
              mean_value = ceiling(mean(Value, na.rm = TRUE)),
              median_value = ceiling(median(Value, na.rm = TRUE)),
              upper_80 = ceiling(quantile(Value, 0.9, na.rm = TRUE)),
              upper_90 = ceiling(quantile(Value, 0.95, na.rm = TRUE)),
              upper_95 = ceiling(quantile(Value, 0.975, na.rm = TRUE))) %>%
    ungroup() %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)

  # cumulative_summaries <- incidence_summaries %>%
  #   arrange(SimType, Variable, Date) %>%
  #   group_by(SimType, Variable) %>%
  #   mutate_at(c("lower_95", "lower_90", "lower_80", "mean_value",
  #                       "median_value", "upper_80", "upper_90", "upper_95"),
  #             cumsum) %>%
  #   ungroup() %>%
  #   mutate(Variable = ifelse(Variable == "daily_cases",
  #                            "cumulative_cases",
  #                            Variable),
  #          Variable = ifelse(Variable == "daily_deaths",
  #                            "cumulative_deaths",
  #                            Variable),
  #          Variable = ifelse(Variable == "daily_hosps",
  #                            "cumulative_hosps",
  #                            Variable))
  
  # Infections over time
  pop_size <- sims %>%
    filter(Date == min(Date)) %>%
    pull(S) %>%
    max()
  mydiff <- function(x) {
    c(0, diff(x))
  }
  infection_summaries <- sims %>%
    dplyr::select(SimType, Period, Date, S, rep_id) %>%
    mutate(N = pop_size) %>%
    mutate(infections = N - S) %>%
    group_by(SimType, Period, rep_id) %>%
    arrange(SimType, Period, rep_id, Date) %>%
    group_by(SimType, Period, rep_id) %>%
    mutate(infections = c(0, diff(infections))) %>%
    ungroup() %>%
    arrange(rep_id, Date) %>%
    group_by(SimType, Date, Period) %>%
    summarise(lower_95 = ceiling(quantile(infections, 0.025, na.rm = TRUE)),
              lower_90 = ceiling(quantile(infections, 0.05, na.rm = TRUE)),
              lower_80 = ceiling(quantile(infections, 0.1, na.rm = TRUE)),
              mean_value = ceiling(mean(infections, na.rm = TRUE)),
              median_value = ceiling(median(infections, na.rm = TRUE)),
              upper_80 = ceiling(quantile(infections, 0.9, na.rm = TRUE)),
              upper_90 = ceiling(quantile(infections, 0.95, na.rm = TRUE)),
              upper_95 = ceiling(quantile(infections, 0.975, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(Variable = "daily_all_infections") %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)
  
  infection_cumulative <- sims %>%
    dplyr::select(SimType, Period, Date, S) %>%
    mutate(N = pop_size) %>%
    mutate(infections = N - S) %>%
    group_by(SimType, Period, Date) %>%
    summarise(lower_95 = ceiling(quantile(infections, 0.025, na.rm = TRUE)),
              lower_90 = ceiling(quantile(infections, 0.05, na.rm = TRUE)),
              lower_80 = ceiling(quantile(infections, 0.1, na.rm = TRUE)),
              mean_value = ceiling(mean(infections, na.rm = TRUE)),
              median_value = ceiling(median(infections, na.rm = TRUE)),
              upper_80 = ceiling(quantile(infections, 0.9, na.rm = TRUE)),
              upper_90 = ceiling(quantile(infections, 0.95, na.rm = TRUE)),
              upper_95 = ceiling(quantile(infections, 0.975, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(Variable = "cumulative_all_infections") %>%
    group_by(SimType, Period) %>%
    arrange(Date) %>%
    ungroup() %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)
  
  # Mobility covariate (phi)
  dates <- unique(sims$Date) %>%
    tibble::enframe(name = "time", value = "Date")
  
  # mobility <- pomp_covar@table["rel_beta_change", ] %>%
  #   tibble::enframe(name = "time", value = "phi") %>%
  #   right_join(dates, by = "time") %>%
  #   fill(phi, .direction = "down") %>%
  #   dplyr::select(-time)
  
  mobility <- sim_covars %>%
    right_join(dates, by = "time") %>%
    group_by(SimType, Date) %>%
    summarise(mean_value = mean(rel_beta_change)) %>%
    ungroup() %>%
    mutate(Variable = "mobility_trend",
           Period = ifelse(Date <= last_obs_date, "Past", "Future")) %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)
  
  # Latent trend (psi)
  latent_trend <- sim_covars %>%
    right_join(dates, by = "time") %>%
    group_by(SimType, Date) %>%
    summarise(mean_value = mean(latent_trend)) %>%
    ungroup() %>%
    mutate(mean_value = exp(mean_value) / (1+exp(mean_value))) %>%
    mutate(Variable = "latent_trend",
           Period = ifelse(Date <= last_obs_date, "Past", "Future")) %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)
  
  combined_trend <- latent_trend %>%
    bind_rows(mobility) %>%
    spread(key = Variable, value = value) %>%
    mutate(combined_trend = latent_trend * mobility_trend) %>%
    dplyr::select(-latent_trend, -mobility_trend) %>%
    gather(key = "Variable", value = "value", -SimType, -Period, -Date, -value_type)

  # Format data
  form_data <- pomp_data %>%
    ungroup() %>%
    dplyr::select(date, cases, deaths) %>%
    rename("actual_daily_cases" = cases,
           "actual_daily_deaths" = deaths,
           "Date" = date) %>%
    gather("Variable", "mean_value", -Date) %>%
    mutate(SimType = NA,
           Period = "Past") %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)
  
  cumulative_data <- pomp_data %>%
    ungroup() %>%
    dplyr::select(date, cases, deaths) %>%
    mutate(cases = ifelse(is.na(cases), 0, cases),
           deaths = ifelse(is.na(deaths), 0, deaths)) %>%
    mutate(cases = cumsum(cases),
           deaths = cumsum(deaths)) %>%
    rename("actual_cumulative_cases" = cases,
           "actual_cumulative_deaths" = deaths,
           "Date" = date) %>%
    gather("Variable", "mean_value", -Date) %>%
    mutate(SimType = NA,
           Period = "Past") %>%
    gather(key = "value_type", value = "value", -SimType, -Period, -Date, -Variable)
  
  # Combine with incidence estimates
  all_summaries <- incidence_summaries %>%
    bind_rows(cumulative_summaries) %>%
    bind_rows(infection_summaries) %>%
    bind_rows(infection_cumulative) %>%
    bind_rows(form_data) %>%
    bind_rows(cumulative_data) %>%
    bind_rows(mobility) %>%
    bind_rows(latent_trend) %>%
    bind_rows(combined_trend) %>%
    rename("sim_type" = SimType) %>%
    rename_all(tolower) %>%
    arrange(sim_type, date, variable) %>%
    as.data.frame()
  
  locationdf <- data.frame(location = rep(location, times = nrow(all_summaries)))
  
  all_summaries <- locationdf %>%
    bind_cols(all_summaries) %>%
    spread(key = value_type, value = value) %>%
    arrange(date, variable)
    # gather(key = "var_type", value = "value", -location, -sim_type, 
    #        -period, -date, -variable) %>%
    # mutate(var_type = ifelse(is.na(sim_type), "data", var_type)) %>%
    # filter(!is.na(value)) %>%
    # spread(key = variable, value = value)
  
  return(all_summaries)
}
