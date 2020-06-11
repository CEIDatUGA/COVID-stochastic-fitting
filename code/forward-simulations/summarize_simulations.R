summarize_simulations <- function(sims, pomp_data, pomp_covar, location) {
  
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
    ungroup()
  
  # Summarize cumulative cases and deaths across simulation reps
  cumulative_summaries <- incidence_summaries %>%
    arrange(SimType, Variable, Date) %>%
    group_by(SimType, Variable) %>%
    mutate_at(c("lower_95", "lower_90", "lower_80", "mean_value", 
                        "median_value", "upper_80", "upper_90", "upper_95"), 
              cumsum) %>%
    ungroup() %>%
    mutate(Variable = ifelse(Variable == "daily_cases",
                             "cumulative_cases",
                             Variable),
           Variable = ifelse(Variable == "daily_deaths",
                             "cumulative_deaths",
                             Variable),
           Variable = ifelse(Variable == "daily_hosps",
                             "cumulative_hosps",
                             Variable))
  
  # Infections over time
  pop_size <- sims %>%
    filter(Date == min(Date)) %>%
    pull(S) %>%
    unique()
  infection_summaries <- sims %>%
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
    mutate(Variable = "cumulative_all_infections")
  
  # Mobility covariate (phi)
  dates <- unique(sims$Date) %>%
    tibble::enframe(name = "time", value = "Date")
  mobility <- pomp_covar@table["rel_beta_change", ] %>%
    tibble::enframe(name = "time", value = "phi") %>%
    right_join(dates, by = "time") %>%
    fill(phi, .direction = "down") %>%
    dplyr::select(-time)
  
  # Latent trend (psi)
  latent_trend <- sims %>% 
    group_by(Date) %>%
    summarise(psi = mean(trendO, na.rm = TRUE)) %>%
    mutate(psi = (exp(psi) / (1+exp(psi))))

  # Format data
  form_data <- pomp_data %>%
    dplyr::select(date, cases, deaths) %>%
    rename("daily_cases" = cases,
           "daily_deaths" = deaths,
           "Date" = date) %>%
    gather("Variable", "mean_value", -Date) %>%
    mutate(SimType = "data",
           Period = "Past")
  
  # Combine with incidence estimates
  all_summaries <- incidence_summaries %>%
    bind_rows(cumulative_summaries) %>%
    bind_rows(infection_summaries) %>%
    bind_rows(form_data) %>%
    left_join(mobility, by = "Date") %>%
    left_join(latent_trend, by = "Date") %>%
    rename("mobility_trend" = phi,
           "latent_trend" = psi,
           "sim_type" = SimType) %>%
    rename_all(tolower) %>%
    arrange(sim_type, date, variable) %>%
    as.data.frame()
  
  locationdf <- data.frame(location = rep(location, times = nrow(all_summaries)))
  
  all_summaries <- locationdf %>%
    bind_cols(all_summaries)
  
  return(all_summaries)
}
