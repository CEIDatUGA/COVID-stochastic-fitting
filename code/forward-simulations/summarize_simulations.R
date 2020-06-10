summarize_simulations <- function(sims, pomp_covar, location) {
  
  # Summarize daily cases and deaths across simulation reps
  incidence_summaries <- sims %>%
    dplyr::select(SimType, Period, Date, cases, deaths) %>%
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
  cumulative_summaries <- sims %>%
    dplyr::select(SimType, Period, Date, rep_id, cases, deaths) %>%
    gather(key = "Variable", value = "Value", -SimType, -Period, -Date, -rep_id) %>%
    group_by(SimType, Variable, rep_id) %>%
    mutate(Cumulative_Value = cumsum(Value)) %>%
    ungroup() %>%
    group_by(SimType, Variable, Date) %>%
    summarise(Cumulative_Mean = ceiling(mean(Cumulative_Value, na.rm = TRUE))) %>%
    ungroup()
  
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
    mutate(Variable = "cumulative_infections")
  
  # Mobility covariate (phi)
  dates <- unique(sims$Date) %>%
    enframe(name = "time", value = "Date")
  mobility <- pomp_covar@table["rel_beta_change", ] %>%
    enframe(name = "time", value = "phi") %>%
    right_join(dates, by = "time") %>%
    fill(phi, .direction = "down") %>%
    dplyr::select(-time)
  
  # Latent trend (psi)
  latent_trend <- sims %>% 
    group_by(Date) %>%
    summarise(psi = mean(trendO, na.rm = TRUE)) %>%
    mutate(psi = (exp(psi) / (1+exp(psi))))

  
  # Combine with incidence estimates
  all_summaries <- incidence_summaries %>%
    left_join(cumulative_summaries, by = c("SimType", "Variable", "Date")) %>%
    bind_rows(infection_summaries) %>%
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
