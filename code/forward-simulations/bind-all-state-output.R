# This script binds the individual state-level output files
# into one large data frame that can be accessed for web
# deployment and other reports.


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)


# Loop over files and row bind --------------------------------------------

all_files <- list.files(here::here("/output/current/"), ".csv")
us_output <- tibble()  # empty storage tibble
for(do_file in all_files) {
  tmp_file <- paste0(here::here("output/current/"), "/", do_file)
  tmp <- read.csv(tmp_file, stringsAsFactors = FALSE)
  
  day1 <- tmp %>%
    filter(period == "Future") %>%
    filter(date == min(date)) %>%
    pull(date) %>%
    unique()
  day0 <-  tmp %>%
    filter(period == "Past") %>%
    filter(date == max(date)) %>%
    pull(date) %>%
    unique()
  day2 <-  tmp %>%
    filter(period == "Future") %>%
    filter(date == (min(as.Date(date))+1)) %>%
    pull(date) %>%
    unique()
  
  avg <- tmp %>%
    filter(variable == "daily_all_infections") %>%
    filter(date %in% c(day0, day2)) %>%
    group_by(sim_type) %>%
    mutate_at(.vars = 6:13, .funs = mean) %>%
    ungroup() %>%
    dplyr::select(-period, -date) %>%
    distinct() %>%
    mutate(period = "Future", date = day1)
  
  rms <- which(tmp$variable == "daily_all_infections" & tmp$date == day1)
  tmp <- tmp %>%
    slice(-rms) %>%
    bind_rows(avg)
  
  # tmp <- tmp %>%
  #   mutate(lower_80 = ifelse(date == day1 & variable == "daily_all_infections", NA, lower_80),
  #          lower_90 = ifelse(date == day1 & variable == "daily_all_infections", NA, lower_90),
  #          lower_95 = ifelse(date == day1 & variable == "daily_all_infections", NA, lower_95),
  #          mean_value = ifelse(date == day1 & variable == "daily_all_infections", NA, mean_value),
  #          median_value = ifelse(date == day1 & variable == "daily_all_infections", NA, median_value),
  #          upper_80 = ifelse(date == day1 & variable == "daily_all_infections", NA, upper_80),
  #          upper_90 = ifelse(date == day1 & variable == "daily_all_infections", NA, upper_90),
  #          upper_85 = ifelse(date == day1 & variable == "daily_all_infections", NA, upper_95))
  # tmp %>% 
  #   filter(sim_type == "status_quo") %>%
  #   filter(variable %in% c("cumulative_all_infections", "daily_all_infections")) %>%
  #   dplyr::select(period, date, variable, mean_value) %>%
  #   spread(variable, mean_value) -> test
  # data <- tmp %>%
  #   filter(is.na(sim_type)) %>%
  #   dplyr::select(location, period, date, variable, mean_value) %>%
  #   spread(variable, mean_value) %>%
  #   rename("cases" = actual_daily_cases,
  #          "deaths" = actual_daily_deaths) %>%
  #   arrange(date) %>%
  #   mutate(cases = ifelse(is.na(cases), 0, cases),
  #          deaths = ifelse(is.na(deaths), 0, deaths)) %>%
  #   mutate(cases = cumsum(cases),
  #          deaths = cumsum(deaths)) %>%
  #   rename("actual_cumulative_cases" = cases,
  #          "actual_cumulative_deaths" = deaths) %>%
  #   gather("variable", "mean_value", -date, -period, -location) %>%
  #   mutate(sim_type = NA)
  # 
  # cumulative_infected <- tmp %>%
  #   filter(variable == "daily_all_infections") %>%
  #   group_by(sim_type) %>%
  #   arrange(date) %>%
  #   mutate(lower_80 = cumsum(lower_80),
  #          lower_90 = cumsum(lower_90),
  #          lower_95 = cumsum(lower_95),
  #          mean_value = cumsum(mean_value),
  #          median_value = cumsum(median_value),
  #          upper_80 = cumsum(upper_80),
  #          upper_90 = cumsum(upper_90),
  #          upper_95 = cumsum(upper_95)) %>%
  #   mutate(variable = "cumulative_all_infections")
  # 
  # tmp <- tmp %>%
  #   bind_rows(data) %>%
  #   bind_rows(cumulative_infected)
  
  tmp <- tmp %>%
    mutate(median_value = ifelse(is.na(sim_type), mean_value, median_value))
  
  us_output <- bind_rows(us_output, tmp)
}

write_csv(us_output, here::here("/output/us_current_results.csv"))
