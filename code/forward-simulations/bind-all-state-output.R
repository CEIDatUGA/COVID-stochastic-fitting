# This script binds the individual state-level output files
# into one large data frame that can be accessed for web
# deployment and other reports.


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)


# Loop over files and row bind --------------------------------------------

all_files <- list.files(here("output/current/"), ".csv")
us_output <- tibble()  # empty storage tibble
for(do_file in all_files) {
  tmp_file <- paste0(here("output/current/"), "/", do_file)
  tmp <- read.csv(tmp_file, stringsAsFactors = FALSE)
  
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

write_csv(us_output, here("output/", "us_current_results.csv"))
