library(tidyverse)
library(here)

state <- "Colorado"
fname <- paste0("output/current/",state,"-COV.csv")
dat <- read.csv(fname) %>%
  filter(variable %in% c("actual_daily_cases")) %>%
  mutate(date = as.Date(date))

read.csv(fname) %>%
  filter(variable %in% c("daily_cases")) %>%
  filter(sim_type == "linear_increase_sd") %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= (Sys.Date() + 7*4)) %>%
  ggplot(aes(x = date, y = median_value)) +
  # geom_ribbon(aes(ymin = lower_80, ymax = upper_80), alpha = 0.2) +
  geom_line() +
  geom_line(data = dat, aes(x = date, y = mean_value), color = "blue") 
  # facet_wrap(~sim_type, scales = "free_y")


read.csv(fname) %>%
  filter(period == "Past") %>%
  mutate(date2 = as.Date(date)) %>%
  pull(date2) %>%
  max()
