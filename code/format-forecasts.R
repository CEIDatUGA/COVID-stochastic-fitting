# format-forecasts.R

rm(list = ls(all.names = TRUE))

library(tidyverse)
library(here)
library(pomp)

pomp_model <- readRDS(here("output/pomp-model.RDS"))
mcmcs <- readRDS(here("output/pmcmc-output.RDS"))

start_date <- as.Date("2020-03-01")
end_date <- start_date + max(time(mcmcs[[1]]))
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

dat <- t(pomp_model@data) %>% 
  as.data.frame() %>%
  mutate(time = 1:n()) %>%
  left_join(dates_df, by = "time")



# Collate for the forecast competition ------------------------------------

hosps_forecasts <- tibble()

for(i in 1:length(mcmcs)) {
  tmp <- mcmcs[[i]]
  tmp <- tmp@filter.traj
  tmp <- t(tmp["H_new", 1001:2000, ]) %>%
    as.data.frame() %>%
    gather(key = "iter", value = "H_new") %>%
    group_by(iter) %>%
    mutate(time = 1:n()) %>%
    ungroup() %>%
    mutate(chain = i,
           iter = as.numeric(iter)) %>%
    left_join(dates_df, by = "time") %>%
    dplyr::select(chain, iter, Date, H_new)
  
  hosps_forecasts <- bind_rows(hosps_forecasts, tmp)
}

hosps_forecasts <- hosps_forecasts %>%
  mutate(Rep = as.numeric(as.factor(paste(chain, iter)))) %>%
  dplyr::select(Rep, Date, H_new)

hosps_summary <- hosps_forecasts %>%
  group_by(Date) %>%
  summarise(lower = quantile(H_new, 0.025),
            med = median(H_new),
            upper = quantile(H_new, 0.975)) %>%
  ungroup() %>%
  mutate(Period = ifelse(Date > Sys.Date(), "Forecast", "Calibration"))

p1 <- ggplot(hosps_summary %>% filter(Date < "2020-04-24"), 
       aes(x = Date, y = med)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Period),
              alpha = 0.2, color = NA) +
  geom_line(aes(color = Period)) +
  geom_point(data = dat, aes(y = hosps)) +
  labs(y = "Number of new hospitalizations") +
  ggtitle("2-week forecast")

p2 <- ggplot(hosps_summary, aes(x = Date, y = med)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Period),
              alpha = 0.2, color = NA) +
  geom_line(aes(color = Period)) +
  geom_point(data = dat, aes(y = hosps)) +
  labs(y = "Number of new hospitalizations") +
  ggtitle("7-week forecast")

cowplot::plot_grid(p1, p2, ncol = 2)

# Save the forecasts
saveRDS(object = as.data.frame(hosps_forecasts), 
        file = here("output/hosps-forecasts-20200409.RDS"))

