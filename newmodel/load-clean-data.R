# load-clean-data.R
# load and clean/process data so it is ready for fitting

# Load libraries ----------------------------------------------------------

library(dplyr)
library(here) #to simplify loading/saving into different folders


## Load data
data <- read.csv(here('GA_daily_status_report_GDPH.csv'))
data$date <- as.Date(data$date, format='%m/%d/%y')

# process/clean data
covid_ga_data <- data %>% dplyr::select(date, cases_cumulative, fatalities_cumulative) %>%
  tidyr::replace_na(list(cases_cumulative = 0, fatalities_cumulative = 0)) %>%
  dplyr::mutate(time = 1:nrow(data)) %>%
  dplyr::mutate(cases = cases_cumulative) %>%
  dplyr::select(time, cases) #using the terms 'time' for time (no matter what unit) to be more general. needs to agree with pomp specification.

# Extend data frame holding data by a month so that pomp runs 
# simulations for that long
# not used right now
future <- data.frame(time = max(covid_ga_data$time):(max(covid_ga_data$time)+31),
                     cases = NA )
#covid_ga_data <- rbind(covid_ga_data,future)
covid_ga_data$cases <- covid_ga_data$cases + 1 # Add one to avoid 0s for fits

# Save cleaned data
filename = here('outputnew/clean-data.RDS')

saveRDS(covid_ga_data, filename)

