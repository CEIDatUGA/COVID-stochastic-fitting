# load-clean-data.R
# load and clean/process Ga Dept of PH data so it is ready for fitting

# Load libraries ----------------------------------------------------------

library(dplyr)
library(here) #to simplify loading/saving into different folders


## Load data
data <- read.csv(here('/data/GA_daily_status_report_GDPH.csv'))
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

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ATT: When we add this, let's also add a new column that indicates
#      "Time_Period" as either calibration or projection. This is because
#      the forecasting/projection is going to happen in a stand-alone script
#      after the data to-date have been fitted. Thus, when add the data
#      to the pomp object, when can just use the subset where
#      Time_Period == "calibration". Then use the other time frame in the 
#      forecasting script.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
future <- data.frame(time = max(covid_ga_data$time):(max(covid_ga_data$time)+31),
                     cases = NA )
#covid_ga_data <- rbind(covid_ga_data,future)
# covid_ga_data$cases <- covid_ga_data$cases + 1 # Add one to avoid 0s for fits

# Turn data into new case reports each day, to match the simulation model
# and to follow the advice of King et al. 2015 PRSB (fit to incidence reports
# not cumulative case reports).
covid_ga_data$Daily_Cases <- c(1, diff(covid_ga_data$cases))
# covid_ga_data$cases <- covid_ga_data$cases + 1 # Add one to avoid 0s for fits
covid_ga_data$hosps <- NA  # placeholder until data stream set up
covid_ga_data$deaths <- NA  # placeholder until data stream set up

# Save cleaned data
filename = here('data/clean-GDPH-data.RDS')

saveRDS(covid_ga_data, filename)

