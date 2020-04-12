# load-clean-data.R
# load and clean/process Ga Dept of PH data so it is ready for fitting

# Load libraries ----------------------------------------------------------

library(dplyr)
library(readr)
library(here) #to simplify loading/saving into different folders

#################################
#GA data from GA DPH
filename_ga_gdph = here('data',paste0("ga-gdph-cleandata-",Sys.Date(),'.rds'))

if (file.exists(filename_ga_gdph)) {
  #################################
  # load already clean data locally
  #################################
  ga_gdph_clean <- readRDS(filename_ga_gdph)
} else {
  #################################
  # load data from GA DPH spreadsheet
  #################################
  data <- read.csv(here('/data/GA_daily_status_report_GDPH.csv'))
  data$date <- as.Date(data$date, format='%m/%d/%y')

  # process/clean data
  ga_gdph_clean <- data %>% dplyr::select(date, cases_cumulative, fatalities_cumulative) %>%
    tidyr::replace_na(list(cases_cumulative = 0, fatalities_cumulative = 0)) %>%
    dplyr::mutate(time = 1:nrow(data)) %>%
    dplyr::mutate(cases = cases_cumulative) %>%
    dplyr::select(time, cases) %>%   #using the terms 'time' for time (no matter what unit) to be more general. needs to agree with pomp specification.
    dplyr::rename(Total_Cases = cases) %>%
    dplyr::mutate(cases = c(0, diff(Total_Cases))) %>%
    dplyr::mutate(hosps = NA, deaths = NA)
  
  # NOTE: 
  # Final data frame MUST have columns named "cases", "hosps", and "deaths" and MUST be
  # new daily reports to match the pomp model as written.
  
  saveRDS(ga_gdph_clean,filename_ga_gdph)
}



## Old stuff

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ATT: When we add this, let's also add a new column that indicates
#      "Time_Period" as either calibration or projection. This is because
#      the forecasting/projection is going to happen in a stand-alone script
#      after the data to-date have been fitted. Thus, when add the data
#      to the pomp object, when can just use the subset where
#      Time_Period == "calibration". Then use the other time frame in the 
#      forecasting script.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#future <- data.frame(time = max(covid_ga_data$time):(max(covid_ga_data$time)+31),                     cases = NA )
#covid_ga_data <- rbind(covid_ga_data,future)
# covid_ga_data$cases <- covid_ga_data$cases + 1 # Add one to avoid 0s for fits

# Turn data into new case reports each day, to match the simulation model
# and to follow the advice of King et al. 2015 PRSB (fit to incidence reports
# not cumulative case reports).
# covid_ga_data$Daily_Cases <- c(1, diff(covid_ga_data$cases))
# covid_ga_data$cases <- covid_ga_data$cases + 1 # Add one to avoid 0s for fits


