# load-clean-CT-data.R
# load and clean/process covid tracking data so it is ready for fitting

# Load libraries ----------------------------------------------------------

library(dplyr)
library(readr)
library(here) #to simplify loading/saving into different folders


  #################################
  # pull data from Covidtracking and process
  #################################
  us_data <- read_csv("https://covidtracking.com/api/states/daily.csv")
  #data for population size for each state/country so we can compute cases per 100K
  #not currently needed, but keep here just in case
  filename = here('data/us_popsize.RDS')
  us_popsize <- readRDS(filename)
  us_clean <- us_data %>% dplyr::select(c(date,state,positive,negative,total,hospitalized,death)) %>%
    mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    group_by(state) %>% arrange(date) %>%
    mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
    mutate(Daily_Test_Negative = c(0,diff(negative))) %>% 
    mutate(Daily_Test_All = c(0,diff(total))) %>% 
    mutate(Daily_Hospitalized = c(0,diff(hospitalized))) %>% 
    mutate(Daily_Deaths = c(0,diff(death))) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = death, 
           Total_Cases = positive, Total_Hospitalized = hospitalized, 
           Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
    mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive)
  #Change NA hospitalizations to zero
  us_clean$Total_Hospitalized[is.na(us_clean$Total_Hospitalized)] <- 0
  us_clean$Daily_Hospitalized[is.na(us_clean$Daily_Hospitalized)] <- 0
  
  # Save cleaned data
  filename = here('data/clean-CT-data.RDS')
  saveRDS(us_clean,filename)

