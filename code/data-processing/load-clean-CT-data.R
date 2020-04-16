# load-clean-CT-data.R
# load and clean/process covid tracking data so it is ready for fitting

rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------

 library(dplyr)
 library(readr)
 library(here) #to simplify loading/saving into different folders


#################################
#US data from https://covidtracking.com/
filename_us_ct_data = here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))

if (file.exists(filename_us_ct_data)) {
  #################################
  # load already clean data locally
  #################################
  us_ct_clean <- readRDS(filename_us_ct_data)
} else {
  #################################
  # pull data from Covidtracking and process
  #################################
  us_data <- read_csv("https://covidtracking.com/api/states/daily.csv")
  #data for population size for each state/country so we can compute cases per 100K
  #not currently needed, but keep here just in case
  filename = here('data/us_popsize.rds')
  us_popsize <- readRDS(filename)
  us_clean <- us_data %>% dplyr::select(c(date,state,positive,negative,total,hospitalized,death)) %>%
    mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    group_by(state) %>% arrange(date) %>%
    mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
    mutate(Daily_Test_Negative = c(0,diff(negative))) %>% 
    mutate(Daily_Test_All = c(0,diff(total))) %>% 
    mutate(Daily_Hospitalized = c(NA,diff(hospitalized))) %>% 
    mutate(Daily_Deaths = c(NA,diff(death))) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state_full, Population_Size = total_pop, Total_Deaths = death, 
           Total_Cases = positive, Total_Hospitalized = hospitalized, 
           Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
    mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive) %>%
    select(-c(state,Total_Test_Negative,Daily_Test_Negative))

  us_ct_clean <- us_clean %>% 
    rename(cases = Daily_Cases,
           hosps = Daily_Hospitalized, 
           deaths = Daily_Deaths)
  
  # Extract Georgia data
  pomp_data <- us_ct_clean %>%
    dplyr::filter(Location == "Georgia") %>%
    dplyr::select(Date, cases, hosps, deaths) %>%
    dplyr::arrange(Date)
  
  # # Fit splines through the data
  # get_spline <- function(x) {
  #   xl <- x[is.na(x) == FALSE]
  #   y <- smooth.spline(seq_along(xl), xl, spar = 0.5)
  #   return(c(x[is.na(x)], ceiling(y$y)))
  # }
  # 
  # smooth_data <- pomp_data %>%
  #   mutate(cases = get_spline(cases),
  #          hosps = get_spline(hosps),
  #          deaths = get_spline(deaths))
  
  pseudo_data <- data.frame(
    Date = seq.Date(from = as.Date("2020-03-01"), to = Sys.Date(), by = "day"),
    hold = NA)
  
  # Merge in the NAs to complete the time series
  pomp_data <- pomp_data %>%
    right_join(pseudo_data, by = "Date") %>%
    dplyr::select(-hold) %>%
    mutate(time = 1:n()) %>%
    dplyr::select(time, cases, hosps, deaths)
  
  #saveRDS(us_ct_clean,filename_us_ct_data) #save clean file for loading unless it's outdated
  saveRDS(pomp_data,filename_us_ct_data)
}

  

