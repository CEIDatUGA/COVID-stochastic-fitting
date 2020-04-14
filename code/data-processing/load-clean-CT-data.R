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
    mutate(Daily_Hospitalized = c(0,diff(hospitalized))) %>% 
    mutate(Daily_Deaths = c(0,diff(death))) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state_full, Population_Size = total_pop, Total_Deaths = death, 
           Total_Cases = positive, Total_Hospitalized = hospitalized, 
           Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
    mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive) %>%
    select(-c(state,Total_Test_Negative,Daily_Test_Negative))


  #Change NA hospitalizations to zero
  us_clean$Total_Hospitalized[is.na(us_clean$Total_Hospitalized)] <- 0
  us_clean$Daily_Hospitalized[is.na(us_clean$Daily_Hospitalized)] <- 0
  
  #this bit of code is to get file names to align with what's currently in the pomp code
  us_ct_clean <- us_clean %>% rename(cases = Daily_Cases, hosps = Daily_Hospitalized, deaths = Daily_Deaths)

  
  #below copied from the master analysis script
  #includes more processing of data for pomp use
  
  #filename = here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))
  #dat <- readRDS(filename)
  dat <- us_ct_clean
  pomp_data <- dat %>%
    dplyr::filter(Location == "Georgia") %>%
    dplyr::select(Date, cases, hosps, deaths) %>%
    dplyr::arrange(Date)
  ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}
  pomp_data$cases <- ceiling(ma(pomp_data$cases))
  pomp_data$hosps <- ceiling(ma(pomp_data$hosps))
  pomp_data$deaths <- ceiling(ma(pomp_data$deaths))
  
  # decycle <- function(y, wday) {
  #   sy <- predict(smooth.spline(seq_along(y), y, spar = 0.75))$y
  #   sy[sy < 0] <- 0
  #   df <- data.frame(w = wday, y = y, sy = sy) %>%
  #     mutate(rd = y / sy) %>%
  #     filter(y > 0 & sy > 0) %>%
  #     group_by(w) %>%
  #     summarise(mrd = mean(rd))
  # }
  # 
  # wday_data <- as.matrix(pomp_data[ , -c(1,5)])
  # wday_data[which(is.na(wday_data))] <- 0
  # wday_deviations <- apply(wday_data, MARGIN = 2, decycle, 
  #                          wday = pomp_data$weekday)
  # 
  # cases <- pomp_data %>% 
  #   dplyr::select(Date, weekday, cases) %>%
  #   left_join(wday_deviations$cases, by = c("weekday" = "w")) %>%
  #   mutate(cases_adj = round(cases / mrd)) %>%
  #   dplyr::select(Date, cases_adj)
  # hosps <- pomp_data %>% 
  #   dplyr::select(Date, weekday, hosps) %>%
  #   left_join(wday_deviations$hosps, by = c("weekday" = "w")) %>%
  #   mutate(hosps_adj = round(hosps / mrd)) %>%
  #   dplyr::select(Date, hosps_adj)
  # deaths <- pomp_data %>% 
  #   dplyr::select(Date, weekday, deaths) %>%
  #   left_join(wday_deviations$deaths, by = c("weekday" = "w")) %>%
  #   mutate(deaths_adj = round(deaths / mrd)) %>%
  #   dplyr::select(Date, deaths_adj)
  # 
  # pomp_data <- cases %>%
  #   left_join(hosps, by = "Date") %>%
  #   left_join(deaths, by = "Date") %>%
  #   rename("cases" = cases_adj,
  #          "hosps" = hosps_adj,
  #          "deaths" = deaths_adj)
  
  # Create full time series of NAs to make sure pomp_data
  # starts at the time of model initialization
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
  saveRDS(pomp_data,filename_us_ct_data) #save clean file for loading unless it's outdated
}

  

