loadcleanapplemobility <- function(location, startdate, enddate)
{

  #  ----------------------------------------------------------
  # load and clean/process data so it is ready for fitting
  #  ----------------------------------------------------------
  # data source and locations (states) to return can be specified
  
  #the code below comes from the shiny tracker. it produces more data than needed for fitting.
  #instead of deleting code, a few extra lines at the end do the filtering so we only retain what we need here
  
  # Load libraries ----------------------------------------------------------
   library(dplyr)
   library(tidyr)
   library('readr')

  filename = here("data",paste0("applemobility_data_",Sys.Date(),'.rds')) #if the data file for today is here, load then return from function
  if (file.exists(filename)) {
    pomp_data <- readRDS(filename)    
    return(pomp_data)  
  }
  #if data file is not here, go through all of the below

  #data for population size for each state/country so we can compute cases per 100K
  #used here to get state names
  us_popsize <- readRDS(here("data","us_popsize.rds")) %>% rename(state_abr = state, state = state_full)

  #################################
  # pull data from Apple and process
  #################################
  x <- read_csv("https://covid19-static.cdn-apple.com/covid19-mobility-data/2007HotfixDev42/v2/en-us/applemobilitytrends-2020-04-28.csv")

  apple_data <- x %>% dplyr::filter(region == location) %>%
                      select(-geo_type, -region, -alternative_name) %>%
                      pivot_longer(- transportation_type, names_to = "date", values_to = "raw_value") %>%
                      mutate(raw_value = raw_value/max(raw_value)) %>% #standardize to have a max of 1
                      mutate(date = as.Date(date)) %>%
                      filter(date >= startdate) %>%
                      filter(date <= enddate) %>%
                      mutate(time  = 1:n()) 
    
  #data is driving data and 'wiggly' with weekly oscillations. to make make it more suitable for fitting, we'll smooth it.  
  fit <- loess(apple_data$raw_value ~ apple_data$time, degree=1, span = 0.3, data=apple_data)
  apple_data <- apple_data %>%
                mutate(rel_beta_change = fit$fitted)
  
  #plot(apple_data$time, apple_data$value,type = 'b')
  #lines(apple_data$time, fit$fitted,col = 'red')
  
  #if data spans wider range than covariate, add the 1st or last value of covariate to beginning/end
  if (min(apple_data$date)>startdate) 
  {
    early_dates = seq.Date(startdate, (min(apple_data$date)-1), by = "days")
    early_df = apple_data %>% slice(rep(1, length(early_dates))) #repeat 1st row as many days as we need to fill
    early_df$date = early_dates
    apple_data = rbind(early_df,apple_data)
  }  
  if (max(apple_data$date)<enddate) 
  {
    late_dates = seq.Date((max(apple_data$date)+1), enddate, by = "days")
    late_df = apple_data %>% slice(rep(nrow(apple_data), length(late_dates))) #repeat last row as needed
    late_df$date = late_dates
    apple_data = rbind(apple_data,late_df)
  }  
  
  #update time column
  apple_data <- apple_data %>%
                mutate(time  = 1:n()) 
  
  saveRDS(apple_data,filename)
  
  return(apple_data)
}
  

