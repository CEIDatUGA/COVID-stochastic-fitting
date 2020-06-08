loadcleanucmobility <- function(locations, pomp_data, timestamp)
{

  
  # Load libraries ----------------------------------------------------------
   library(dplyr)
   library(tidyr)
   library('readr')

  us_popsize <- readRDS(here("data","us_popsize.rds")) %>% 
    rename(state_abr = state, state = state_full)
  
  # Start and end dates by state
  dates <- pomp_data %>%
    group_by(location) %>%
    summarise(startdate = min(date),
              enddate = max(date)) %>%
    ungroup()
  
  alldates <- pomp_data %>%
    dplyr::select(location, date)
  
  state_map <- data.frame(state_name = state.name,
                          state_abb = state.abb, stringsAsFactors = FALSE) 
  
  allfiles <- list.files(here("data/ucmobility/state_breakdown/"))
  
  uc_mobility <- tibble()
  for(i in 1:length(allfiles)) {
    fname <- allfiles[i]
    stateabb <- strsplit(strsplit(fname, "_")[[1]][2], "[.]")[[1]][1]
    tmp <- readRDS(here("data/ucmobility/state_breakdown/", fname)) %>%
      mutate(state_abb = stateabb) %>%
      group_by(state, Date, time, state_abb) %>%
      summarise(rel_beta_change = mean(rel_beta_change)) %>%
      ungroup() %>%
      mutate(state = as.character(state))
    uc_mobility <- bind_rows(uc_mobility, tmp)
  }
  
  uc_mobility <- uc_mobility %>%
    filter(!state_abb == "DC") %>%
    left_join(state_map, by = "state_abb") %>%
    dplyr::select(-state_abb) %>%
    rename("location" = state_name,
           "date" = Date) 
  
  out <- tibble()
  for(dostate in unique(uc_mobility$location)) {

    startdate <- dates %>%
      filter(location == dostate) %>%
      pull(startdate)
    enddate <- dates %>%
      filter(location == dostate) %>%
      pull(enddate)
    tmp <- uc_mobility %>%
      filter(location == dostate) %>%
      filter(date >= startdate) %>%
      filter(date <= enddate)
    
    if (min(tmp$date)>startdate) 
    {
      early_dates = seq.Date(startdate, (min(tmp$date)-1), by = "days")
      early_df = tmp %>% slice(rep(1, length(early_dates))) #repeat 1st row as many days as we need to fill
      early_df$date = early_dates
      tmp = rbind(early_df,tmp)
    }  
    if (max(tmp$date)<enddate) 
    {
      late_dates = seq.Date((max(tmp$date)+1), enddate, by = "days")
      late_df = tmp %>% slice(rep(nrow(tmp), length(late_dates))) #repeat last row as needed
      late_df$date = late_dates
      tmp = rbind(tmp,late_df)
    }  
    
    #update time column
    tmp <- tmp %>%
      mutate(rel_beta_change = ifelse(rel_beta_change > 1, 1, rel_beta_change)) %>%
      mutate(time  = 1:n()) 
    
    out <- bind_rows(out, tmp)
  }

  return(out)
}


  

