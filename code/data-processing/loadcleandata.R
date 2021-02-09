loadcleandata <- function(datasource, locations, timestamp, smooth = FALSE, trim = TRUE)
{

  #  ----------------------------------------------------------
  # load and clean/process data so it is ready for fitting
  #  ----------------------------------------------------------
  # data source and locations (states) to return can be specified
  
  #the code below comes from the shiny tracker. it produces more data than needed for fitting.
  #instead of deleting code, a few extra lines at the end do the filtering so we only retain what we need here
  
  # Load libraries ----------------------------------------------------------
  # library(dplyr)
  # library(tidyr)
  #library('readr')
  

  # filename = here("data",paste0("pomp_data_",timestamp,'.rds')) #if the data file for today is here, load then return from function
  # if (file.exists(filename)) {
  #   pomp_data <- readRDS(filename)    
  #   return(pomp_data)  
  # }
  #if data file is not here, go through all of the below

  #data for population size for each state/country so we can compute cases per 100K
  #not used but kept here from shiny tracker
  us_popsize <- readRDS(here::here("data","us_popsize.rds")) %>% rename(state_abr = state, state = state_full)

  #################################
  # pull data from Covidtracking and process
  #################################
  us_ct_data <- read_csv("https://covidtracking.com/api/v1/states/daily.csv")
  us_ct_clean <- us_ct_data %>% dplyr::select(c("date","state","positive","negative","total","hospitalized","death")) %>%
    mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    group_by(state) %>%
    arrange(state, date) %>%
    group_by(state) %>%
    mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
    mutate(Daily_Test_Negative = c(0,diff(negative))) %>% 
    mutate(Daily_Test_All = c(0,diff(total))) %>% 
    mutate(Daily_Hospitalized = c(0,diff(hospitalized))) %>% 
    mutate(Daily_Deaths = c(NA,diff(death))) %>% rename(state_abr = state) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = death, 
           Total_Cases = positive, Total_Hospitalized = hospitalized, 
           Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
    mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive) %>%
    select(-c(state_abr,Total_Test_Negative,Daily_Test_Negative))
  
  #add all US by summing over all variables
  all_us <- us_ct_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  us_ct_clean = rbind(us_ct_clean,all_us)
  
  #################################
  # pull data from NYT and process
  #################################
  us_nyt_data <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  us_nyt_clean <- us_nyt_data %>% dplyr::select(c(date,state,cases,deaths)) %>%
    group_by(state) %>% arrange(date) %>%
    mutate(Daily_Cases = c(0,diff(cases))) %>% 
    mutate(Daily_Deaths = c(0,diff(deaths))) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = deaths, 
           Total_Cases = cases)  %>%
    select(-state_abr)
  
  #add all US by summing over all variables
  all_us <- us_nyt_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  us_nyt_clean = rbind(us_nyt_clean,all_us)
  
  #################################
  # pull data from USAFacts and process
  #################################
  usafct_case_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
  # state_df = usafct_case_data %>% distinct(stateFIPS, .keep_all = TRUE) %>% select(3:4)
  state_df = usafct_case_data %>% distinct(stateFIPS, .keep_all = TRUE) %>% select(State, stateFIPS)
  usafct_case_clean <- usafct_case_data %>% 
    dplyr::select(-countyFIPS) %>% 
    dplyr::group_by(stateFIPS) %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>%
    left_join(state_df) %>% 
    dplyr::select(-stateFIPS) %>% 
    tidyr::pivot_longer(-State,names_to = "Date", values_to = "Total_Cases") %>%
    mutate(Date = as.Date(Date)) %>% 
    group_by(State) %>% arrange(Date) %>%
    mutate(Daily_Cases = c(0,diff(Total_Cases))) %>% 
    rename(state_abr = State) %>%
    merge(us_popsize) %>%
    rename(Location = state, Population_Size = total_pop) %>%
    select(-c(state_abr))
  
  usafct_death_data <- readr::read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
  # state_df = usafct_death_data %>% distinct(stateFIPS, .keep_all = TRUE) %>% select(3:4)
  state_df = usafct_death_data %>% distinct(stateFIPS, .keep_all = TRUE) %>% select(State, stateFIPS)
  usafct_death_clean <- usafct_death_data %>% 
    dplyr::select(-countyFIPS) %>%
    rename(stateFIPS = StateFIPS) %>% 
    mutate(stateFIPS = as.numeric(stateFIPS)) %>% 
    dplyr::group_by(stateFIPS) %>% 
    summarize_if(is.numeric, sum, na.rm=TRUE) %>%
    left_join(state_df) %>%    
    dplyr::select(-stateFIPS) %>% 
    tidyr::pivot_longer(-State, names_to = "Date", values_to = "Total_Deaths") %>%
    mutate(Date = as.Date(Date)) %>% 
    group_by(State) %>% arrange(Date) %>%
    mutate(Daily_Deaths = c(0,diff(Total_Deaths))) %>% 
    rename(state_abr = State) %>%
    merge(us_popsize) %>%
    rename(Location = state, Population_Size = total_pop) %>%
    select(-state_abr, -Population_Size)
  
  usafct_clean <- left_join(usafct_case_clean, usafct_death_clean) %>%
    group_by(Location) %>% arrange(Date)  %>%
    ungroup() %>%
    data.frame()
  
  #add all US by summing over all variables
  all_us <- usafct_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  usafct_clean = rbind(usafct_clean,all_us)
  
  
  #################################
  # pull US data from JHU github and process
  #################################
  us_jhu_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  us_jhu_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  # Clean cases
  us_jhu_cases <- us_jhu_cases %>% filter(iso3 == "USA") %>%
    dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key)) %>%
    rename(Location = Province_State)
  us_jhu_cases <- aggregate(. ~ Location, us_jhu_cases, FUN = sum)
  us_jhu_cases_clean <- tidyr::gather(us_jhu_cases, Date, Cases, -Location)
  # Clean deaths
  us_jhu_deaths <- us_jhu_deaths %>% filter(iso3 == "USA") %>%
    dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key, -Population)) %>%
    rename(Location = Province_State)
  us_jhu_deaths <- aggregate(. ~ Location, us_jhu_deaths, FUN = sum)
  us_jhu_deaths_clean <- gather(us_jhu_deaths, Date, Deaths, -Location)
  us_jhu_combined <- merge(us_jhu_cases_clean, us_jhu_deaths_clean)
  us_jhu_popsize <- us_popsize %>% rename(Location = state)
  # This merge removes cruise ship cases/death counts
  us_jhu_merge <- merge(us_jhu_combined, us_jhu_popsize)
  us_jhu_clean <- us_jhu_merge %>% mutate(Date = as.Date(as.character(Date),format="%m/%d/%y")) %>%
    group_by(Location) %>% arrange(Date) %>%
    mutate(Daily_Cases = c(0,diff(Cases))) %>%
    mutate(Daily_Deaths = c(0,diff(Deaths))) %>% 
    ungroup() %>%
    rename(Total_Deaths = Deaths, Total_Cases = Cases, Population_Size = total_pop) %>% 
    select(-state_abr) %>%
    data.frame()
  
  #add all US by summing over all variables
  all_us <- us_jhu_clean %>% group_by(Date) %>% summarize_if(is.numeric, sum, na.rm=TRUE)
  all_us$Location = "US"
  all_us$Population_Size = max(all_us$Population_Size) #because of na.rm in sum, pop size only right at end
  us_jhu_clean = rbind(us_jhu_clean,all_us)
  
  
  #################################
  # combine all data 
  #################################
  
  # give each US dataset a source label
  us_source_var = c("COV","NYT","JHU","USF")
  
  us_ct_clean$source = us_source_var[1]
  us_nyt_clean$source = us_source_var[2]
  us_jhu_clean$source = us_source_var[3]
  usafct_clean$source = us_source_var[4]
  
  #combine all US data from different sources
  us_dat <- dplyr::bind_rows(us_ct_clean, us_nyt_clean, us_jhu_clean, usafct_clean) 
  
 
  pomp_data <- us_dat %>% 
    dplyr::filter(source == datasource) %>%
    dplyr::filter(Location %in% locations) %>%
    rename(cases = Daily_Cases,
    hosps = Daily_Hospitalized, 
    deaths = Daily_Deaths) %>%
    dplyr::select(Date, Location, cases, hosps, deaths) %>% 
    rename(date = Date, location = Location) %>% #consistent lower case spelling for variables
    group_by(location) %>% 
    arrange(location, date) %>%
    group_by(location) %>%
    mutate(time = 1:n()) %>%  #careful here: 1 must align with first observation for fitting
    ungroup()
  
  # Trim leading zeros
  if(trim) {
    inf_leading_zeros <- function(x) {
      x[ 1 : min( which( x != 0 )) -1 ] <- -Inf
      x
    }
    
    pomp_data <- pomp_data %>%
      group_by(location) %>%
      arrange(date) %>%
      mutate(cases = inf_leading_zeros(cases)) %>% # set leading zeros to -Inf
      mutate(deaths = inf_leading_zeros(deaths)) %>% # set leading zeros to -Inf
      dplyr::filter(cases != -Inf | deaths != -Inf) %>%  # trim rows to first reported case or death
      mutate(time = 1:n()) %>%  #careful here: 1 must align with first observation for fitting
      ungroup() %>% 
      
      # reset -Inf values to 0
      mutate(cases = replace(cases, is.infinite(cases), 0),
             deaths = replace(deaths, is.infinite(deaths), 0))
  }
  
  # Remove bad WY data point
  pomp_data <- pomp_data %>%
    mutate(cases = ifelse(location == "Wyoming" & cases > 100 & date < "2020-05-01", NA, cases),
           cases = ifelse(location == "Maryland" & cases > 2500 & date < "2020-04-01", NA, cases),
           deaths = ifelse(location == "New York" & date == "2020-05-07", NA, deaths),
           deaths = ifelse(location == "New Jersey" & deaths > 1500, NA, deaths))
  
  # Apply 7-day moving average to the data
  ma <- function(x) {
    window <- 7
    n <- c(seq.int(window), rep(window, length(x)-window))
    xm <- ceiling(data.table::frollmean(x, n, adaptive=TRUE, na.rm = T))
    xm[is.nan(xm)] <- NA
    return(xm)
  }
  
  if(smooth) {
    pomp_data <- pomp_data %>%
      group_by(location) %>%
      arrange(date) %>%
      mutate(cases = ma(cases),
             hosps = ma(hosps),
             deaths = ma(deaths)) %>%
      ungroup()
  }
  
  # Remove negative values
  pomp_data <- pomp_data %>%
    mutate(deaths = ifelse(sign(deaths) == -1, NA, deaths),
           cases = ifelse(sign(cases) == -1, NA, cases))
  
  # Check to make sure no negative values
  neg_deaths <- pomp_data %>%
    filter(sign(deaths) == -1) %>%
    pull(deaths)
  stopifnot(length(neg_deaths) == 0)
  
  neg_cases <- pomp_data %>%
    filter(sign(cases) == -1) %>%
    pull(cases)
  stopifnot(length(neg_cases) == 0)
    
  # Save cleaned data
  # saveRDS(pomp_data,filename)
  
  return(pomp_data)
}
  

