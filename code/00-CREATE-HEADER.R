# 00-MASTER-LOCAL-CLUSTER.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 model and for producing
# estimates of observed states and forecasts of states. 

# --------------------------------------------------
# --------------------------------------------------
# General setup
# --------------------------------------------------
# --------------------------------------------------


# Start with a clean workspace to avoid downstream errors -----------------
rm(list = ls(all.names = TRUE))

# --------------------------------------------------
# Load necessary libraries 
# --------------------------------------------------
# could be loaded here or inside functions
library('lubridate') #needs to loaded first so it doesn't mess with 'here'
library('readr')
library('doParallel')
library('foreach')
library('purrr')
library('ggplot2')
library('pomp')
library('dplyr')
library('tidyr')
library('here')
library('vctrs')


# --------------------------------------------------
# Source all needed functions/scripts
# --------------------------------------------------
source(here::here("code/model-setup/setparsvars_warm.R")) #setting all parameters, specifying those that are  fitted
source(here::here("code/data-processing/loadcleandata.R")) #data processing function
source(here::here("code/data-processing/loadcleanucmobility.R")) #function that processes and retrieves covariate


# --------------------------------------------------
# Set data source 
# --------------------------------------------------
datasource = c("COV") #one of CovidTracker (COV), NYT (NYT), JHU (JHU), USAFacts (USF)

# --------------------------------------------------
# Define parameter and variable names that are being estimated
# this is currently set up such that they are the same for each state
# if one wanted to change this it would require a bit of re-coding
# --------------------------------------------------
#This is passed to setparsvars function. 
#If set to "all", all params are estimated



# --------------------------------------------------
# Create a time-stamp variable
# Will be applied to saved results
# defined outside loop so it's the same for each state
# --------------------------------------------------
tm <- .POSIXct(Sys.time(), "US/Eastern")  # time stamp with date, hours, minutes
timestamp <- paste(lubridate::date(tm),
               stringr::str_pad(as.character(lubridate::hour(tm)), 
                                width = 2, side = "left", pad = "0"),
               stringr::str_pad(as.character(lubridate::minute(tm)), 
                                width = 2, side = "left", pad = "0"),
               sep='-')


# --------------------------------------------------
# specify which states to run as a vector 
# --------------------------------------------------

# statevec = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 
#              'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 
#              'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 
#              'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts',
#              'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana',
#              'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 
#              'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma',
#              'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina',
#              'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 
#              'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 
#              'Wyoming')
# 'District of Columbia',  'Puerto Rico', 'Guam', 'American Samoa', 'Mariana Islands', 'Virgin Islands' #could include those

statevec <- state.name  # internal R vector of 50 state names
rmids <- which(statevec %in% c("Washington", "New York", "New Jersey"))
statevec <- c(statevec[rmids], statevec[-rmids])

# --------------------------------------------------
# specify parameter initialization and mif runs for each state 
# --------------------------------------------------
state_pops <- readRDS(here::here("data/us_popsize.rds"))
statedf <- state_pops %>% 
  # warm start spec for each state 
  dplyr::mutate(init = dplyr::case_when(
    # state_full == "Indiana" ~ "2020-09-14", # example: use date of last good fit for warm start
    # state_full == "Indiana" ~ "fresh", # example: fit from scratch
    # state_full == "Indiana" ~ "last", # example: use last date for warm start
    state_full == "Maryland" ~ "2021-02-01",
    # state_full == "Massachusetts" ~ "fresh",
    # state_full == "Georgia" ~ "fresh",
    state_full == "Washington" ~ "2021-01-23",
    TRUE ~ "last" # default warm start 
  )) %>% 
  
  # R0 at beginning of epidemic for each state
  dplyr::mutate(initR0 = dplyr::case_when(
    state_full %in% c("Illinois") ~ 8,
    state_full %in% c("Indiana") ~ 8, 
    state_full %in% c("Maryland") ~ 10,
    state_full %in% c("Massachusetts") ~ 10,
    state_full %in% c("New Jersey") ~ 8,
    state_full %in% c("New York") ~ 10, 
    state_full %in% c("Ohio") ~ 8,
    state_full %in% c("Washington") ~ 8,
    TRUE ~ 6 # default initial R0
  )) %>% 
  
  # Mif runs for each state
  dplyr::mutate(mifruns = dplyr::case_when(
    state_full %in% c("California") ~ list(c(350,150)), 
    state_full %in% c("Florida") ~ list(c(350,150)), 
    state_full %in% c("Georgia") ~ list(c(350,150)), 
    state_full %in% c("Kansas") ~ list(c(350,150)), 
    state_full %in% c("Illinois") ~ list(c(350,150)), 
    state_full %in% c("Indiana") ~ list(c(350,150)), 
    state_full %in% c("Iowa") ~ list(c(350,150)), 
    state_full %in% c("Maryland") ~ list(c(350,150)),
    state_full %in% c("Massachusetts") ~ list(c(500,150)),
    state_full %in% c("Michigan") ~ list(c(350,150)),
    state_full %in% c("New Jersey") ~ list(c(350,150)),
    state_full %in% c("New York") ~ list(c(500,150)), 
    state_full %in% c("Ohio") ~ list(c(350,150)),
    state_full %in% c("Washington") ~ list(c(500,150)),
    TRUE ~ list(c(150,150)) # default mif runs vector
  ))

# Run data cleaning script.
all_states_pomp_data <- loadcleandata(datasource = datasource, 
                                      locations = statevec, 
                                      timestamp = timestamp,
                                      smooth = FALSE,
                                      trim = TRUE)  # trim leading zeros (trim to first reported case or death for each state)

all_states_pomp_covar <- loadcleanucmobility(
  location = statevec, 
  pomp_data = all_states_pomp_data, 
  timestamp = timestamp
) 


# --------------------------------------------------
# Loop over states  
# initial loop is serial and done to create the different bits needed for mif for each state 
# --------------------------------------------------

# --------------------------------------------------
# Specify parameters and initial state variables to estimate  
# --------------------------------------------------

est_these_pars = c("log_sigma_dw", "min_frac_dead", "max_frac_dead", "log_half_dead",
                   "log_theta_cases", "log_theta_deaths")
est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
# est_these_inivals = ""  # to not estimate any initial values

# initialize large list that will hold pomp model and other info for each state
pomp_list <- vector("list",length(statevec))

# --------------------------------------------------
# Loop over states  
# initial loop is serial and done to create the different bits needed for mif for each state 
# --------------------------------------------------

for (i in 1:length(statevec))
{
  dolocation <- rev(statevec)[i]
  print(sprintf('starting state %s', dolocation))
  
  # This will be appended to each saved file 
  filename_label <- dolocation
  
  # Get pomp data for location
  pomp_data <- all_states_pomp_data %>%
    filter(location == dolocation)
  
  # calculate number of knots for location
  n_knots <- round(nrow(pomp_data) / 21 )
  knot_coefs <-  paste0("b", 1:n_knots)
  
  # Get the locations's iniparvals
  initdate <- statedf %>% filter(state_full == dolocation) %>% pull(init)
  iniparvals <- initdate %>% 
    switch(
      # if init = 'fresh'
      fresh = 'fresh',
      
      # if init = 'last'
      ## edit source file location for 00-CREATE-HEADER.R
      last = readRDS(here::here(paste0("output/current/parameter-estimates-", filename_label, ".rds"))) %>% as.list(),
      
      # else
      ## does not exist for 00-Run-LOCAL.R
      ## edit source file location as needed for 00-CREATE-HEADER.R
      readRDS(here::here(paste0("output/", initdate, "/parameter-estimates-", filename_label, ".rds"))) %>% as.list()
    )
  
  # Set the parameter values and initial conditions
  par_var_list <- setparsvars_warm(iniparvals = iniparvals, # list or "fresh"
                                   est_these_pars = c(est_these_pars, knot_coefs), 
                                   est_these_inivals = est_these_inivals,
                                   population = statedf %>% 
                                     filter(state_full == dolocation) %>% pull(total_pop),
                                   n_knots = n_knots,
                                   # set R0 at beginning of epidemic
                                   rnaught = statedf %>% 
                                     filter(state_full == dolocation) %>% pull(initR0))  
  
  
  # Get covariate 
  tmp_covar <- all_states_pomp_covar %>%
    filter(location == dolocation)
  
  covar <- covariate_table(
    t = pomp_data$time,
    seas = bspline.basis(
      x=t,
      nbasis=n_knots,
      degree=3
    ),
    rel_beta_change = as.matrix(tmp_covar$rel_beta_change),
    trend_sim = as.matrix(rep(10, times = nrow(tmp_covar))),  # this is a placeholder only needed for simulation
    fit = 1,  # 1 = fitting; 0 = simulating
    times="t",
    order = "constant"
  )
  
  # Save all pieces for each state in a list
  # pomp_list[[i]]$pomp_model <- pomp_model 
  pomp_list[[i]]$filename_label <- filename_label
  pomp_list[[i]]$pomp_data <- pomp_data
  pomp_list[[i]]$pomp_covar <- covar
  pomp_list[[i]]$location <- dolocation
  pomp_list[[i]]$par_var_list <- par_var_list
  pomp_list[[i]]$mifruns <- statedf %>% 
    filter(state_full == dolocation) %>% pull(mifruns)
  
} #done serial loop over all states that creates pomp object and other info 


# Save the outputs
saveRDS(pomp_list, file = here::here("header/pomp_list.rds"))
saveRDS(timestamp, file = here::here("header/timestamp.rds"))
saveRDS(statedf, file = here::here("header/statedf.rds"))

# Create new folder for benchmark storage
datestamp <- Sys.Date()
dir.create(paste0("output/", datestamp, "/"))

# Create folder to write current fits and projections
dir.create(paste0("output/current/"))

