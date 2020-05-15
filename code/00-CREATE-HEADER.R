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
source(here("code/model-setup/setparsvars.R")) #setting all parameters, specifying those that are  fitted
source(here("code/data-processing/loadcleandata.R")) #data processing function
source(here("code/data-processing/loadcleanapplemobility.R")) #function that processes and retrieves covariate
source(here("code/model-setup/makepompmodel.R")) #function that generates the pomp model


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
est_these_pars = c("log_beta_s", 
                   "frac_hosp", "frac_dead", 
                   "max_detect_par", "log_detect_inc_rate", "log_half_detect",
                   "log_sigma_dw", 
                   "log_theta_cases", "log_theta_hosps", "log_theta_deaths")
# est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
est_these_inivals = ""


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

statevec = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
# 'District of Columbia',  'Puerto Rico', 'Guam', 'American Samoa', 'Mariana Islands', 'Virgin Islands' #could include those




# --------------------------------------------------
# --------------------------------------------------
# Run the functions sourced above 
# --------------------------------------------------
# --------------------------------------------------

# run function that sets variables and parameters 
par_var_list <- setparsvars(est_these_pars = est_these_pars, est_these_inivals = est_these_inivals)

# --------------------------------------------------
# Loop over states  
# initial loop is serial and done to create the different bits needed for mif for each state 
# --------------------------------------------------

pomp_list <- vector("list",length(statevec)) #large list that will hold pomp model and other info for each state
ct = 1 #an indexer
for (location in statevec)
{
  print(sprintf('starting state %s',location))
  
  # This will be appended to each saved file 
  filename_label <- paste(location,datasource,timestamp,sep="-") 
  
  # Run data cleaning script.
  pomp_data <- loadcleandata(datasource = datasource, location = location, timestamp = timestamp) 
  
  # Get covariate 
  pomp_covar <- loadcleanapplemobility(location = location, startdate = min(pomp_data$date), enddate = max(pomp_data$date), timestamp = timestamp) 
  
  # Make a pomp model 
  pomp_model <- makepompmodel(par_var_list = par_var_list, pomp_data = pomp_data, pomp_covar = pomp_covar)

  # Save all pieces for each state in a list
  pomp_list[[ct]]$pomp_model = pomp_model 
  pomp_list[[ct]]$filename_label = filename_label
  pomp_list[[ct]]$pomp_data = pomp_data
  pomp_list[[ct]]$pomp_covar = pomp_covar
  pomp_list[[ct]]$location = location
  
  ct = ct + 1
  
} #done serial loop over all states that creates pomp object and other info 

saveRDS(pomp_list, file = here("header", "pomp_list.rds"))
saveRDS(timestamp, file = here("header", "timestamp.rds"))
saveRDS(par_var_list, file = here("header", "par_var_list.rds"))

