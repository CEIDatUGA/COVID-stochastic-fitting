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
source(here::here("code/model-setup/setparsvars.R")) #setting all parameters, specifying those that are  fitted
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
state_pops <- readRDS(here::here("data/us_popsize.rds"))

# Run data cleaning script.
all_states_pomp_data <- loadcleandata(datasource = datasource, 
                                      locations = statevec, 
                                      timestamp = timestamp,
                                      smooth = FALSE)

all_states_pomp_covar <- loadcleanucmobility(
  location = statevec, 
  pomp_data = all_states_pomp_data, 
  timestamp = timestamp
) 


# --------------------------------------------------
# Loop over states  
# initial loop is serial and done to create the different bits needed for mif for each state 
# --------------------------------------------------

pomp_list <- vector("list",length(statevec)) #large list that will hold pomp model and other info for each state
ct = 1 #an indexer
for (dolocation in rev(statevec))
{
  print(sprintf('starting state %s',dolocation))
  
  # This will be appended to each saved file 
  filename_label <- paste(dolocation,datasource,timestamp,sep="-") 
  
  # Get the state's population size
  population <- state_pops %>%
    filter(state_full == dolocation) %>%
    pull(total_pop)
  
  pomp_data <- all_states_pomp_data %>%
    filter(location == dolocation)
  
  n_knots <- round(nrow(pomp_data) / 10 )
  est_these_pars = c("log_sigma_dw", "df1", "df2", "td",
                     "log_theta_cases", "log_theta_deaths")
  est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
  # est_these_inivals = ""
  knot_coefs <-  paste0("b", 1:n_knots)
  est_these_pars <- c(est_these_pars, knot_coefs)
  
  # Set the parameter values and initial conditions
  par_var_list <- setparsvars(est_these_pars = est_these_pars, 
                              est_these_inivals = est_these_inivals,
                              population = population,
                              rnaught = 6)
  
  # Get covariate 
  tmp_covar <- all_states_pomp_covar %>%
    filter(location == dolocation)
  
  covar = covariate_table(
    t = pomp_data$time,
    seas = bspline.basis(
      x=t,
      nbasis=n_knots,
      degree=3
    ),
    rel_beta_change = as.matrix(tmp_covar$rel_beta_change),
    trend_sim = as.matrix(rep(10, times = nrow(tmp_covar))),
    fit = 1,
    times="t",
    order = "constant"
  )
  
  # # Make a pomp model 
  # pomp_model <- makepompmodel(par_var_list = par_var_list, 
  #                             pomp_data = pomp_data, 
  #                             pomp_covar = pomp_covar)

  # Save all pieces for each state in a list
  # pomp_list[[ct]]$pomp_model = pomp_model 
  pomp_list[[ct]]$filename_label = filename_label
  pomp_list[[ct]]$pomp_data = pomp_data
  pomp_list[[ct]]$pomp_covar = covar
  pomp_list[[ct]]$location = dolocation
  pomp_list[[ct]]$par_var_list = par_var_list
  
  ct = ct + 1
  
} #done serial loop over all states that creates pomp object and other info 


# Save the outputs
saveRDS(pomp_list, file = here::here("header/pomp_list.rds"))
saveRDS(timestamp, file = here::here("header/timestamp.rds"))

# Create new folder for benchmark storage
datestamp <- Sys.Date()
dir.create(paste0("output/", datestamp, "/"))

# Create folder to write current fits and projections
dir.create(paste0("output/current/"))

