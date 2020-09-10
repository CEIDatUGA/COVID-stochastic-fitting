# Script to run the workflow locally for trouble-shooting

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
# source(here::here("code/model-setup/setparsvars.R")) #set all parameters, specifying those to fit
source(here::here("code/model-setup/setparsvars_warm.R")) #set all parameters, specifying those to fit (warm start)
source(here::here("code/data-processing/loadcleandata.R")) #data processing function
source(here::here("code/data-processing/loadcleanucmobility.R")) #function that processes and retrieves covariate
source(here::here("code/model-fitting/runmif_allstates_array.R")) #runs mif fitting
source(here::here("code/result-exploration/exploremifresults.R")) #explore mif results
source(here::here("code/forward-simulations/simulate_trajectories.R"))
source(here::here("code/forward-simulations/runscenarios.R")) #run forward simulations for best fit mif results
source(here::here("code/model-setup/makepompmodel.R")) #function that generates the pomp model
source(here::here("code/forward-simulations/summarize_simulations.R"))


# --------------------------------------------------
# Set data source 
# --------------------------------------------------
datasource = c("COV") #one of CovidTracker (COV), NYT (NYT), JHU (JHU), USAFacts (USF)

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
statevec <- "Washington"

# --------------------------------------------------
# specify parameter initialization and mif runs for each state 
# --------------------------------------------------
state_pops <- readRDS(here::here("data/us_popsize.rds"))
statedf <- state_pops %>% 
  
  # warm start spec for each state
  dplyr::mutate(init = dplyr::case_when(
    state_full %in% c("New York") ~ "fresh", # fit from scratch
    state_full == "California" ~ "2020-07-23", # specify date of last good fit for warm start
    TRUE ~ "last" # default to last fit for warm start
  )) %>% 
  
  # R0 at beginning of epidemic for each state
  dplyr::mutate(initR0 = dplyr::case_when(
    state_full %in% c("Washington", "New York", "New Jersey") ~ 10, 
    TRUE ~ 6 # default initial R0
  )) %>% 
  
  # Mif runs for each state
  dplyr::mutate(mifruns = dplyr::case_when(
    state_full %in% c("Washington", "New York", "New Jersey") ~ list(c(350,150)),
    TRUE ~ list(c(150,150)) # default mif runs vector
  ))
  
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
  n_knots <- round(nrow(pomp_data) / 10 )
  knot_coefs <-  paste0("b", 1:n_knots)

  # Get the locations's iniparvals
  initdate <- statedf %>% filter(state_full == dolocation) %>% pull(init)
  iniparvals <- initdate %>% 
    switch(
      # if init = 'fresh'
      fresh = 'fresh',
      
      # if init = 'last'
      ## edit source file location for 00-CREATE-HEADER.R
      last = readRDS(here::here(paste0("output-local/", filename_label, "_results.rds")))$all_partable %>% 
        dplyr::arrange(-LogLik) %>% dplyr::slice(1) %>% as.list(),
      
      # else
      ## does not exist for 00-Run-LOCAL.R
      ## edit source file location as needed for 00-CREATE-HEADER.R
      readRDS(here::here(paste0("output/", initdate, "/", filename_label, "_results.rds")))$all_partable %>% 
        dplyr::arrange(-LogLik) %>% dplyr::slice(1) %>% as.list()
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


### MODEL FITTING #######
# turn on parallel running or not
parallel_info <- list()
parallel_info$parallel_run <- TRUE
parallel_info$num_cores <- 2  # on HPC - should ideally be M states * replicates mif runs (e.g. 10 states at a time, 20 mif runs, so 200) 

# --------------------------------------------------
# Specify settings for MIF fitting
# --------------------------------------------------
# two rounds of MIF are currently hard-coded into runmif
mif_settings <- list()
mif_settings$mif_num_particles  <- c(200,200)
mif_settings$mif_num_iterations <- c(15,15)
mif_settings$pf_num_particles <- 50  # particles for filter run following mif
mif_settings$pf_reps <- 2  # replicates for particle filter following mif
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$replicates <- 2  # number of different starting conditions - this is parallelized

this_pomp <- pomp_list[[1]]
n_knots <- round(nrow(this_pomp$pomp_data) / 10 )

# Make the pomp model
pomp_model <- makepompmodel(par_var_list = this_pomp$par_var_list, 
                            pomp_data = this_pomp$pomp_data, 
                            pomp_covar = this_pomp$pomp_covar,
                            n_knots = n_knots)
this_pomp$pomp_model <- pomp_model

#browser()

# MIF
mif_res <- runmif_allstates(parallel_info = parallel_info, 
                            mif_settings = mif_settings, 
                            pomp_list = this_pomp, 
                            par_var_list = this_pomp$par_var_list)

pomp_res <- this_pomp #current state
rm(this_pomp) #remove the old object
pomp_res$mif_res <- mif_res #add mif results for each state to overall pomp object

mif_explore <- exploremifresults(pomp_res = pomp_res, 
                                 par_var_list = pomp_res$par_var_list,
                                 n_knots = n_knots) #compute trace plot and best param tables for mif
#add resutls computed to the pomp_res object
pomp_res$traceplot <- mif_explore$traceplot
pomp_res$all_partable <- mif_explore$all_partable
pomp_res$est_partable <- mif_explore$est_partable
pomp_res$partable_natural <- mif_explore$partable_natural

# Simulate from the model at best MLE
params <- pomp_res$all_partable %>%
  slice(1) %>%
  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE) %>%
  gather() %>%
  tibble::deframe()
sim <- pomp::simulate(pomp_res$pomp_model, params = params, 
                      nsim = 100, format = "data.frame")
pomp_res$sims <- sim 

#save the completed analysis for each state to a file with time-stamp
#this file could be large, needs checking
filename <- paste0('output-local/', pomp_res$filename_label, '_results.rds')
saveRDS(object = pomp_res, file = filename)

# Run scenarios
pomp_res$scenarios <- runscenarios(pomp_res, par_var_list = pomp_res$par_var_list)
filename <- paste0('output-local/', pomp_res$filename_label, '_results.rds')
saveRDS(object = pomp_res, file = filename)  # resave/overwrite...

# Summarize results
res_summary <- summarize_simulations(sims_out = pomp_res$scenarios, 
                                     pomp_data = pomp_res$pomp_data,
                                     pomp_covar = pomp_res$pomp_covar, 
                                     location = pomp_res$location,
                                     mle_sim = pomp_res$sims)

outfile <- paste0("output-local/", pomp_res$filename_label, '.csv')
write.csv(res_summary, outfile, row.names = FALSE)
