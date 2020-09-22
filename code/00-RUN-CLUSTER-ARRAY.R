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
library('tibble')


# --------------------------------------------------
# Read in indexing argument for states
# --------------------------------------------------
args = (commandArgs(TRUE))
##args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
myargument = as.numeric(a)


# --------------------------------------------------
# Source all needed functions/scripts
# --------------------------------------------------
source("../code/model-fitting/runmif_allstates_array.R") #runs mif fitting
source("../code/result-exploration/exploremifresults.R") #explore mif results
source("../code/forward-simulations/simulate_trajectories.R")
source("../code/forward-simulations/runscenarios.R") #run forward simulations for best fit mif results
source("../code/model-setup/makepompmodel.R") #function that generates the pomp model
source("../code/forward-simulations/summarize_simulations.R")

# Load the pomp information
pomp_listr <- readRDS("../header/pomp_list.rds")
this_pomp <- pomp_listr[[myargument]]
n_knots <- round(nrow(this_pomp$pomp_data) / 21 )

# --------------------------------------------------
# Specify if functions that are able to run in parallel will do so
# if yes, set parallel options
# --------------------------------------------------

# turn on parallel running or not
parallel_info = list()
parallel_info$parallel_run <- TRUE
#parallel_info$num_cores <- parallel::detectCores() - 1  # alter as needed
# Add one extra core for the master process
parallel_info$num_cores <- 32  # on HPC - should ideally be M states * replicates mif runs (e.g. 10 states at a time, 20 mif runs, so 200) 

#to estimate run-time: 
#run interactively non-parallel with planned MIF settings (possibly lower MIF replicates)
#watch output to see how long 1 state takes
#main loop is done 5x, 10 states each. 
#so if core number is 10 x mif runs (replicates), speed is 5x single state (max speed)
#for less cores or more/less MIF replicates, multiply by the appropriate factor
#on my machine,  2000 particles, 150 iterations and 5 reps on 50 cores takes 75min per 
#batch of states, so total 5 x 75
#if we do 20 mif reps on 200 cores on cluster, should be same duration

# --------------------------------------------------
# Specify settings for MIF fitting
# --------------------------------------------------
# two rounds of MIF are currently hard-coded into runmif
mif_settings = list()
mif_settings$mif_num_particles  <- c(2000,2000)
# mif_settings$mif_num_iterations <- c(15,15)
mif_settings$mif_num_iterations <- this_pomp$mifruns %>% unlist()
mif_settings$pf_num_particles <- 5000#particles for filter run following mif
mif_settings$pf_reps <- 32#replicates for particle filter following mif
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$replicates <- 32 #number of different starting conditions - this is parallelized

# --------------------------------------------------
# Create a time-stamp variable
# Will be applied to saved results
# defined outside loop so it's the same for each state
# --------------------------------------------------
timestamp <- readRDS("../header/timestamp.rds")

  
# --------------------------------------------------
# Loop over states  
# done in parallel and batches of 10 states each
# --------------------------------------------------

# states_map <- tibble(state = state.name) %>%
#   mutate(num = 1:n()) %>%
#   filter(state %in% c("New York", "Washington"))
# myargument <- states_map %>%
#   slice(myargument) %>%
#   pull(num)

# Make the pomp model
pomp_model <- makepompmodel(par_var_list = this_pomp$par_var_list, 
                            pomp_data = this_pomp$pomp_data, 
                            pomp_covar = this_pomp$pomp_covar,
                            n_knots = n_knots)
this_pomp$pomp_model <- pomp_model


# if(this_pomp$location %in% c("New York", "Washington", "New Jersey")) {
#   mif_settings$mif_num_iterations <- c(350,150)
# }


# params <- this_pomp$par_var_list$allparvals
# params["df2"] <- 0.00000001
# params["td"] <- log(1)
# ttt <- simulate(pomp_model, nsim = 1, params = params, format = "data.frame")
# plot(ttt$deaths)

mif_res <- runmif_allstates(parallel_info = parallel_info, 
                            mif_settings = mif_settings, 
                            pomp_list = this_pomp, 
                            par_var_list = this_pomp$par_var_list)

# filename = paste0('../output/', this_pomp$filename_label, '_results.rds')
# saveRDS(object = mif_res, file = filename)

# --------------------------------------------------
# Another serial loop over states  
# performs mif result analysis, forecasting and data processing for each state
# --------------------------------------------------
# 
# all_scenarios = NULL #will contain all scenarios for a state as a data frame

pomp_res = this_pomp #current state
rm(this_pomp) #remove the old object
pomp_res$mif_res = mif_res #add mif results for each state to overall pomp object

mif_explore <- exploremifresults(pomp_res = pomp_res, 
                                 par_var_list = pomp_res$par_var_list,
                                 n_knots = n_knots) #compute trace plot and best param tables for mif
#add resutls computed to the pomp_res object
pomp_res$traceplot = mif_explore$traceplot
pomp_res$all_partable = mif_explore$all_partable
pomp_res$est_partable = mif_explore$est_partable
pomp_res$partable_natural = mif_explore$partable_natural

#run simulations/forecasts based on mif fits for each state
# scenario_res <- runscenarios(pomp_res = pomp_res, 
#                              par_var_list = pomp_res$par_var_list, 
#                              forecast_horizon_days = 37, 
#                              nsim = 100)
# 
# #add forward simulation results to pomp_res object
# pomp_res$scenario_res = scenario_res

pomp_res$mif_res <- NULL #to save space, one can delete the full mif results before saving

# Simulate from the model at best MLE
params <- pomp_res$all_partable %>%
  arrange(-LogLik) %>%
  slice(1) %>%
  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE) %>%
  gather() %>%
  tibble::deframe()
sim <- pomp::simulate(pomp_res$pomp_model, params = params, 
                      nsim = 100, format = "data.frame")
pomp_res$sims <- sim 

#save the completed analysis for each state to a file with time-stamp
#this file could be large, needs checking
filename = paste0('../output/', pomp_res$filename_label, '_results.rds')
saveRDS(object = pomp_res, file = filename)

# Run scenarios
pomp_res$scenarios <- runscenarios(pomp_res, par_var_list = pomp_res$par_var_list)
filename = paste0('../output/', pomp_res$filename_label, '_results.rds')
saveRDS(object = pomp_res, file = filename)  # resave...

# Summarize results
res_summary <- summarize_simulations(sims_out = pomp_res$scenarios, 
                                     pomp_data = pomp_res$pomp_data,
                                     pomp_covar = pomp_res$pomp_covar, 
                                     location = pomp_res$location,
                                     mle_sim = pomp_res$sims)

# Store for benchmarking
rundate <- strsplit(timestamp, split = "-")[[1]][1:3]
rundate <- paste0(rundate, collapse = "-")
outdir <- paste0("../output/", rundate, "/")
outfile <- paste0(outdir, pomp_res$filename_label, '.csv')
write.csv(res_summary, outfile, row.names = FALSE)

# Store for updating
outdir <- "../output/current/"
# fname <- strsplit(pomp_res$filename_label, split = "-")[[1]][1:2]
# fname <- paste0(fname, collapse = "-")
outfile <- paste0(outdir, pomp_res$filename_label, '.csv')
write.csv(res_summary, outfile, row.names = FALSE)

# Store parameter estimates
saveRDS(pomp_res$partable_natural, file = paste0("../output/current/", pomp_res$filename_label, "-params-natural.rds"))
outdir <- paste0("../output/", rundate, "/")
outfile <- paste0(outdir, 'parameter-estimates-', pomp_res$filename_label ,'.rds')
saveRDS(params, file = outfile)

outdir <- paste0("../output/current/")
outfile <- paste0(outdir, 'parameter-estimates-', pomp_res$filename_label ,'.rds')
saveRDS(params, file = outfile)


# all_df = rbind(all_df, all_scenarios) #add all states together into a long data frame, will be saved below and used by shiny


# #this is what shiny will use
# filename = here('output','results_for_shiny.rds')
# saveRDS(all_df,filename)  


# all_files <- list.files("../output/cache/2020-06-11", pattern = ".rds")
# for(do_file in all_files) {
#   pomp_res <- readRDS(paste0("../output/cache/2020-06-11/", do_file))
#   # Summarize results
#   res_summary <- summarize_simulations(sims = pomp_res$scenarios$sims,
#                                        pomp_data = pomp_res$pomp_data,
#                                        pomp_covar = pomp_res$pomp_covar,
#                                        location = pomp_res$location,
#                                        mle_sim = pomp_res$sims)
# 
#   # Store for benchmarking
#   rundate <- strsplit(pomp_res$filename_label, split = "-")[[1]][3:5]
#   rundate <- paste0(rundate, collapse = "-")
#   outdir <- paste0("../output/", rundate, "/")
#   outfile <- paste0(outdir, pomp_res$filename_label, '.csv')
#   write.csv(res_summary, outfile, row.names = FALSE)
# 
#   # Store for updating
#   outdir <- "../output/current/"
#   fname <- strsplit(pomp_res$filename_label, split = "-")[[1]][1:2]
#   fname <- paste0(fname, collapse = "-")
#   outfile <- paste0(outdir, fname, '.csv')
#   write.csv(res_summary, outfile, row.names = FALSE)
# 
#   # Store parameter estimates
#   saveRDS(pomp_res$partable_natural, file = paste0("../output/current/", fname, "-params.rds"))
# }


# all_files <- list.files("../output/", pattern = ".rds")
# for(do_file in all_files) {
#   pomp_res <- readRDS(paste0("../output/", do_file))
#   # Summarize results
#   res_summary <- summarize_simulations(sims_out = pomp_res$scenarios, 
#                                        pomp_data = pomp_res$pomp_data,
#                                        pomp_covar = pomp_res$pomp_covar, 
#                                        location = pomp_res$location,
#                                        mle_sim = pomp_res$sims)
# 
#   # Store for benchmarking
#   rundate <- strsplit(pomp_res$filename_label, split = "-")[[1]][3:5]
#   rundate <- paste0(rundate, collapse = "-")
#   outdir <- paste0("../output/", rundate, "/")
#   outfile <- paste0(outdir, pomp_res$filename_label, '.csv')
#   write.csv(res_summary, outfile, row.names = FALSE)
# 
#   # Store for updating
#   outdir <- "../output/current/"
#   fname <- strsplit(pomp_res$filename_label, split = "-")[[1]][1:2]
#   fname <- paste0(fname, collapse = "-")
#   outfile <- paste0(outdir, fname, '.csv')
#   write.csv(res_summary, outfile, row.names = FALSE)
# 
#   # Store parameter estimates
#   saveRDS(pomp_res$partable_natural, file = paste0("../output/current/", fname, "-params.rds"))
# }
# 

# pass state dataframe to /output/current
saveRDS(
  readRDS("../header/statedf.rds"), 
  file = paste0(outdir,"statedf.rds")
  )

