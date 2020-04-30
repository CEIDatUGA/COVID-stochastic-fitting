# runscenarios.R
# takes results from mif fitting, runs various scenarios

runscenarios <- function(mif_res)
{
  

  # Load libraries ----------------------------------------------------------
  library('pomp')
  library('dplyr')
  library('tidyr')
  library('here')
  library('vctrs')

  # assign pomp model and MLEs ------------------------------------------------
  mifs = mif_res$mif_runs
  pfs = mif_res$pf_runs
  pomp_model = mif_res$pomp_model
  est_partable = mif_res$est_partable
  all_partable = mif_res$all_partable
  start_date = min(mif_res$pomp_data$date)
  

  # get best fit parameter values for all MIF fits that are within 2 LL of the best LL
  best_partable <- all_partable %>%
                   filter(LogLik > (max(LogLik)-2)) %>%
                   dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)

  # Make sure there are some decent MLEs, i.e., not -inf
  # if there is at least 1 non-crazy LL, this should not be triggered
  stopifnot(nrow(best_partable) > 0)

  best_partable = all_partable[1:2,] %>%  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)


  scenariovec = c("status_quo",)
  
  #loop over all scenarios
  for (scenario in scenariovec)  
    
  {
    #for each scenario, build the appropriate co-variate
    if (scenario == "status_quo")
    {
      
    }

    #update pomp object with new covariate
    M2 <- pomp(
      pomp_model,
      time = newtimes, # update time of pomp object 
      covar = covariate_table(covars, 
                              times = "time",
                              order = "constant") # update covariate
            )
    
    
    #for each scenario, run over all best fits to build ensemble of forecasts
    for(i in 1:nrow(all_mles))
    {
      
      # Run the simulations
      sim_out <- pomp::simulate(M2, 
                                params = param_vals,
                                nsim = nsims, 
                                format="data.frame")
      
    }      
    
      
  } #

  
  
  
  
  
  
  
  
  # #  ---------------------------------------------------------
  # # Run simulations for the time period covered by the data
  # #  ---------------------------------------------------------
  # 
  # #run the pomp model for each set of parameters that are within 2 LL of best fit
  # #for each set of parameters, we create a certain number of stochastic trajectories
  # #as specified by nsim
  # obs_sim <- tibble() #this will be a large data frame holding nsim simulations for each best fit MLE 
  # 
  # for(i in 1:nrow(best_partable)) {
  # sim <- simulate(pomp_model,
  #                     params = best_partable[i, ],
  #                     nsim = 100,
  #                     format="data.frame") %>%
  #   mutate(mle_id = i)
  #   obs_sim <- bind_rows(obs_sim, sim)
  # }


#  ---------------------------------------------------------
# Run simulations for a future time period
#  ---------------------------------------------------------
weeks_ahead <- 6
num_sims <- 100

out_sims <- tibble()  # empty storage object, will contain future trajectories (stochastic)
covar_scens <- tibble()  # empty storage object, will contain covariates for different future scenarios (deterministic)

all_mles = best_partable #assign to this since that's the original name, so I don't have to edit code below



#loop over all scenarios


#loop over all best fit estimates
for(i in 1:nrow(all_mles))
{
  mles <- all_mles[i, ]
  obs <- obs_sim %>% 
    filter(mle_id %in% c(i, 999))


  sim_sql <- simulate_trajectories(pomp_model, start_date = start_date,
                                  covar_action = "status_quo", param_vals = mles,
                                  forecast_horizon_wks = weeks_ahead, 
                                  nsims = num_sims, obs_sim = obs) 
  sim_sq <- sim_sql$sims_ret %>%
    mutate(SimType = "status_quo")
  
  sim_nal <- simulate_trajectories(pomp_model, start_date = start_date,
                                  covar_action = "no_intervention", 
                                  covar_no_action = 1,
                                  param_vals = mles,
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims, obs_sim = obs)
  sim_na <- sim_nal$sims_ret %>%
    mutate(SimType = "no_intervention") %>%
    mutate(.id = as.character(.id))  # added to match the non-counterfactual returns
  
  sim_minsdl <- simulate_trajectories(pomp_model, start_date = start_date,
                                     covar_action = "lowest_sd", 
                                     param_vals = mles,
                                     forecast_horizon_wks = weeks_ahead,
                                     nsims = num_sims, obs_sim = obs)
  sim_minsd <- sim_minsdl$sims_ret %>%
    mutate(SimType = "lowest_sd") %>%
    mutate(.id = as.character(.id))  # added to match the non-counterfactual returns
  
  sim_msdl <- simulate_trajectories(pomp_model, start_date = start_date,
                                  covar_action = "more_sd",
                                  param_vals = mles, 
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims, obs_sim = obs) 
  sim_msd <- sim_msdl$sims_ret %>%
    mutate(SimType = "linear_increase_sd")
  
  sim_lsdl <- simulate_trajectories(pomp_model, start_date = start_date,
                                  covar_action = "less_sd",
                                  param_vals = mles, 
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims, obs_sim = obs)
  sim_lsd <- sim_lsdl$sims_ret %>%
    mutate(SimType = "linear_decrease_sd")
  
  sim_norl <- simulate_trajectories(pomp_model, start_date = start_date,
                                   covar_action = "normal",
                                   param_vals = mles, 
                                   forecast_horizon_wks = weeks_ahead,
                                   nsims = num_sims, obs_sim = obs)
  sim_nor <- sim_norl$sims_ret %>%
    mutate(SimType = "return_normal")
  
  all_sims <- bind_rows(sim_sq, sim_na, sim_minsd, 
                        sim_msd, sim_lsd, sim_nor) %>%
    mutate(mle_id = i,
           rep_id =  paste(.id, mle_id, sep = "-"))
  out_sims <- bind_rows(out_sims, all_sims)
  
  # Collate the covariate scenarios
  cov_sq <- sim_sql$covars %>%
    mutate(SimType = "status_quo")
  cov_na <- sim_nal$covars %>%
    mutate(SimType = "no_intervention")
  cov_minsd <- sim_minsdl$covars %>%
    mutate(SimType = "lowest_sd") 
  cov_msd <- sim_msdl$covars %>%
    mutate(SimType = "linear_increase_sd")
  cov_lsd <- sim_lsdl$covars %>%
    mutate(SimType = "linear_decrease_sd")
  cov_nor <- sim_norl$covars %>%
    mutate(SimType = "return_normal")
  all_covars <- bind_rows(cov_sq, cov_na, cov_minsd, cov_msd, cov_lsd, cov_nor)
  covar_scens <- bind_rows(covar_scens, all_covars)
}

# Save the simulations
fname <- here('output', paste0(filename_label, '_simulation-scenarios.rds'))
saveRDS(object = out_sims, file = fname)

# Save the covariates
fname2 <- here('output', paste0(filename_label, '_simulation-covariates.rds'))
saveRDS(object = covar_scens, file = fname2)

scenario_res = list()
scenario_res$sims = out_sims
scenario_res$covars = covar_scens

return(scenario_res)

}

