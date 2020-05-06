# runscenarios.R
# takes results from mif fitting, runs various scenarios

runscenarios <- function(mif_res, forecast_horizon_days, nsim)
{
  

  # Load libraries ----------------------------------------------------------
  library('pomp')
  library('dplyr')
  library('tidyr')
  library('here')
  library('vctrs')

  mif_res <- readRDS(filename_temp); forecast_horizon_days = 33; nsim = 10;
  
  
  # assign pomp model and MLEs ------------------------------------------------
  mifs = mif_res$mif_runs
  pfs = mif_res$pf_runs
  pomp_model = mif_res$pomp_model
  est_partable = mif_res$est_partable
  all_partable = mif_res$all_partable
  start_date = min(mif_res$pomp_data$date)
  param_vals = mif_res$par_var_list$allparvals
  
  # get best fit parameter values for all MIF fits that are within 2 LL of the best LL
  best_partable <- all_partable %>%
                   filter(LogLik > (max(LogLik, na.rm = TRUE)-2)) %>%
                   dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)

  # Make sure there are some decent MLEs, i.e., not -inf
  # if there is at least 1 non-crazy LL, this should not be triggered
  stopifnot(nrow(best_partable) > 0)

  #manually take top 2 entries, just for testing
  best_partable = all_partable[1:2,] %>%  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)


  # New time series starting at beginning of simulation and running into the future as specified
  # also compute corresponding dates
  newtimes <- c(time(pomp_model), max(time(pomp_model))+seq_len(forecast_horizon_days))
  enddate = max(mif_res$pomp_data$date)
  newdates <- c(mif_res$pomp_data$date, seq.Date(enddate+1, enddate+forecast_horizon_days, by = "days"))
  
  #vector of names for all different scenarios to be explored
  #scenariovec = c("no_intervention","status_quo","lowest_sd","linear_increase_sd","linear_decrease_sd","return_normal")
  scenariovec = c("no_intervention","status_quo","strong_sd")
  
  #assign covariate table, then update it below
  covar_table = mif_res$covar_table  
  
  scenario_res = vector("list", length(scenariovec)) #will contain results for all simulations for all scenarios
  ct = 1 # a counter/indexer
  #loop over all scenarios
  for (scenario in scenariovec)  
    
  {
    print(sprintf('starting scenario %s',scenario))
    #for each scenario, build the appropriate co-variate
    if (scenario == "strong_sd")
    {
      covars <- covar_table$rel_beta_change #get the beta values
      lastval <- tail(covars, 1) #get the last value of beta
      minval <- 0.3  # max observed in NY
      dec <- seq(lastval, minval, length.out = 7) #7 day decrease from current to future
      final <- rep(minval, times = (forecast_horizon_days - length(dec))) #continue at low value after that
      rel_beta_change <- c(covars, dec, final) #all new values
    }
    if (scenario == "status_quo")
    {
      rel_beta_change = c(covar_table$rel_beta_change,rep(tail(covar_table$rel_beta_change,1),forecast_horizon_days))
    }
    if (scenario == "no_intervention")
    {
      rel_beta_change = rep(1,length(newtimes))
    }
    
    #build new covariate table for pomp
    covars = data.frame(rel_beta_change = rel_beta_change, time = newtimes)
    

    #update pomp object with new covariate
    M2 <- pomp(
      pomp_model,
      time = newtimes, # update time of pomp object 
      covar = covariate_table(covars, 
                              times = "time",
                              order = "constant") # update covariate
    )
    
    
    # #run the pomp model for each set of parameters that are within 2 LL of best fit
    # #for each set of parameters, we create a certain number of stochastic trajectories
    # #as specified by nsim
    # for each scenario, run over all best fits to build ensemble of forecasts
    for(i in 1:nrow(best_partable))
    {
      
      
      # Run the simulations
       sims <- pomp::simulate(M2, 
                                params = best_partable[i,],
                                nsim = nsim, 
                                format="data.frame", 
                                include.data = FALSE)
      
    }      
    
   
    
    scenario_res[[ct]]$sims = sims #save simulation result in a big list for each scenario
    scenario_res[[ct]]$scenario = scenario #save the scenario label too  
    scenario_res[[ct]]$covar = covars #save covar  
    scenario_res[[ct]]$dates = newdates #save date information  
    
    ct = ct + 1
    
  } #end loop over all scenarios

  
# Save the list containing simulations, scenario names and covariates for each scenario
# this big list of results is further processed into a format that can be used easily by our shiny app
  fname <- here('output', paste0(filename_label, '_simulation-scenarios.rds'))
saveRDS(object = scenario_res, file = fname)

return(scenario_res)

} #end function

