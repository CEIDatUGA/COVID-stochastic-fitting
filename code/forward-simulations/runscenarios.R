# runscenarios.R
# takes results from mif fitting, runs various scenarios

runscenarios <- function(pomp_res, par_var_list, forecast_horizon_days = 6*7, nsim = 100)
{
  # Load libraries ----------------------------------------------------------
  #library('pomp')
  #library('dplyr')
  #library('tidyr')
  #library('here')
  #library('vctrs')

  # assign pomp model and MLEs ------------------------------------------------
  # from mif_res object, extract mif run results for each run  
  # mifs = sapply(pomp_res$mif_res, "[[", "out_mif")
  # pfs = sapply(pomp_res$mif_res, "[", "pf")
  
  #pull various things out of pomp_res object
  pomp_model = pomp_res$pomp_model
  pomp_data = pomp_res$pomp_data
  est_partable = pomp_res$est_partable
  all_partable = pomp_res$all_partable
  start_date = min(pomp_res$pomp_data$date)
  param_vals = pomp_res$par_var_list$allparvals
  covar_table = pomp_res$pomp_covar  
  location = pomp_res$location
  
  # get best fit parameter values for all MIF fits that are within 2 LL of the best LL
  all_mles <- all_partable %>% 
    filter(!is.nan(LogLik)) %>%
    filter(LogLik > (max(LogLik, na.rm = TRUE)-2)) %>%
    arrange(LogLik) %>%
    dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)
  
  # Simulate observation-timed trajectories
  obs_sim <- tibble()
  for(i in 1:nrow(all_mles)) {
    sim <- simulate(pomp_model,
                    params = all_mles[i, ],
                    nsim = 100,
                    format="data.frame") %>%
      mutate(mle_id = i) %>%
      mutate(.id = as.character(.id))
    obs_sim <- bind_rows(obs_sim, sim)
  }
  
  obs_sim2 <- simulate(pomp_model,
                       params = all_mles[1, ],
                       nsim = 1,
                       format="data.frame",
                       include.data = TRUE)
  obs_sim2 <- obs_sim2 %>%
    filter(.id == "data") %>% 
    mutate(mle_id = 999) %>%
    mutate(.id = as.character(.id))
  
  obs_sim <- bind_rows(obs_sim, obs_sim2)
  
  weeks_ahead <- forecast_horizon_days / 7
  num_sims <- nsim
  
  out_sims <- tibble()  # empty storage object
  covar_scens <- tibble()  # empty storage object
  for(i in 1:nrow(all_mles)){
    mles <- all_mles[i, ]
    obs <- obs_sim %>% 
      filter(mle_id %in% c(i, 999))
    
    sim_sql <- simulate_trajectories(pomp_model, start_date = min(pomp_data$date),
                                     covar_action = "status_quo", param_vals = mles,
                                     forecast_horizon_wks = weeks_ahead, 
                                     nsims = num_sims, obs_sim = obs) 
    sim_sq <- sim_sql$sims_ret %>%
      mutate(SimType = "status_quo")

    sim_msdl <- simulate_trajectories(pomp_model, start_date = min(pomp_data$date),
                                      covar_action = "more_sd",
                                      param_vals = mles, 
                                      forecast_horizon_wks = weeks_ahead,
                                      nsims = num_sims, obs_sim = obs) 
    sim_msd <- sim_msdl$sims_ret %>%
      mutate(SimType = "linear_increase_sd")
    
    sim_norl <- simulate_trajectories(pomp_model, start_date = min(pomp_data$date),
                                      covar_action = "normal",
                                      param_vals = mles, 
                                      forecast_horizon_wks = weeks_ahead,
                                      nsims = num_sims, obs_sim = obs)
    sim_nor <- sim_norl$sims_ret %>%
      mutate(SimType = "return_normal")
    
    all_sims <- bind_rows(sim_sq, #sim_na, #sim_minsd, sim_lsd,
                          sim_msd, sim_nor) %>%
      mutate(mle_id = i,
             rep_id =  paste(.id, mle_id, sep = "-"))
    out_sims <- bind_rows(out_sims, all_sims)
    
    # Collate the covariate scenarios
    cov_sq <- sim_sql$covars %>%
      mutate(SimType = "status_quo")
    cov_msd <- sim_msdl$covars %>%
      mutate(SimType = "linear_increase_sd")
    cov_nor <- sim_norl$covars %>%
      mutate(SimType = "return_normal")
    all_covars <- bind_rows(cov_sq, #cov_na, #cov_minsd, 
                            cov_msd, cov_nor)
    covar_scens <- bind_rows(covar_scens, all_covars)
  }
  
  scenario_res <- list(sims = out_sims,
                       covars = covar_scens)
  return(scenario_res)
  



  # # New time series starting at beginning of simulation and running into the future as specified
  # # also compute corresponding dates
  # newtimes <- c(time(pomp_model), max(time(pomp_model))+seq_len(forecast_horizon_days))
  # enddate = max(pomp_data$date)
  # newdates <- c(pomp_data$date, seq.Date(enddate+1, enddate+forecast_horizon_days, by = "days"))
  # 
  # #vector of names for all different scenarios to be explored
  # #scenariovec = c("no_intervention","status_quo","lowest_sd","linear_increase_sd","linear_decrease_sd","return_normal")
  # scenariovec = c("no_intervention","status_quo","strong_sd")
  # 
  # 
  # scenario_res = vector("list", length(scenariovec)) #will contain full results for all simulations for all scenarios
  # scenario_df = NULL # a data frame containing processed results (as used by shiny)
  # ct = 1 # a counter/indexer
  # #loop over all scenarios
  # for (scenario in scenariovec)  
  #   
  # {
  #   #print(sprintf('starting scenario %s',scenario))
  #   #for each scenario, build the appropriate co-variate
  #   if (scenario == "strong_sd")
  #   {
  #     covars <- covar_table$rel_beta_change #get the beta values
  #     lastval <- tail(covars, 1) #get the last value of beta
  #     minval <- 0.3  # max observed in NY
  #     dec <- seq(lastval, minval, length.out = 7) #7 day decrease from current to future
  #     final <- rep(minval, times = (forecast_horizon_days - length(dec))) #continue at low value after that
  #     rel_beta_change <- c(covars, dec, final) #all new values
  #   }
  #   if (scenario == "status_quo")
  #   {
  #     rel_beta_change = c(covar_table$rel_beta_change,rep(tail(covar_table$rel_beta_change,1),forecast_horizon_days))
  #   }
  #   if (scenario == "no_intervention")
  #   {
  #     rel_beta_change = rep(1,length(newtimes))
  #   }
  #   
  #   #build new covariate table for pomp
  #   covars = data.frame(rel_beta_change = rel_beta_change, time = newtimes)
  # 
  #   #update pomp object with new covariate
  #   M2 <- pomp(
  #     pomp_model,
  #     time = newtimes, # update time of pomp object 
  #     covar = covariate_table(covars, 
  #                             times = "time",
  #                             order = "constant") # update covariate
  #   )
  #   
  #   
  #   # #run the pomp model for each set of parameters that are within 2 LL of best fit
  #   # #for each set of parameters, we create a certain number of stochastic trajectories
  #   # #as specified by nsim
  #   # for each scenario, run over all best fits to build ensemble of forecasts
  #   for(i in 1:nrow(best_partable))
  #   {
  #     
  #     
  #     # Run the simulations
  #      sims <- pomp::simulate(M2, 
  #                               params = best_partable[i,],
  #                               nsim = nsim, 
  #                               format="data.frame", 
  #                               include.data = FALSE)
  #     
  #   }      
  #   
  #  
  #   
  #   scenario_res[[ct]]$sims = sims #save simulation result in a big list for each scenario
  #   scenario_res[[ct]]$scenario = scenario #save the scenario label too  
  #   scenario_res[[ct]]$covar = covars #save covar  
  #   scenario_res[[ct]]$dates = newdates #save date information  
  #  
  #   
  #   
  #   # some extra code to compute quantiles (and potentially dump the raw trajectories when saving to file)
  #   x = scenario_res[[ct]]
  #   sims = x$sims
  #   
  #   #test plot for debugging
  #   #p1 <- sims %>% ggplot(aes(x=time,y=C_new,group=.id)) +  geom_line()
  #   
  #   scenario_df <- sims %>%
  #     rename( id = ".id") %>% 
  #     group_by(id) %>%
  #     mutate(Date = x$dates) %>% 
  #     ungroup() %>%
  #     select(-time) %>%
  #     pivot_longer(cols = -c("id","Date"),  names_to = "Variable", values_to = "Value") %>%
  #     group_by(Variable, Date) %>%
  #     summarise(lower = ceiling(quantile(Value, 0.1)),
  #               ptvalue = (mean(Value)),
  #               upper = ceiling(quantile(Value, 0.9))) %>%
  #     ungroup() %>%
  #     pivot_longer(-c("Variable","Date"), names_to = 'Var_Type', values_to = "Value") %>%
  #     mutate(Scenario = x$scenario, Location = location) 
  #   
  #   #test plot for debugging
  #   #p2 <- res_df %>% filter(Variable == "C_new") %>% ggplot(aes(x=Date , y=Value, color = Var_Type)) +  geom_line()
  # 
  #   scenario_res[[ct]]$scenario_df = scenario_df 
  # 
  #   ct = ct + 1
  #   
  # } #end loop over all scenarios

  
# Save the list containing simulations, scenario names and covariates for each scenario
# this big list of results is further processed into a format that can be used easily by our shiny app
#fname <- here('output', paste0(filename_label, '_simulation-scenarios.rds'))
#saveRDS(object = scenario_res, file = fname)

# return(scenario_res)

} #end function

