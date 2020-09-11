setparsvars_warm <- function(iniparvals, est_these_pars, est_these_inivals, population, n_knots, rnaught = 6)
{

  #this is a simple script that specifies model parameters, and variable names
  #it then assigns values for initial conditions and parameters
  #the idea is that there is a single place to specify them and will be loaded by other scripts
  
  # State variables to track ------------------------------------------------
  varnames <- c("S", 
                "E1", "E2", "E3", "E4",  
                "Ia1", "Ia2", "Ia3", "Ia4", 
                "Isu1", "Isu2", "Isu3", "Isu4", 
                "Isd1", "Isd2", "Isd3", "Isd4", 
                "C1", "C2", "C3", "C4",  
                "H1", "H2", "H3", "H4",
                "C_new", "H_new", "D_new",
                "R",
                "D",
                "trendO")
  
  #initial conditions are also considered parameters and some are estimated
  #we supply them on a log scale and in the code exponentiate to ensure no negative values
  inivals <- c(S_0 = population, 
               E1_0 = log(1), #E2_0 = 40, E3_0 = 40, E4_0 = 40, 
               Ia1_0 = log(1), #Ia2_0 = 22, Ia3_0 = 22, Ia4_0 = 22, 
               Isu1_0 = log(1), # Isu2_0 = 90, Isu3_0 = 90, Isu4_0 = 90, 
               Isd1_0 = log(1), #Isd2_0 = 14, Isd3_0 = 14, Isd4_0 = 14, 
               C1_0 = 1, #C2_0 = 2, C3_0 = 2, C4_0 = 2, 
               H1_0 = 0, #H2_0 = 2, H3_0 = 2, H4_0 = 2, 
               R_0 = 0,
               D_0 = 0,
               trendO_0 = 10
  )
  
  Ntot <- sum(inivals)  # total population size - needed below

  
  # Parameters --------------------------------------------------------------
  #rev_logistic <- function(x) {
  #  log((1/x)-1)
  #}
  
  #note that a lot of parameters below are transformed versions of what the meaning specifies
  #see the Google sheet for detailed definitions and explanations
  beta <- rnaught/10
  
  fresh <- length(iniparvals) == 1 & iniparvals[[1]] == "fresh"
  
  if(fresh == TRUE) {
    parvals <- c(log_beta_s = log(beta/population), #rate of infection of symptomatic 
                 trans_e = 2, 
                 trans_a = 0, 
                 trans_c = 1,  
                 trans_h = 10,  #parameter that determines relative infectiousness of E/Ia/C classes compared to Isu/Isd 
                 
                 log_g_e = log(4/4), #rate of movement through E/Ia/Isu/Isd/C/H compartments
                 log_g_a = log(4/3.5),
                 log_g_su = log(4/6),
                 log_g_sd = log(4/3),
                 log_g_c = log(4/3),
                 log_g_h = log(4/6),
                 
                 log_max_diag = log(1), #max for factor by which movement through Isd happens faster (quicker diagnosis). value of 1 means double speed to diagnosis 
                 log_diag_inc_rate = log(1.1), #rate at which faster diagnosis ramps up to max, value close to 1 is slow ramp-up, larger values means steeper ramp-up
                 log_half_diag = log(30),  #days post simulation start at which intervention is at 50%
                 
                 max_detect_par = log(1.5),  #max fraction detected. value of 1 means 50% increase above base_detect_frac. Value of 1.5 gives 40% increase. 
                 log_detect_inc_rate = log(1.1), #speed at which fraction detected ramps up
                 log_half_detect = log(30), #days post simulation start at which intervention is at 50%
                 base_detect_frac = log(0.1), #min fraction detected at start
                 
                 frac_asym = 1.5, #fraction asymptomatic
                 frac_hosp = 2, #fraction diagnosed that go into hospital, modeled as 1/(1+exp(frac_hosp))
                 min_frac_dead = 4, #fraction hospitalized that die at end (min), modeled as above, so a value of 4 is about 1.8% dead 
                 max_frac_dead = 2, #fraction hospitalized that die at start (max). value of 2 is about 12% dead 
                 log_half_dead = log(30), #days at which death rate is half between max and min
                 
                 log_theta_cases = log(10),
                 log_theta_hosps = log(10),
                 log_theta_deaths = log(10),
                 log_sigma_dw = log(0.1)
    )
  }else{
    parvals <- c(log_beta_s = log(beta/population), #rate of infection of symptomatic 
                 trans_e = iniparvals$trans_e, 
                 trans_a = iniparvals$trans_a, 
                 trans_c = iniparvals$trans_c,  
                 trans_h = iniparvals$trans_h, #parameter that determines relative infectiousness of E/Ia/C classes compared to Isu/Isd 
                 
                 log_g_e = iniparvals$log_g_e, #rate of movement through E/Ia/Isu/Isd/C/H compartments
                 log_g_a = iniparvals$log_g_a,
                 log_g_su = iniparvals$log_g_su,
                 log_g_sd = iniparvals$log_g_sd,
                 log_g_c = iniparvals$log_g_c,
                 log_g_h = iniparvals$log_g_h,
                 
                 log_max_diag = iniparvals$log_max_diag, #max for factor by which movement through Isd happens faster (quicker diagnosis). value of 1 means double speed to diagnosis 
                 log_diag_inc_rate = iniparvals$log_diag_inc_rate, #rate at which faster diagnosis ramps up to max, value close to 1 is slow ramp-up, larger values means steeper ramp-up
                 log_half_diag = iniparvals$log_half_diag,  #days post simulation start at which intervention is at 50%
                 
                 max_detect_par = iniparvals$max_detect_par,  #max fraction detected. value of 1 means 50% increase above base_detect_frac. Value of 1.5 gives 40% increase. 
                 log_detect_inc_rate = iniparvals$log_detect_inc_rate, #speed at which fraction detected ramps up
                 log_half_detect = iniparvals$log_half_detect, #days post simulation start at which intervention is at 50%
                 base_detect_frac = iniparvals$base_detect_frac, #min fraction detected at start
                 
                 frac_asym = iniparvals$frac_asym, #fraction asymptomatic
                 frac_hosp = iniparvals$frac_hosp, #fraction diagnosed that go into hospital, modeled as 1/(1+exp(frac_hosp))
                 min_frac_dead = iniparvals$min_frac_dead, #fraction hospitalized that die at end (min), modeled as above, so a value of 4 is about 1.8% dead 
                 max_frac_dead = iniparvals$max_frac_dead, #fraction hospitalized that die at start (max). value of 2 is about 12% dead 
                 log_half_dead = iniparvals$log_half_dead, #days at which death rate is half between max and min
                 
                 log_theta_cases = iniparvals$log_theta_cases,
                 log_theta_hosps = iniparvals$log_theta_hosps,
                 log_theta_deaths = iniparvals$log_theta_deaths,
                 log_sigma_dw = iniparvals$log_sigma_dw
    )
  }
  
  
  knot_coefs <- rep(0, n_knots)
  names(knot_coefs) <- paste0("b", 1:n_knots)
  parvals = c(parvals, knot_coefs) 
  
  parnames = names(parvals)
  
  #all parameter values, including initial conditions
  allparvals = c(parvals,inivals)
  
  # select if all parameters and initial values are being estimated
  # if the est_these_pars vector is null, the default is to esitmate all
  if (est_these_pars[1] == "all")
  {
    params_to_estimate <- names(parvals) 
  } else {
    params_to_estimate <- est_these_pars
  }
  
  if (est_these_inivals[1] == "all")
  {
    inivals_to_estimate <- names(inivals)
  } else {
    inivals_to_estimate <- est_these_inivals
  }
  
    
  # Create an output list to save -------------------------------------------
  
  par_var_list = list()
  par_var_list$params_to_estimate =  params_to_estimate
  par_var_list$inivals_to_estimate =  inivals_to_estimate
  par_var_list$varnames = varnames
  par_var_list$parnames = parnames
  par_var_list$allparvals = allparvals

  return(par_var_list)
} #finish function that defines and writes variables and parameters
