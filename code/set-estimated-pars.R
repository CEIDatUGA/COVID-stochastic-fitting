# set-estimated-pars.R
rm(list = ls(all.names = TRUE))

#this is a simple script that defines that parameters (and initial conditions) we want to estimate
#it is used by the various fitting procedures
#the idea is that there is a single place to specify them and will be loaded by other scripts

library(here)

# Specify the parameters we want to estimate (i.e., those to vary) --------
# This is a subset of all parameters.
params_to_estimate <- c(
  # rate of infection of symptomatic 
  "log_beta_s", 
  
  # parameter that determines relative infectiousness of E/Ia/C 
  # classes compared to Isu/Isd 
  "trans_e", "trans_a", "trans_c", "trans_h",
  
  # overall transmission reduction due to social distancing
  "beta_reduce",  
  
  # rate of movement through E/Ia/Isu/Isd/C/H compartments,
  "log_g_e", "log_g_a", "log_g_su", "log_g_sd", "log_g_c","log_g_h",
  
  # factor by which movement through Isd happens faster (quicker diagnosis) 
  "log_diag_speedup", 
  
  # determines fraction that get diagnosed before and after intervention
  "detect_0","detect_1", 
  
  "frac_asym", # fraction asymptomatic
  "frac_hosp", # fraction diagnosed that go into hospital
  "frac_dead", # fraction hospitalized that die
  
  # negative binomial observation dispersion parameters
  "log_theta_cases","log_theta_hosps","log_theta_deaths"
)

# Specify which initial conditions to estimate
inivals_to_estimate <- c(                        
  "E1_0", "E2_0", "E3_0", "E4_0",  
  "Ia1_0", "Ia2_0", "Ia3_0", "Ia4_0", 
  "Isu1_0", "Isu2_0", "Isu3_0", "Isu4_0", 
  "Isd1_0", "Isd2_0", "Isd3_0", "Isd4_0" 
)

est_list = list()
est_list$params_to_estimate =  params_to_estimate
est_list$inivals_to_estimate = inivals_to_estimate

filename <- here('output/estpars.RDS')
saveRDS(est_list,filename)
