# make-priors.R
library(here)
library(pomp)

#values for priors are explained in a separate spreadsheet:
#https://docs.google.com/spreadsheets/d/10K_3bPck0GOCAfuUQtyZ-KEmwf6FQE3CcpSe36HtL4c/edit#gid=478517635

#code below implements them so they can be used by pomp

# mean/central values for parameters, as specified by spreadsheet
# for meaning of parameters, see spreadhseet


#load values for model parameters and initial conditions
#use those as mean/central value for prior definition
filename = here('output/parvals.RDS')
x <- readRDS(filename) #for ease of notation, assign parameter values to x


# the idea is to use the point estimates/central values above
#and add a distribution with some spread to define priors
#this should be done only for parameters that are estimated

##Since we don't know how to pass variables into this Csnippet, we create it as a string


pr_log_beta_s = paste0("dnorm(log_beta_s, ",x["log_beta_s"],", 0.2, 1)")  
pr_trans_e = paste0("dnorm(trans_e, ",x["trans_e"],", 0.2, 1)") 
pr_trans_a = paste0("dnorm(trans_a, ",x["trans_e"],", 0.2, 1)") 
pr_trans_c = paste0("dnorm(trans_c, ",x["trans_c"],", 0.2, 1)") 
pr_trans_h = paste0("dnorm(trans_h, ",x["trans_h"],", 0.2, 1)") 
pr_beta_reduce = paste0("dnorm(beta_reduce, ",x["beta_reduce"],", 0.2, 1)")   
pr_log_g_e = paste0("dnorm(log_g_e, ",x["log_g_e"],", 0.2, 1)") 
pr_log_g_a = paste0("dnorm(log_g_a, ",x["log_g_a"],", 0.2, 1)") 
pr_log_g_su = paste0("dnorm(log_g_su, ",x["log_g_su"],", 0.2, 1)") 
pr_log_g_sd = paste0("dnorm(log_g_sd, ",x["log_g_sd"],", 0.2, 1)") 
pr_log_g_c = paste0("dnorm(log_g_c, ",x["log_g_c"],", 0.2, 1)") 
pr_log_g_h = paste0("dnorm(log_g_h, ",x["log_g_h"],", 0.2, 1)") 
pr_log_diag_speedup = paste0("dnorm(log_diag_speedup, ",x["log_diag_speedup"],", 0.2, 1)") 
pr_detect_0 = paste0("dnorm(detect_0, ",x["detect_0"],", 0.2, 1)") 
pr_detect_1 = paste0("dnorm(detect_1, ",x["detect_1"],", 0.2, 1)") 
pr_frac_asym = paste0("dnorm(frac_asym, ",x["frac_asym"],", 0.2, 1)") 
pr_frac_hosp = paste0("dnorm(frac_hosp, ",x["frac_hosp"],", 0.2, 1)") 
pr_frac_dead = paste0("dnorm(frac_dead, ",x["frac_dead"],", 0.2, 1)") 
pr_log_theta_cases = paste0("dnorm(log_theta_cases, ",x["log_theta_cases"],", 0.2, 1)")
pr_log_theta_hosps = paste0("dnorm(log_theta_hosps, ",x["log_theta_hosps"],", 0.2, 1)")
pr_log_theta_deaths = paste0("dnorm(log_theta_deaths, ",x["log_theta_deaths"],", 0.2, 1)")


prior_dens_text = paste0("lik =", pr_log_beta_s," + ",
                                  pr_trans_e, " + ",
                                  pr_trans_a, " + ",
                                  pr_trans_c, " + ",
                                  pr_trans_h, " + ",
                         pr_beta_reduce , " + ",  
                         pr_log_g_e , " + ",
                         pr_log_g_a , " + ",
                         pr_log_g_su , " + ",
                         pr_log_g_sd , " + ",
                         pr_log_g_c , " + ",  
                         pr_log_g_h , " + ",
                         pr_log_diag_speedup , " + ", 
                         pr_detect_0 , " + ",
                         pr_detect_1 , " + ", 
                         pr_frac_asym , " + ",
                         pr_frac_hosp , " + ",
                         pr_frac_dead , " + ",
                         pr_log_theta_cases , " + ",
                         pr_log_theta_hosps , " + ",
                         pr_log_theta_deaths , "; \n ",
                         "if (!give_log) lik = exp(lik);"
)


prior_dens <- pomp::Csnippet(
  prior_dens_text
)
filename = here('output/prior-dens-object.RDS')
saveRDS(prior_dens,filename)

