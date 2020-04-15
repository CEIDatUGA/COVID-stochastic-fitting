# make-priors.R
 library(here)
 library(pomp)
rm(list = ls(all.names = TRUE))

#values for priors are explained in a separate spreadsheet:
#https://docs.google.com/spreadsheets/d/10K_3bPck0GOCAfuUQtyZ-KEmwf6FQE3CcpSe36HtL4c/edit#gid=478517635

# code below implements them so they can be used by pomp
# mean/central values for parameters, as specified by spreadsheet
# for meaning of parameters, see spreadhseet

#load values for model parameters and initial conditions
#use those as mean/central value for prior definition
filename = here('output/var-par-definitions.RDS')
x <- readRDS(filename) #for ease of notation, assign parameter values to x

param_sds <- 3
prior_par_list = list()
prior_ini_list = list()
ini_sds <- 1

#makes priors of form "dnorm(par,mu,param_sd,1)

for (nn in 1:length(x$params_to_estimate))
{
  prior_par_list[[nn]] = paste0("dnorm(", x$params_to_estimate[nn],", ",as.numeric(x$allparvals[x$params_to_estimate[nn]]),", ",param_sds, ", 1)")
}

for (nn in 1:length(x$inivals_to_estimate)) #need different sds, so extra loop
{
  prior_ini_list[[nn]] = paste0("dnorm(", x$inivals_to_estimate[nn],", ",x$allparvals[x$inivals_to_estimate[nn]],", ",ini_sds, ", 1)")
}

s1 = paste0(prior_par_list, collapse = " + ")
s2 = paste0(prior_ini_list, collapse = " + ")
prior_dens_text = paste0("lik = ",s1," + ",s2, " ; \n " ,
                         "if (!give_log) lik = exp(lik);" )

prior_dens <- pomp::Csnippet(
  prior_dens_text
)
filename = here('output/prior-dens-object.RDS')
saveRDS(prior_dens,filename)


###################
#Old/manual way of doing priors below
###################

# x = x$allparvals
# pr_log_beta_s = paste0("dnorm(log_beta_s, ",x["log_beta_s"],",", param_sds, ", 1)")
# pr_trans_e = paste0("dnorm(trans_e, ",x["trans_e"],",", param_sds, ", 1)")
# pr_trans_a = paste0("dnorm(trans_a, ",x["trans_e"],",", param_sds, ", 1)")
# pr_trans_c = paste0("dnorm(trans_c, ",x["trans_c"],",", param_sds, ", 1)")
# pr_trans_h = paste0("dnorm(trans_h, ",x["trans_h"],",", param_sds, ", 1)")
# pr_log_g_e = paste0("dnorm(log_g_e, ",x["log_g_e"],",", param_sds, ", 1)")
# pr_log_g_a = paste0("dnorm(log_g_a, ",x["log_g_a"],",", param_sds, ", 1)")
# pr_log_g_su = paste0("dnorm(log_g_su, ",x["log_g_su"],",", param_sds, ", 1)")
# pr_log_g_sd = paste0("dnorm(log_g_sd, ",x["log_g_sd"],",", param_sds, ", 1)")
# pr_log_g_c = paste0("dnorm(log_g_c, ",x["log_g_c"],",", param_sds, ", 1)")
# pr_log_g_h = paste0("dnorm(log_g_h, ",x["log_g_h"],",", param_sds, ", 1)")
# pr_log_max_diag = paste0("dnorm(log_max_diag, ",x["log_max_diag"],",", param_sds, ", 1)")
# pr_log_diag_inc_rate = paste0("dnorm(log_diag_inc_rate, ",x["log_diag_inc_rate"],",", param_sds, ", 1)")
# pr_max_detect_par = paste0("dnorm(max_detect_par, ",x["max_detect_par"],",", param_sds, ", 1)")
# pr_log_detect_inc_rate = paste0("dnorm(log_detect_inc_rate, ",x["log_detect_inc_rate"],",", param_sds, ", 1)")
# pr_frac_asym = paste0("dnorm(frac_asym, ",x["frac_asym"],",", param_sds, ", 1)")
# pr_frac_hosp = paste0("dnorm(frac_hosp, ",x["frac_hosp"],",", param_sds, ", 1)")
# pr_frac_dead = paste0("dnorm(frac_dead, ",x["frac_dead"],",", param_sds, ", 1)")
# pr_log_theta_cases = paste0("dnorm(log_theta_cases, ",x["log_theta_cases"],",", param_sds, ", 1)")
# pr_log_theta_hosps = paste0("dnorm(log_theta_hosps, ",x["log_theta_hosps"],",", param_sds, ", 1)")
# pr_log_theta_deaths = paste0("dnorm(log_theta_deaths, ",x["log_theta_deaths"],",", param_sds, ", 1)")
# 
# 
# pr_E1_0 = paste0("dnorm(E1_0, ",x["E1_0"],", 5, 1)")
# pr_E2_0 = paste0("dnorm(E2_0, ",x["E2_0"],", 5, 1)")
# pr_E3_0 = paste0("dnorm(E3_0, ",x["E3_0"],", 5, 1)")
# pr_E4_0 = paste0("dnorm(E4_0, ",x["E4_0"],", 5, 1)")
# pr_Ia1_0 = paste0("dnorm(Ia1_0, ",x["Ia1_0"],", 4, 1)")
# pr_Ia2_0 = paste0("dnorm(Ia2_0, ",x["Ia2_0"],", 4, 1)")
# pr_Ia3_0 = paste0("dnorm(Ia3_0, ",x["Ia3_0"],", 4, 1)")
# pr_Ia4_0 = paste0("dnorm(Ia4_0, ",x["Ia4_0"],", 4, 1)")
# pr_Isu1_0 = paste0("dnorm(Isu1_0, ",x["Isu1_0"],", 7, 1)")
# pr_Isu2_0 = paste0("dnorm(Isu2_0, ",x["Isu2_0"],", 7, 1)")
# pr_Isu3_0 = paste0("dnorm(Isu3_0, ",x["Isu3_0"],", 7, 1)")
# pr_Isu4_0 = paste0("dnorm(Isu4_0, ",x["Isu4_0"],", 7, 1)")
# pr_Isd1_0 = paste0("dnorm(Isd1_0, ",x["Isd1_0"],", 3, 1)")
# pr_Isd2_0 = paste0("dnorm(Isd2_0, ",x["Isd2_0"],", 3, 1)")
# pr_Isd3_0 = paste0("dnorm(Isd3_0, ",x["Isd3_0"],", 3, 1)")
# pr_Isd4_0 = paste0("dnorm(Isd4_0, ",x["Isd4_0"],", 3, 1)")
# 
# 
# 
# prior_dens_text2 = paste0("lik = ", pr_log_beta_s," + ",
#                                   pr_trans_e, " + ",
#                                   pr_trans_a, " + ",
#                                   pr_trans_c, " + ",
#                                   pr_trans_h, " + ",
#                           pr_log_g_e , " + ",
#                          pr_log_g_a , " + ",
#                          pr_log_g_su , " + ",
#                          pr_log_g_sd , " + ",
#                          pr_log_g_c , " + ",
#                          pr_log_g_h , " + ",
#                          pr_log_max_diag, " + ",
#                          pr_log_diag_inc_rate , " + ",
#                          pr_max_detect_par , " + ",
#                          pr_log_detect_inc_rate , " + ",
#                          pr_frac_asym , " + ",
#                          pr_frac_hosp , " + ",
#                          pr_frac_dead , " + ",
#                          pr_log_theta_cases , " + ",
#                          pr_log_theta_hosps , " + ",
#                          pr_log_theta_deaths , " + ",
#                          pr_E1_0 , " + ",
#                          pr_E2_0 , " + ",
#                          pr_E3_0 , " + ",
#                          pr_E4_0 , " + ",
#                          pr_Ia1_0 , " + ",
#                          pr_Ia2_0 , " + ",
#                          pr_Ia3_0 , " + ",
#                          pr_Ia4_0 , " + ",
#                          pr_Isu1_0 , " + ",
#                          pr_Isu2_0 , " + ",
#                          pr_Isu3_0 , " + ",
#                          pr_Isu4_0 , " + ",
#                          pr_Isd1_0 , " + ",
#                          pr_Isd2_0 , " + ",
#                          pr_Isd3_0 , " + ",
#                          pr_Isd4_0 , " ; \n " ,
#                          "if (!give_log) lik = exp(lik);"
# )


