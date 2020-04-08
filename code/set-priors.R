# make-priors.R

#values for priors are explained in a separate spreadsheet:
#https://docs.google.com/spreadsheets/d/10K_3bPck0GOCAfuUQtyZ-KEmwf6FQE3CcpSe36HtL4c/edit#gid=478517635

#code below implements them so they can be used by pomp

# mean/central values for parameters, as specified by spreadsheet
# for meaning of parameters, see spreadhseet


#load values for model parameters and initial conditions
#use those as mean/central value for prior definition
filename = here('output/parvals.RDS')
allparvals <- readRDS(filename)

#use central values above and add a distribution with some spread to define priors
#only done for parameters that are estimated
#above all parameters are included, even fixed ones. Those do net get a prior


prior_dens <- Csnippet(
  "
  lik = dnorm(log_beta_s, -18.8975, 0.2, 1) +
        dunif(trans_e, -5, 5, 1) +
        dunif(trans_a, -5, 5, 1) +
        dunif(trans_c, -15, 15, 1) +
        dunif(beta_reduce, -5, 5, 1) +
        dnorm(log_g_e, -0.2231436, 0.05, 1) +
        dunif(log_g_a, -5, 5, 1) +
        dunif(log_g_su, -5, 5, 1) +
        dnorm(log_g_c, 5.764807, 1.157302, 1) +
        dunif(log_g_h, -5, 5, 1) +
        dnorm(log_diag_speedup, 0.6931472, 0.5, 1) +
        dunif(detect_0, -5, 5, 1) +
        dunif(detect_1, -5, 5, 1) +
        dunif(theta, 1, 4, 1);
  
  if (!give_log) lik = exp(lik);
  "
)
