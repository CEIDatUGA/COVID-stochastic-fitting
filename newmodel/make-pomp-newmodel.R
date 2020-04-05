# make-pomp-newmodel.R
#
# This script generates a pomp object for an SEIR model of COVID 19 
# This is the new version of the model
# Running this script saves an RDS object: pomp-newmodel.RDS.
# The pomp object can be used for simulating trajectories and fitting

# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------
library(pomp)
library(here) #to simplify loading/saving into different folders

############################################################################
# Code to define  process model -------------------------------------------
############################################################################

#1 step function of model
pomp_step <- Csnippet(
    "
  // C indexes at 0, for R users making things 1 bigger and start 
  // with index 1, i.e. leave trans[0] empty, same for rate[0]
  double E_tot, Ia_tot, Isu_tot, Isd_tot, C_tot; H_tot;
  
  double trans[17];
  double rate[17];
  double foi;  // force of infection
  double g_sd;  // rate of transition through I compartments
  double detect_frac; // fraction of those that get eventually diagnosed

  E_tot = E1+E2+E3+E4;  // all pre-symptomatic
  Ia_tot = Ia1+Ia2+Ia3+Ia4;  // all asymptomatic
  Isu_tot = Isu1+Isu2+Isu3+Isu4;  // all symptomatic who will remain undiagnosed
  Isd_tot = Isd1+Isd2+Isd3+Isd4;  // all symptomatic who will become diagnosed
  C_tot = C1+C2+C3+C4;  // all diagnosed/cases
  H_tot = H1+H2+H3+H4;  // all hospitalized
  

  // Force of Infection (foi):
  // Each group can have its own transmission rate.
  // symptomatic are assumed to transmit the most, other groups have reduced transmission by some factor
  // all parameters are used in the model such that they can take on any numeric value (useful for fitting)
  foi = exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*Ic_tot)

  // t_int1 days after simulation start, social distancing intervention reduces transmission rate
  if (t>t_int1)
      foi = 1/(1+exp(beta_reduce))*foi;


  // Time-dependent rate of movement through Isd dummy compartments
  // t_int2 days after simulation start, the time it takes for individuals to get diagnosed decreases
  if (t<t_int2) 
      g_sd = g_su;  //regular time in sympomatic stage, without diagnosis
  else
      g_sd = exp(diag_speedup)*g_su; //shortened time in symptomatic stage, due to diagnosis

  // Time dependent fraction of those that move into detected category at the end of the E phase.
  // t_int3 days after simulation start, the fraction that move into Isd increases
  if (t<t_int3)
    detect_frac = 1/(1+exp(detect_0));
  else
    detect_frac = 1/(1+exp(detect_1));
  
  // Compute the transition rates
  rate[1] = foi;                                                //infection, movement from S to E
  rate[2] = exp(log_ge);                                        //movement through E compartments
  rate[3] = exp(log_ge) * frac_asym                             //from E to Ia
  rate[4] = exp(log_ge) * (1 - frac_asym) * (1 - detect_frac);  //from E to Isu
  rate[5] = exp(log_ge) * (1 - frac_asym) * detect_frac;        //from E to Isd

  rate[6] = g_a;                                                //movement through Ia stages
  rate[7] = g_su;                                               //movement through Isu stages
  rate[8] = g_sd;                                               //movement through Isd stages
  rate[9] = g_c;                                                //movement through C stages
  
  rate[10] = g_c * frac_hosp;                                   //movement from C to H  
  rate[11] = g_c * (1 - frac_hosp);                             //movement from C to R  

  rate[12] = g_h;                                               //movement through H stages  
  rate[13] = g_h * (1 - frac_dead);                             //movement from H to R  
  rate[14] = g_h *  frac_dead;                                  //movement from H to D  
  
  
  // Compute the state transitions
  reulermultinom(1, S, &rate[1], dt, &trans[1]); //infection
  
  reulermultinom(1, E1, &rate[2], dt, &trans[2]); //move through E stages
  reulermultinom(1, E2, &rate[2], dt, &trans[3]);
  reulermultinom(1, E3, &rate[2], dt, &trans[4]);
  reulermultinom(3, E4, &rate[3], dt, &trans[5]); // move from E to Ia or Isu or Isd, goes through rates 3-5 and trans 5-7
  
  reulermultinom(1, Ia1, &rate[6], dt, &trans[8]); //move through Ia stages
  reulermultinom(1, Ia2, &rate[6], dt, &trans[9]);
  reulermultinom(1, Ia3, &rate[6], dt, &trans[10]);
  reulermultinom(1, Ia4, &rate[6], dt, &trans[11]);

  reulermultinom(1, Isu1, &rate[7], dt, &trans[12]); //move through Isu stages
  reulermultinom(1, Isu2, &rate[7], dt, &trans[13]);
  reulermultinom(1, Isu3, &rate[7], dt, &trans[14]);
  reulermultinom(1, Isu4, &rate[7], dt, &trans[15]);

  reulermultinom(1, Isd1, &rate[8], dt, &trans[16]); //move through Isd stages
  reulermultinom(1, Isd2, &rate[8], dt, &trans[17]);
  reulermultinom(1, Isd3, &rate[8], dt, &trans[18]);
  reulermultinom(1, Isd4, &rate[8], dt, &trans[19]);
  
  reulermultinom(1, C1, &rate[9], dt, &trans[8]); //move through C stages
  reulermultinom(1, C2, &rate[9], dt, &trans[9]);
  reulermultinom(1, C3, &rate[9], dt, &trans[10]);
  reulermultinom(2, C4, &rate[9], dt, &trans[11]); //move from C to H or R



  reulermultinom(1, H1, &rate[6], dt, &trans[8]); //move through H stages
  reulermultinom(1, H2, &rate[6], dt, &trans[9]);
  reulermultinom(1, H3, &rate[6], dt, &trans[10]);
  reulermultinom(1, H4, &rate[6], dt, &trans[11]);


  // Apply transitions to state variables
  S -= trans[1];

  E1 += trans[1] - trans[2];
  E2 += trans[2] - trans[3];
  E3 += trans[3] - trans[4];
  E4 += trans[4] - trans[5] - trans[6] - trans[7];

  Ia1 += trans[5] - trans[8];
  Ia2 += trans[8] - trans[9];
  Ia3 += trans[9] - trans[10];
  Ia4 += trans[10] - trans[11];

  Iu1 += trans[8] - trans[13];
  Iu2 += trans[13] - trans[14];
  Iu3 += trans[14] - trans[15];
  Iu4 += trans[15] - trans[16];

  C += trans[12];  // detected cases, assumed to be isolated and not 
                   // further contribute to transmission

  Ru += trans[16]; //undetected cases that recover, assumed to not 
                   //further contribute to transmission
  "
)


# C snippet for initial condition specification ---------------------------

rinit <- Csnippet(
  "
  S = nearbyint(S_0);
  E1 = nearbyint(E1_0);
  E2 = nearbyint(E2_0);
  E3 = nearbyint(E3_0);
  E4 = nearbyint(E4_0);
  E5 = nearbyint(E5_0);
  E6 = nearbyint(E6_0);
  I1 = nearbyint(I1_0);
  I2 = nearbyint(I2_0);
  I3 = nearbyint(I3_0);
  I4 = nearbyint(I4_0);
  Iu1 = nearbyint(Iu1_0);
  Iu2 = nearbyint(Iu2_0);
  Iu3 = nearbyint(Iu3_0);
  Iu4 = nearbyint(Iu4_0);
  C = nearbyint(C_0);
  Ru = nearbyint(Ru_0);
  "
)


############################################################################
# Code to define estimation components of  model 
############################################################################

# Define likelihood function ----------------------------------------------

dmeas <- Csnippet(
  "
  lik = dnbinom_mu(cases, theta, rho * C, give_log);
  "
)


# Define process simulator for observations  ------------------------------

rmeas <- Csnippet(
  "
  cases = rnbinom_mu(theta, rho * C);
  "
)



############################################################################
# Code to define variables, parameters and parameter transformations
############################################################################

# State variables to track ------------------------------------------------
varnames <- c("S", 
             "E1", "E2", "E3", "E4", "E5", "E6", 
             "I1", "I2", "I3", "I4", "Iu1", "Iu2", "Iu3", "Iu4", 
             "C", 
             "Ru")


# Parameters --------------------------------------------------------------
# Parameter and variable names
model_pars <- c("log_beta_d", "log_beta_u", "log_beta_e", "beta_reduce", 
               "t_int1", "t_int2", "t_int3", 
               "log_gamma_u", "log_gamma_d",
               "detect_0","detect_1",
               "log_sigma")

measure_pars <- c("rho","theta")

# Initial conditions of state variables are also parameters
ini_pars <- c("S_0", "E1_0", "E2_0", "E3_0", "E4_0", "E5_0", "E6_0", 
               "I1_0", "I2_0", "I3_0", "I4_0", 
               "Iu1_0", "Iu2_0", "Iu3_0", "Iu4_0", 
               "C_0","Ru_0")

parnames <- c(model_pars,measure_pars,ini_pars)


#######################################################################
# Load cleaned data ---------------------------------------------------------
#######################################################################
filename = here('output2/clean-data.RDS')
pomp_data <- readRDS(filename)


# Define the pomp model object --------------------------------------------
pomp_model <- pomp(
  data = pomp_data, 
  times = "time",
  t0 = 0,
  dmeasure = dmeas,
  rmeasure = rmeas,
  rinit = rinit,
  rprocess = euler(step.fun = pomp_step, delta.t = 1/20),
  statenames = varnames,
  paramnames = parnames, 
  obsnames = c("cases"),
  accumvars = c("C")
)

# Save the pomp object ----------------------------------------------------
filename = here('output2/pomp-model.RDS')
saveRDS(pomp_model, filename)
