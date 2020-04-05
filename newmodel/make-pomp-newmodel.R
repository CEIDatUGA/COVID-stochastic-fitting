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
  
  double rate[14];
  double trans[29];
  
  double E_tot, Ia_tot, Isu_tot, Isd_tot, C_tot, H_tot;
  double foi;  // force of infection
  double g_sd;  // rate of transition through I_sd compartments
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
  foi = exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot);

  // t_int1 days after simulation start, social distancing intervention reduces transmission rate
  if (t>t_int1)
      foi = 1/(1+exp(beta_reduce))*foi;


  // Time-dependent rate of movement through Isd dummy compartments
  // t_int2 days after simulation start, the time it takes for individuals to get diagnosed decreases
  if (t<t_int2) 
      g_sd = exp(log_g_su);  //regular time in sympomatic stage, without diagnosis
  else
      g_sd = exp(log_diag_speedup)*exp(log_g_su); //shortened time in symptomatic stage, due to diagnosis

  // Time dependent fraction of those that move into detected category at the end of the E phase.
  // t_int3 days after simulation start, the fraction that move into Isd increases
  if (t<t_int3)
    detect_frac = 1/(1+exp(detect_0));
  else
    detect_frac = 1/(1+exp(detect_1));
  
  // Compute the transition rates
  rate[1] = foi;                                                //infection, movement from S to E
  rate[2] = exp(log_g_e);                                        //movement through E compartments
  rate[3] = exp(log_g_e) * frac_asym;                             //from E to Ia
  rate[4] = exp(log_g_e) * (1 - frac_asym) * (1 - detect_frac);  //from E to Isu
  rate[5] = exp(log_g_e) * (1 - frac_asym) * detect_frac;        //from E to Isd

  rate[6] = exp(log_g_a);                                                //movement through Ia stages
  rate[7] = exp(log_g_su);                                               //movement through Isu stages
  rate[8] = g_sd;                                                        //movement through Isd stages - computed above
  rate[9] = exp(log_g_c);                                                //movement through C stages
  
  rate[10] = exp(log_g_c) * frac_hosp;                                   //movement from C to H  
  rate[11] = exp(log_g_c) * (1 - frac_hosp);                             //movement from C to R  

  rate[12] = exp(log_g_h);                                               //movement through H stages  
  rate[13] = exp(log_g_h) *  frac_dead;                                  //movement from H to D  
  rate[14] = exp(log_g_h) * (1 - frac_dead);                             //movement from H to R  
  
  
  // Compute the state transitions
  reulermultinom(1, S, &rate[1], dt, &trans[1]); //infection
  
  reulermultinom(1, E1, &rate[2], dt, &trans[2]); //move through E stages
  reulermultinom(1, E2, &rate[2], dt, &trans[3]);
  reulermultinom(1, E3, &rate[2], dt, &trans[4]);
  reulermultinom(3, E4, &rate[3], dt, &trans[5]); // move from E to Ia or Isu or Isd, goes through rates 3-5 and trans 5-7
  
  reulermultinom(1, Ia1, &rate[6], dt, &trans[8]); //move through Ia stages
  reulermultinom(1, Ia2, &rate[6], dt, &trans[9]);
  reulermultinom(1, Ia3, &rate[6], dt, &trans[10]);
  reulermultinom(1, Ia4, &rate[6], dt, &trans[11]); //move into R

  reulermultinom(1, Isu1, &rate[7], dt, &trans[12]); //move through Isu stages
  reulermultinom(1, Isu2, &rate[7], dt, &trans[13]);
  reulermultinom(1, Isu3, &rate[7], dt, &trans[14]);
  reulermultinom(1, Isu4, &rate[7], dt, &trans[15]); //move into R

  reulermultinom(1, Isd1, &rate[8], dt, &trans[16]); //move through Isd stages
  reulermultinom(1, Isd2, &rate[8], dt, &trans[17]);
  reulermultinom(1, Isd3, &rate[8], dt, &trans[18]);
  reulermultinom(1, Isd4, &rate[8], dt, &trans[19]); //move into C
  
  reulermultinom(1, C1, &rate[9], dt, &trans[20]); //move through C stages
  reulermultinom(1, C2, &rate[9], dt, &trans[21]);
  reulermultinom(1, C3, &rate[9], dt, &trans[22]);
  reulermultinom(2, C4, &rate[10], dt, &trans[23]); //move from C to either H or R, goes through rates 10-11 and trans 23-24


  reulermultinom(1, H1, &rate[12], dt, &trans[25]); //move through H stages
  reulermultinom(1, H2, &rate[12], dt, &trans[26]);
  reulermultinom(1, H3, &rate[12], dt, &trans[27]);
  reulermultinom(2, H4, &rate[13], dt, &trans[28]); //move from H to either D or R, goes through rates 13-14 and trans 28-29


  // Apply transitions to state variables
  S -= trans[1];

  E1 += trans[1] - trans[2];
  E2 += trans[2] - trans[3];
  E3 += trans[3] - trans[4];
  E4 += trans[4] - trans[5] - trans[6] - trans[7];

  Ia1 += trans[5] - trans[8]; //from E
  Ia2 += trans[8] - trans[9];
  Ia3 += trans[9] - trans[10];
  Ia4 += trans[10] - trans[11]; //into R

  Isu1 += trans[6] - trans[12]; //from E
  Isu2 += trans[12] - trans[13];
  Isu3 += trans[13] - trans[14];
  Isu4 += trans[14] - trans[15]; //into R

  Isd1 += trans[7] - trans[16]; //from E
  Isd2 += trans[16] - trans[17];
  Isd3 += trans[17] - trans[18];
  Isd4 += trans[18] - trans[19]; //into C

  C1 += trans[19] - trans[20]; //from Isd4
  C2 += trans[20] - trans[21];
  C3 += trans[21] - trans[22];
  C4 += trans[22] - trans[23] - trans[24]; //into H or R

  H1 += trans[23] - trans[25]; //from C
  H2 += trans[25] - trans[26];
  H3 += trans[26] - trans[27];
  H4 += trans[27] - trans[28] - trans[29]; //into D or R

  R += trans[11] + trans[15] + trans[24] + trans[29];
  D += trans[23] + trans[28]; 
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
  
  Ia1 = nearbyint(Ia1_0);
  Ia2 = nearbyint(Ia2_0);
  Ia3 = nearbyint(Ia3_0);
  Ia4 = nearbyint(Ia4_0);
  
  Isu1 = nearbyint(Isu1_0);
  Isu2 = nearbyint(Isu2_0);
  Isu3 = nearbyint(Isu3_0);
  Isu4 = nearbyint(Isu4_0);

  Isd1 = nearbyint(Isd1_0);
  Isd2 = nearbyint(Isd2_0);
  Isd3 = nearbyint(Isd3_0);
  Isd4 = nearbyint(Isd4_0);
  
  C1 = nearbyint(C1_0);
  C2 = nearbyint(C2_0);
  C3 = nearbyint(C3_0);
  C4 = nearbyint(C4_0);

  H1 = nearbyint(H1_0);
  H2 = nearbyint(H2_0);
  H3 = nearbyint(H3_0);
  H4 = nearbyint(H4_0);

  R = nearbyint(R_0);
  D = nearbyint(D_0);
  "
)


############################################################################
# Code to define estimation components of  model 
############################################################################

# Define likelihood function ----------------------------------------------

dmeas <- Csnippet(
  "
  double d1, d2, d3;
  
  if(ISNA(cases)) {
    d1 = 0;
  } else {
    d1 = dnbinom_mu(cases, theta, rho * C1, 1);
  }
  
  if(ISNA(hosps)) {
    d2 = 0;
  } else {
    d2 = dnbinom_mu(hosps, theta_hosp, H1, 1);
  }
  
  if(ISNA(deaths)) {
    d3 = 0;
  } else {
    d3 = dnbinom_mu(deaths, theta_death, D, 1);
  }
  
  //lik = d1 + d2 + d3;
  lik = (give_log) ? (d1 + d2 + d3) : exp(d1 + d2 + d3);
  "
)


# Define process simulator for observations  ------------------------------

rmeas <- Csnippet(
  "
  cases = rnbinom_mu(theta, rho * C1);  // for forecasting
  hosps = rnbinom_mu(theta_hosp, H1);  // for forecasting
  deaths = rnbinom_mu(theta_death, D);  // for forecasting
  "
)



############################################################################
# Code to define variables, parameters and parameter transformations
############################################################################

# State variables to track ------------------------------------------------
varnames <- c("S", 
             "E1", "E2", "E3", "E4",  
             "Ia1", "Ia2", "Ia3", "Ia4", 
             "Isu1", "Isu2", "Isu3", "Isu4", 
             "Isd1", "Isd2", "Isd3", "Isd4", 
             "C1", "C2", "C3", "C4",  
             "H1", "H2", "H3", "H4",  
             "R",
             "D")


# Parameters --------------------------------------------------------------
# Parameter and variable names
model_pars <- c("log_beta_s", #rate of infection of symptomatic 
                "trans_e", "trans_a", "trans_c", #parameter that determines relative infectiousness of E/Ia/C classes compared to Isu/Isd 
                "beta_reduce",  #overall transmission reduction due to social distancing
                "t_int1", "t_int2", "t_int3", #time at which different interventions are started
                "log_g_e", "log_g_a", "log_g_su","log_g_c","log_g_h", #rate of movement through E/Ia/Isu/C/H compartments,
                "log_diag_speedup", #factor by which movement through Isd happens faster (quicker diagnosis) 
                "detect_0","detect_1", #determines fraction that get diagnosed before and after intervention
                "frac_asym", #fraction asymptomatic
                "frac_hosp", #fraction diagnosed that go into hospital
                "frac_dead" #fraction hospitalized that die
)

measure_pars <- c("rho","theta","theta_hosp","theta_death")

# Initial conditions of state variables are also parameters
ini_pars <- c("S_0", 
              "E1_0", "E2_0", "E3_0", "E4_0",  
              "Ia1_0", "Ia2_0", "Ia3_0", "Ia4_0", 
              "Isu1_0", "Isu2_0", "Isu3_0", "Isu4_0", 
              "Isd1_0", "Isd2_0", "Isd3_0", "Isd4_0", 
              "C1_0", "C2_0", "C3_0", "C4_0",  
              "H1_0", "H2_0", "H3_0", "H4_0",  
              "R_0",
              "D_0")
parnames <- c(model_pars,measure_pars,ini_pars)


#######################################################################
# Load cleaned data ---------------------------------------------------------
#######################################################################
filename = here('outputnew/clean-data.RDS')
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
  obsnames = c("cases", "hosps", "deaths"),
  accumvars = c("C1","H1","R","D") 
)

# Save the pomp object ----------------------------------------------------
filename = here('outputnew/pomp-newmodel.RDS')
saveRDS(pomp_model, filename)
