# make-pomp-model.R
#
# This script generates the model part of a pomp object for an SEIR model of COVID 19 
# this model can be used for simulation. By adding data and measurement part to pomp object, one can do inference
# The stochastic process model was developed by
# John Drake and Pej Rohani; the pomp snippet code was written by 
# John Drake and Andreas Handel, with minor edits by Andrew Tredennick.
# Running this script saves an RDS object: covid-pomp-model.RDS. This object
# can then be loaded to perform model simulations as needed.
# for fitting, the pomp object needs to be augmented with data and measurement functions

# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------
library(pomp)
library(here) #to simplify loading/saving into different folders

# Define COVID-19 process model -------------------------------------------

covid_step_C <- Csnippet(
  "
  // C indexes at 0, for R users making things 1 bigger and start 
  // with index 1, i.e. leave trans[0] empty, same for rate[0]
  double trans[17];
  double rate[17];
  double Epresymptom;
  double Idetected;
  double Iundetected;
  double foi;  // force of infection
  double gamma;  // rate of transition through I compartments
  double detect_frac; // fraction of those that get eventually diagnosed

  Epresymptom = E1+E2+E3+E4+E5+E6;  // all pre-symptomatic
  Idetected = I1+I2+I3+I4;          // all symptomatic that will be detected
  Iundetected = Iu1+Iu2+Iu3+Iu4;    // all symptomatic/asymptomatic that 
                                    // won't be detected

  // Force of Infection (foi):
  // Time dependent transmission, multiplied by different groups.
  // Each group can have its own transmission rate.
  // t_int1 days after simulation start, an intervention reduces 
  //    transmission rate by some factor.
  // t_int1 is new, not in original code. There it was assumed to be 
  //    the same as t_int2.

  if (t<=t_int1)
    foi = beta_d*Idetected + beta_u*Iundetected + beta_e*Epresymptom;
  else
    foi = (1/(beta_red_factor+1))*(beta_d*Idetected + beta_u*Iundetected + beta_e*Epresymptom);


  // Time-dependent rate of movement through infected and detected classes:
  // t_int2 days after simulation start, the time at which individuals are 
  //    diagnosed and thus the time spent in the I categories before ending 
  //    up in H decreases
  //t_int2 is caled z in original code
  
  // IF time (t) is less then intervention time (t_int2), duration spent in I 
  //    is given by 1/gamma_u, otherwise 1/gamma_d
  if (t<t_int2) 
      gamma = gamma_u;
  else
      gamma = gamma_d;

  // Time dependent fraction of those that move into detected category at 
  //    the end of the E phase.
  // t_int3 days after simulation start, the fraction detected (those that 
  //    move into I instead of Iu after exiting E) increases
  // Note that both higher fraction detected and faster rate of detection 
  //    speed up arrival of individuals in H and subsequently C
  // t_int3 is called w in original code, detect_frac is called q/q0/q1 in
  //    the original code

  if (t<t_int3)
    detect_frac = detect_frac_0;
  else
    detect_frac = detect_frac_1;
  
  // Compute the transition rates
  rate[1] = foi;
  rate[2] = sigma;
  rate[3] = sigma * detect_frac;
  rate[4] = sigma * (1 - detect_frac);
  rate[5] = gamma;
  rate[6] = gamma_u;
  
  // Compute the state transitions
  reulermultinom(1, S, &rate[1], dt, &trans[1]);
  
  reulermultinom(1, E1, &rate[2], dt, &trans[2]);
  reulermultinom(1, E2, &rate[2], dt, &trans[3]);
  reulermultinom(1, E3, &rate[2], dt, &trans[4]);
  reulermultinom(1, E4, &rate[2], dt, &trans[5]);
  reulermultinom(1, E5, &rate[2], dt, &trans[6]);
  reulermultinom(2, E6, &rate[3], dt, &trans[7]);  // goes through trans[8]
  
  reulermultinom(1, I1, &rate[5], dt, &trans[9]);
  reulermultinom(1, I2, &rate[5], dt, &trans[10]);
  reulermultinom(1, I3, &rate[5], dt, &trans[11]);
  reulermultinom(1, I4, &rate[5], dt, &trans[12]);
  
  reulermultinom(1, Iu1, &rate[6], dt, &trans[13]);
  reulermultinom(1, Iu2, &rate[6], dt, &trans[14]);
  reulermultinom(1, Iu3, &rate[6], dt, &trans[15]);
  reulermultinom(1, Iu4, &rate[6], dt, &trans[16]);


  // Apply transitions to state variables
  S -= trans[1];

  E1 += trans[1] - trans[2];
  E2 += trans[2] - trans[3];
  E3 += trans[3] - trans[4];
  E4 += trans[4] - trans[5];
  E5 += trans[5] - trans[6];
  E6 += trans[6] - trans[7] - trans[8];

  I1 += trans[7] - trans[9];
  I2 += trans[9] - trans[10];
  I3 += trans[10] - trans[11];
  I4 += trans[11] - trans[12];

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


# C snipped for initial condition specification -------------------------------------

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

# Parameter transforms for estimation -------------------------------------

param_transforms <- parameter_trans(
  log = c("beta_d", "beta_u", "beta_e", "beta_red_factor", "t_int1", 
          "t_int2", "t_int3", "gamma_u", "gamma_d"),
  logit = c("detect_frac_0", "detect_frac_1")
)

# State variables to track ------------------------------------------------
varnames <- c("S", 
             "E1", "E2", "E3", "E4", "E5", "E6", 
             "I1", "I2", "I3", "I4", "Iu1", "Iu2", "Iu3", "Iu4", 
             "C", 
             "Ru")


# Parameters --------------------------------------------------------------
# Parameter and variable names
parnames1 <- c("beta_d", "beta_u", "beta_e", "beta_red_factor", 
               "t_int1", "t_int2", "t_int3", 
               "gamma_u", "gamma_d",
               "detect_frac_0","detect_frac_1",
               "sigma")

# Initial conditions of state variables are also parameters
parnames2 <- c("S_0", "E1_0", "E2_0", "E3_0", "E4_0", "E5_0", "E6_0", 
               "I1_0", "I2_0", "I3_0", "I4_0", 
               "Iu1_0", "Iu2_0", "Iu3_0", "Iu4_0", 
               "C_0","Ru_0")

parnames <- c(parnames1,parnames2)


#######################################################################
#create fake data to make pomp happy
#######################################################################
tmax = 100
fake_data = data.frame(time = 0:tmax, cases = rpois(tmax+1,lambda  = 50))

#######################################################################
#not used here, but added as dummy 
#to make pomp stop produce warning messages
#real rmeas should be specified separately and added to pomp object, 
#together with data
#######################################################################
rmeas <- pomp::Csnippet("
      cases = C;
    ")  

# Define the pomp model object --------------------------------------------

covid_pomp_model <- pomp(
  data = fake_data,
  times = "time",
  t0 = 0,
  rmeasure = rmeas,
  rinit = rinit,
  rprocess = euler(step.fun = covid_step_C, delta.t = 1/20),
  partrans = param_transforms,
  statenames = varnames,
  paramnames = parnames, 
  obsnames = c("cases"),
  accumvars = c("C")
  )


# Save the pomp object ----------------------------------------------------
filename = here('output2/covid-pomp-simulator.RDS')

saveRDS(covid_pomp_model, filename)
