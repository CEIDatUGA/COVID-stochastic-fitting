# make-pomp-object.R
#
# This script generates a pomp object for fitting an SEIR model to
# COVID-19 case data. The stochastic process model was developed by
# John Drake and Pej Rohani; the pomp snippet code was written by 
# John Drake and Andreas Handel, with minor edits by Andrew Tredennick.
# Running this script saves an RDS object: covid-pomp.RDS. This object
# can then be loaded to perform model fitting and simulations as needed.


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)


# Set up the data ---------------------------------------------------------

data <- read.csv('../GA_daily_status_report_GDPH.csv')
data$date <- as.Date(data$date, format='%m/%d/%y')

covid_ga_data <- data %>% dplyr::select(date, cases_cumulative, fatalities_cumulative) %>%
  tidyr::replace_na(list(cases_cumulative = 0, fatalities_cumulative = 0)) %>%
  dplyr::mutate(days = 1:nrow(data)) %>%
  dplyr::mutate(cases = cases_cumulative) %>%
  dplyr::select(days, cases)

# Extend data frame holding data by a month so that pomp runs 
# simulations for that long
future <- data.frame(days = max(covid_ga_data$days):(max(covid_ga_data$days)+31),
                     cases = NA )
covid_ga_data <- rbind(covid_ga_data,future)
covid_ga_data$cases <- covid_ga_data$cases + 1 # Add one to avoid 0s for fits


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
    foi = exp(beta_d)*Idetected + exp(beta_u)*Iundetected + exp(beta_e)*Epresymptom;
  else
    foi = (1/(beta_red_factor+1))*(exp(beta_d)*Idetected + exp(beta_u)*Iundetected + exp(beta_e)*Epresymptom);


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


# C snippet for initial condition specification ---------------------------

rinit <- Csnippet(
  "
  S = S_0; 
  E1 = E1_0;
  E2 = E2_0;
  E3 = E3_0;
  E4 = E4_0; 
  E5 = E5_0; 
  E6 = E6_0; 
  I1 = I1_0;
  I2 = I2_0; 
  I3 = I3_0; 
  I4 = I4_0; 
  Iu1 = Iu1_0;
  Iu2 = Iu2_0;
  Iu3 = Iu3_0; 
  Iu4 = Iu4_0;
  C = C_0;
  Ru = Ru_0;
  "
)



# Parameter transforms for estimation -------------------------------------

param_transforms <- parameter_trans(
  log = c("beta_red_factor", "t_int1", 
          "t_int2", "t_int3", "gamma_u", "gamma_d", "theta"),
  logit = c("detect_frac_0", "detect_frac_1"),
  barycentric =  c("E1_0", "E2_0", "E3_0", "E4_0", "E5_0", "E6_0", 
                   "I1_0", "I2_0", "I3_0", "I4_0", 
                   "Iu1_0", "Iu2_0", "Iu3_0", "Iu4_0")
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
               "sigma", 
               "rho", 
               "theta")

# Initial conditions of state variables are also parameters
parnames2 <- c("S_0", "E1_0", "E2_0", "E3_0", "E4_0", "E5_0", "E6_0", 
               "I1_0", "I2_0", "I3_0", "I4_0", 
               "Iu1_0", "Iu2_0", "Iu3_0", "Iu4_0", 
               "C_0","Ru_0")

parnames <- c(parnames1,parnames2)


# Initial values ----------------------------------------------------------

inivals <- c(S_0 = 10600000, 
             E1_0 = 35, E2_0 = 35, E3_0 = 35, E4_0 = 35, E5_0 =35, E6_0 =35,
             I1_0 = 14, I2_0 = 14, I3_0 = 14, I4_0 = 14, 
             Iu1_0 = 111, Iu2_0 = 111, Iu3_0 = 111, Iu4_0 = 111, 
             C_0 = 1,
             Ru_0 = 1)

Ntot <- sum(inivals)  # total population size

# Values for parameters
# beta is scaled by population size here instead of inside the process model
parvals <- c(beta_d = log(2/Ntot), 
             beta_u = log(0.25/Ntot), 
             beta_e = log(0.1/Ntot), 
             beta_red_factor = 0.5, 
             t_int1 = 12, t_int2 = 12, t_int3 = 12, 
             gamma_u = 4*0.1,
             gamma_d = 4*0.5, 
             detect_frac_0 = 0.1, 
             detect_frac_1 = 0.5,
             sigma = 6*0.18, 
             rho = 0.5, 
             theta = 100)



# Define the pomp model object --------------------------------------------

covid_ga_pomp <- pomp(
  data = covid_ga_data[1:25, ],  # currently removes NAs
  times = "days",
  t0 = 0,
  dmeasure = dmeas,
  rmeasure = rmeas,
  rinit = rinit,
  rprocess = euler(step.fun = covid_step_C, delta.t = 1/20),
  partrans = param_transforms,
  statenames = varnames,
  paramnames = parnames, 
  obsnames = c("cases"),
  accumvars = c("C"),
  params = c(parvals, inivals)
)




# Save the pomp object ----------------------------------------------------

saveRDS(covid_ga_pomp, "../output/covid-ga-pomp-object.RDS")


# Test to make sure we can simulate ---------------------------------------

# res <- simulate(covid_ga_pomp)
# simcases <- as.data.frame(t(res@states))$C
# outdf <- covid_ga_data[1:25, ] %>%
#   mutate(pred_cases = simcases)
# 
# ggplot(outdf, aes(x = days)) +
#   geom_point(aes(y = cases)) +
#   geom_line(aes(y = pred_cases))


# Cache -------------------------------------------------------------------

# // Define all transmission rates
# trans[1] = rbinom(S,1-exp(-foi*dt));     //transition from S to E
# trans[2] = rbinom(E1,1-exp(-sigma*dt));  // transition between E compartments 1/2
# trans[3] = rbinom(E2,1-exp(-sigma*dt));  // transition between E compartments
# trans[4] = rbinom(E3,1-exp(-sigma*dt));  // transition between E compartments
# trans[5] = rbinom(E4,1-exp(-sigma*dt));  // transition between E compartments 4/5
# trans[6] = rbinom(E5,1-exp(-sigma*dt));  // transition between E compartments 5/6
# 
# trans[7] = rbinom(E6,(1-exp(-sigma*dt))*detect_frac); // transition between E6 compartment and I --- THIS BIT NEEDS TO BE MULTINOMIAL
# trans[8] = rbinom(E6,(1-exp(-sigma*dt))*(1-detect_frac)); // transition between E6 compartment and Iu
# trans[9] = rbinom(I1,1-exp(-gamma*dt));   // transition between I compartments 1/2
# trans[10] = rbinom(I2,1-exp(-gamma*dt));  // transition between I compartments 2/3
# trans[11] = rbinom(I3,1-exp(-gamma*dt));  // transition between I compartments 3/4
# trans[12] = rbinom(I4,1-exp(-gamma*dt));  // transition between I compartments and C
# 
# trans[13] = rbinom(Iu1,1-exp(-gamma_u*dt));           // transition between Iu compartments 1/2
# trans[14] = rbinom(Iu2,1-exp(-gamma_u*dt));           // transition between Iu compartments 2/3
# trans[15] = rbinom(Iu3,1-exp(-gamma_u*dt));           // transition between Iu compartments 3/4
# trans[16] = rbinom(Iu4,1-exp(-gamma_u*dt));           // transition between Iu compartments and Ru
