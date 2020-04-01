# simulate-pomp-model.R
#
# This script runs the previously generated pomp model
# it does not do fitting, but can be used for exploration 
# and to generate generate synthetic data


# Load libraries ----------------------------------------------------------
library(pomp)
library(here) #to simplify loading/saving into different folders

# Load pomp simulator object ---------------------------------------------------------
filename = here('output/covid-pomp-model.RDS')
covid_pomp_model <- readRDS(filename)


#Specify values to run model at

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
parvals <- c(beta_d = 5e-7, 
             beta_u = 0.25/Ntot, 
             beta_e = 0.1/Ntot, 
             beta_red_factor = 0.5, 
             t_int1 = 12, t_int2 = 12, t_int3 = 12, 
             gamma_u = 4*0.1,
             gamma_d = 4*0.5, 
             detect_frac_0 = 0.1, 
             detect_frac_1 = 0.5,
             sigma = 6*0.18, 
             rho = 0.5, 
             theta = 100)

#run simulation a number of times
simp <- pomp::simulate(covid_pomp_model, 
                       params=c(parvals,inivals), 
                       nsim=1, format="data.frame", 
                       include.data=FALSE)


  
