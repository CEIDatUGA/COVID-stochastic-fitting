# simulate-pomp-model.R
#
# This script runs the previously generated pomp model
# it does not do fitting, but can be used for exploration 
# and to generate generate synthetic data


# Load libraries ----------------------------------------------------------
library(pomp)
library(ggplot2)
library(here) #to simplify loading/saving into different folders

# Load pomp simulator object ---------------------------------------------------------
filename = here('output2/pomp-model.RDS')
pomp_model <- readRDS(filename)


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
parvals <- c(log_beta_d = log(0.75/Ntot), 
             log_beta_u = log(0.5/Ntot), 
             log_beta_e = log(0.1/Ntot), 
             beta_reduce = 0.1, 
             t_int1 = 12, t_int2 = 12, t_int3 = 12, 
             log_gamma_u = log(4*0.2),
             log_gamma_d = log(4*0.3), 
             detect_0 = 1, 
             detect_1 = 1.5,
             log_sigma = log(6*0.18), 
             rho = 0.5, 
             theta = 100)

#run simulation a number of times
sims <- pomp::simulate(pomp_model, 
                       params=c(parvals,inivals), 
                       nsim=10, format="data.frame", 
                       include.data=TRUE)


p <- sims %>% ggplot(aes(x=time,y=cases,group=.id,color=.id=="data"))+
  geom_line()+ scale_y_log10()
  guides(color=FALSE)
plot(p)
  
