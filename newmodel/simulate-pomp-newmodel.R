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
filename = here('outputnew/pomp-newmodel.RDS')
pomp_model <- readRDS(filename)


#Specify values to run model at

# Initial values ----------------------------------------------------------

inivals <- c(S_0 = 10600000, 
             E1_0 = 35, E2_0 = 35, E3_0 = 35, E4_0 = 35, 
             Ia1_0 = 14, Ia2_0 = 14, Ia3_0 = 14, Ia4_0 = 14, 
             Isu1_0 = 111, Isu2_0 = 111, Isu3_0 = 111, Isu4_0 = 111, 
             Isd1_0 = 111, Isd2_0 = 111, Isd3_0 = 111, Isd4_0 = 111, 
             C1_0 = 35, C2_0 = 35, C3_0 = 35, C4_0 = 35, 
             H1_0 = 35, H2_0 = 35, H3_0 = 35, H4_0 = 35, 
             R_0 = 1,
             D_0 = 0
            )

Ntot <- sum(inivals)  # total population size

# Values for parameters
# beta is scaled by population size here instead of inside the process model
parvals <- c(log_beta_s = log(0.75/Ntot), 
             trans_e = 1, #value of 1 means factor is 0.5
             trans_a = 1, 
             trans_c = 10, 
             beta_reduce = 1,  
             t_int1 = 12,
             t_int2 = 12,
             t_int3 = 12,
             log_g_e = log(4*0.2),
             log_g_a = log(4*0.15),
             log_g_su = log(4*0.15),
             log_g_c = log(4*0.3),
             log_g_h = log(4*0.3),
             log_diag_speedup = 1, 
             detect_0 = 1,
             detect_1 = 2, 
             frac_asym = 0.2, 
             frac_hosp = 0.05, 
             frac_dead = 0.1, #fraction hospitalized that die
             rho = 0.5, 
             theta = 100
  )
  

#run simulation a number of times
sims <- pomp::simulate(pomp_model, 
                       params=c(parvals,inivals), 
                       nsim=10, format="data.frame", 
                       include.data=TRUE)


p <- sims %>% ggplot(aes(x=time,y=cases,group=.id,color=.id=="data"))+
  geom_line()+ scale_y_log10()
  guides(color=FALSE)
plot(p)
  
