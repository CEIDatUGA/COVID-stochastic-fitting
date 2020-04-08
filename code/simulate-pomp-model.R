# simulate-pomp-model.R
#
# This script runs the previously generated pomp model
# it does not do fitting, but can be used for exploration 
# and to generate generate synthetic data

rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(pomp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here) #to simplify loading/saving into different folders

# Load pomp simulator object ---------------------------------------------------------
filename = here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)


#Specify values to run model at

# Initial values ----------------------------------------------------------

inivals <- c(S_0 = 10600000, 
             E1_0 = 35, E2_0 = 35, E3_0 = 35, E4_0 = 35, 
             Ia1_0 = 14, Ia2_0 = 14, Ia3_0 = 14, Ia4_0 = 14, 
             Isu1_0 = 56, Isu2_0 = 56, Isu3_0 = 56, Isu4_0 = 56, 
             Isd1_0 = 56, Isd2_0 = 56, Isd3_0 = 56, Isd4_0 = 56, 
             C1_0 = 2, C2_0 = 2, C3_0 = 2, C4_0 = 2, 
             H1_0 = 2, H2_0 = 2, H3_0 = 2, H4_0 = 2, 
             R_0 = 0,
             D_0 = 0
            )

Ntot <- sum(inivals)  # total population size

# Values for parameters
# beta is scaled by population size here instead of inside the process model
parvals <- c(log_beta_s = log(0.6/Ntot), 
             trans_e = 0.5, #value of 1 means factor is 0.5
             trans_a = 0.5, 
             trans_c = 10, 
             trans_h = 10, 
             beta_reduce = 1,  
             t_int1 = 12,
             t_int2 = 12,
             t_int3 = 12,
             log_g_e = log(4*0.2),
             log_g_a = log(4*0.15),
             log_g_su = log(4*0.15),
             log_g_sd = log(4*0.15),
             log_g_c = log(4*0.3),  #updated
             log_g_h = log(4*0.3),
             log_diag_speedup = log(2), 
             detect_0 = log((1/0.2)-1),
             detect_1 = log((1/0.4)-1), 
             frac_asym = log((1/0.2)-1), # 1.39
             frac_hosp = log((1/0.5)-1), # 2.94
             frac_dead = log((1/0.3)-1), #fraction hospitalized that die, 2.19
             log_theta_cases = log(10),
             log_theta_hosps = log(50),
             log_theta_deaths = log(50)
  )
 
# this should probably not be in this script? 
# pf <- pfilter(pomp_model, params = c(parvals,inivals), Np = 2000)
# plot(pf@cond.loglik, type = "l", xlab = "time", ylab = "Cond. log likelihood")
# sum(pf@cond.loglik)
# logLik(pf)


#run simulation a number of times
sims <- pomp::simulate(pomp_model, 
                       params=c(parvals,inivals), 
                       nsim=3, format="data.frame", 
                       include.data=TRUE)

filename = here('output/model-predictions.RDS')
saveRDS(sims,filename)

pl <- sims %>%
  dplyr::select(time, .id, cases, hosps, deaths) %>%
  tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
  ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = FALSE)

plot(pl)
  
