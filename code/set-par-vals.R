# set-par-vals.R

#this is a simple script that assigns values for initial conditions and parameters
#the idea is that there is a single place to specify them and will be loaded by other scripts

library(here)

#inital conditions are also considered parameters and some are estimated
inivals <- c(S_0 = 10600000, 
             E1_0 = 40, E2_0 = 40, E3_0 = 40, E4_0 = 40, 
             Ia1_0 = 22, Ia2_0 = 22, Ia3_0 = 22, Ia4_0 = 22, 
             Isu1_0 = 90, Isu2_0 = 90, Isu3_0 = 90, Isu4_0 = 90, 
             Isd1_0 = 14, Isd2_0 = 14, Isd3_0 = 14, Isd4_0 = 14, 
             C1_0 = 2, C2_0 = 2, C3_0 = 2, C4_0 = 2, 
             H1_0 = 2, H2_0 = 2, H3_0 = 2, H4_0 = 2, 
             R_0 = 0,
             D_0 = 0
)

Ntot <- sum(inivals)  # total population size

parvals <- c(log_beta_s = log(0.4/Ntot), 
             trans_e = 2, 
             trans_a = 0, 
             trans_c = 1, 
             trans_h = 10, 
             beta_reduce = 0,  
             t_int1 = 12,
             t_int2 = 12,
             t_int3 = 12,
             log_g_e = log(4/4),
             log_g_a = log(4/3.5),
             log_g_su = log(4/6),
             log_g_sd = log(4/3),
             log_g_c = log(4/3),  
             log_g_h = log(4/12),
             log_diag_speedup = log(2), 
             detect_0 = 2,
             detect_1 = 0, 
             frac_asym = 1.5, 
             frac_hosp = 3, 
             frac_dead = 1.2, 
             log_theta_cases = log(10),
             log_theta_hosps = log(10),
             log_theta_deaths = log(10)
)

allparvals = c(parvals,inivals)
filename = here('output/parvals.RDS')
saveRDS(allparvals,filename)
