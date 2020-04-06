#######################################################################
# doing a stochastic simulator with pomp
# no data fitting
# trying to do COVID
#######################################################################
#examples are here
#https://kingaa.github.io/pomp/vignettes/oaxaca.html#a_more_complex_example:_a_stochastic,_seasonal_sir_model
#https://kingaa.github.io/pomp/vignettes/He2010.html
#https://kingaa.github.io/sbied/index.html
#https://kingaa.github.io/sbied/stochsim/stochsim.html
#https://kingaa.github.io/sbied/stochsim/exercises.html#basic-exercise-the-seir-model
#https://kingaa.github.io/sbied/polio/polio.html

## ----prelims,cache=FALSE-------------------------------------------------
set.seed(123L)
library(ggplot2)
library(dplyr)
library(tidyr)
library(pomp)

#######################################################################
#specify functions for pomp first
#######################################################################

#######################################################################
#1 step for model
#C-snippet
#######################################################################
covid_step_C <- pomp::Csnippet("
  double trans[17]; //C indexes at 0, I hate that so I'm making things 1 bigger and start with index 1, i.e. leave trans[0] empty
  double Epresymptom;
  double Idetected;
  double Iundetected;
  double foi; //force of infection
  double gamma; // rate of transition through I compartments
  double detect_frac; //fraction of those that get eventually diagnosed

  Epresymptom = E1+E2+E3+E4+E5+E6;  //all pre-symptomatic
  Idetected = I1+I2+I3+I4;          //all symptomatic that wil be detected
  Iundetected = Iu1+Iu2+Iu3+Iu4;    //all symptomatic/asymptomatic that won't be detected

  //force of infection
  //time dependent transmission, multiplied by different groups
  //each group can have its own transmission rate
  //t_int1 days after simulation start, an intervention reduces transmission rate by some factor
  //t_int1 is new, not in original code. There it was assumed to be the same as t_int2

  if (t<=t_int1)
        foi = beta_d*Idetected + beta_u*Iundetected + beta_e*Epresymptom;
  else
        foi = beta_red_factor*(beta_d*Idetected + beta_u*Iundetected + beta_e*Epresymptom);


  //time-dependent rate of movement through infected and detected classes
  //t_int2 days after simulation start, the time at which individuals are diagnosed and thus the time spent in the I categories before ending up in H decreases
  //t_int2 is caled z in original code

  if (t<t_int2) //if time is less then intervention time, duration spent in I is given by 1/gamma_u, otherwise 1/gamma_d
      gamma = gamma_u;
  else
      gamma = gamma_d;

  //time dependent fraction of those that move into detected category at the end of the E phase
  //t_int3 days after simulation start, the fraction detected (those that move into I instead of Iu after exiting E) increases
  //note that both higher fraction detected and faster rate of detection speed up arrival of individuals in H and subsequently C
  //t_int3 is called w in original code, detect_frac is called q/q0/q1 in the original code

  if (t<t_int3)
    detect_frac = detect_frac_0;
  else
    detect_frac = detect_frac_1;


  // define all transmission rates
  trans[1] = rbinom(S,1-exp(-foi*dt));              //transition from S to E
  trans[2] = rbinom(E1,1-exp(-sigma*dt));           // transition between E compartments 1/2
  trans[3] = rbinom(E2,1-exp(-sigma*dt));           // transition between E compartments
  trans[4] = rbinom(E3,1-exp(-sigma*dt));           // transition between E compartments
  trans[5] = rbinom(E4,1-exp(-sigma*dt));           // transition between E compartments 4/5
  trans[6] = rbinom(E5,1-exp(-sigma*dt));           // transition between E compartments 5/6

  trans[7] = rbinom(E6,(1-exp(-sigma*dt))*detect_frac);           // transition between E6 compartment and I
  trans[8] = rbinom(E6,(1-exp(-sigma*dt))*(1-detect_frac));           // transition between E6 compartment and Iu

  trans[9] = rbinom(I1,1-exp(-gamma*dt));           // transition between I compartments 1/2
  trans[10] = rbinom(I2,1-exp(-gamma*dt));           // transition between I compartments 2/3
  trans[11] = rbinom(I3,1-exp(-gamma*dt));           // transition between I compartments 3/4
  trans[12] = rbinom(I4,1-exp(-gamma*dt));          // transition between I compartments and C

  trans[13] = rbinom(Iu1,1-exp(-gamma_u*dt));           // transition between Iu compartments 1/2
  trans[14] = rbinom(Iu2,1-exp(-gamma_u*dt));           // transition between Iu compartments 2/3
  trans[15] = rbinom(Iu3,1-exp(-gamma_u*dt));           // transition between Iu compartments 3/4
  trans[16] = rbinom(Iu4,1-exp(-gamma_u*dt));           // transition between Iu compartments and Ru

  // define all transmissions for each compartment
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

  C += trans[12]; //detected cases, assumed to be isolated and not further contribute to transmission
  Ru += trans[16]; //undetected cases that recover, assumed to not further contribute to transmission

")


#######################################################################
#initial conditions for model
#######################################################################
covid_init_C <- pomp::Csnippet("
    S = S_0; E1 = E1_0;  E2 = E2_0;  E3 = E3_0;  E4 = E4_0;  E5 = E5_0;  E6 = E6_0;
    I1 = I1_0;  I2 = I2_0;  I3 = I3_0;  I4 = I4_0;
    Iu1 = Iu1_0; Iu2 = Iu2_0; Iu3 = Iu3_0; Iu4 = Iu4_0;  C = C_0; Ru = Ru_0;
    ")

#######################################################################
#not currently used since we don't fit
#added to make pomp stop produce warning messages
#######################################################################
rmeas <- pomp::Csnippet("
      cases = C;
    ")

#variable names
varnames = c("S", "E1", "E2", "E3", "E4", "E5", "E6", "I1", "I2", "I3", "I4", "Iu1", "Iu2", "Iu3", "Iu4", "C", "Ru")
#parameter and variable names
parnames1 = c("beta_d", "beta_u", "beta_e", "beta_red_factor", "t_int1", "t_int2", "t_int3", "gamma_u", "gamma_d", "detect_frac_0","detect_frac_1","sigma")
#initial conditions of state variables are also parameters
parnames2 = c("S_0", "E1_0", "E2_0", "E3_0", "E4_0", "E5_0", "E6_0", "I1_0", "I2_0", "I3_0", "I4_0", "Iu1_0", "Iu2_0", "Iu3_0", "Iu4_0", "C_0", "Ru_0")
parnames = c(parnames1,parnames2)


#######################################################################
#data loading
#note that we don't fit, but supply the data to the pomp object anyway since it wants data
#######################################################################
#data <- read.csv('https://raw.githubusercontent.com/CEIDatUGA/COVID-19-DATA/master/GA_daily_status_report_GDPH.csv?token=ABQMQXYHNZXBU7W3UPEIJCS6QKTCE')
data = read.csv('GA_daily_status_report_GDPH.csv')
data$date <- as.Date(data$date, format='%m/%d/%y')

covid_ga_data <- data %>% dplyr::select(date, cases_cumulative, fatalities_cumulative) %>%
                          tidyr::replace_na(list(cases_cumulative = 0, fatalities_cumulative = 0)) %>%
                          dplyr::mutate(days = 1:nrow(data)) %>%
                          dplyr::mutate(cases = cases_cumulative) %>%
                          dplyr::select(days, cases)

#extend data frame holding data by a month so that pomp runs simulations for that long

future = data.frame(days = max(covid_ga_data$days):(max(covid_ga_data$days)+31), cases = NA )
covid_ga_data = rbind(covid_ga_data,future)

#######################################################################
#create pomp object
#######################################################################

covid_model_C <- pomp(data = covid_ga_data,
                times="days",t0=0,
                rprocess=euler(covid_step_C,delta.t=0.05),
                rmeasure = rmeas,
                rinit=covid_init_C,
                paramnames = parnames,
                statenames = varnames
              )



#######################################################################
#run some simulations for different conditions
#######################################################################
#initial conditions for states
inivals = c(S_0 = 10600000, E1_0 = 35, E2_0 = 35, E3_0 = 35, E4_0 = 35, E5_0 =35, E6_0 =35, I1_0 = 14, I2_0 = 14, I3_0 = 14, I4_0 = 14, Iu1_0 = 111, Iu2_0= 111, Iu3_0= 111, Iu4_0= 111, C_0  = 0, Ru_0 = 0 )
Ntot = sum(inivals)

#values for parameters. beta is scaled by population size here instead of inside the simulation function
parvals = c(beta_d = 0.5/Ntot, beta_u = 0.25/Ntot, beta_e = 0.1/Ntot, beta_red_factor = 0.5, t_int1 = 12, t_int2 = 12, t_int3 = 12, gamma_u = 4*0.1, gamma_d = 4*0.5, detect_frac_0 = 0.1, detect_frac_1 = 0.2, sigma = 6*0.18)

#run simulation a number of times
# sims <- pomp::simulate(covid_model_C,
#                          params=c(parvals,inivals),
#                          nsim=25, format="data.frame", include.data=TRUE)
# 
# # plot result and data
# startdate = as.Date('2020-03-01')
# dates = seq(from = startdate, to = startdate + nrow(covid_ga_data), "days" )
# 
# pl <- sims %>%
#   ggplot(aes(x=days,y=cases,group=.id,color=.id=="data")) +
#   geom_line() +
#   scale_x_continuous(breaks = seq(1,length(dates),by=10),  labels = dates[seq(1,length(dates),by=10)]) +
#   scale_y_continuous( trans = 'log10') +
#   guides(color=FALSE)
# plot(pl)


