# run-mif.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)
library(doParallel)
library(foreach)
library(here)


# Load the pomp object ----------------------------------------------------

filename <- here('output/pomp-model.RDS')
pomp_object <- readRDS(filename)


# Initial values ----------------------------------------------------------

inivals <- c(E1_0 = 40, E2_0 = 40, E3_0 = 40, E4_0 = 40, 
             Ia1_0 = 22, Ia2_0 = 22, Ia3_0 = 22, Ia4_0 = 22, 
             Isu1_0 = 90, Isu2_0 = 90, Isu3_0 = 90, Isu4_0 = 90, 
             Isd1_0 = 14, Isd2_0 = 14, Isd3_0 = 14, Isd4_0 = 14)

Ntot <- sum(inivals) + 10600000 + 2*4 + 2*4  # total population size

# Values for parameters
# beta is scaled by population size here instead of inside the process model
parvals <- c(log_beta_s = log(0.6/Ntot), 
             trans_e = 0.5, #value of 1 means factor is 0.5
             trans_a = 0.5, 
             trans_c = 10, 
             trans_h = 10, 
             beta_reduce = -0.61,  
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
             frac_hosp = log((1/0.05)-1), # 2.94
             frac_dead = log((1/0.1)-1), #fraction hospitalized that die, 2.19
             log_theta_cases = log(50),
             log_theta_hosps = log(50),
             log_theta_deaths = log(50)
)


# pf <- pfilter(pomp_object, params = c(parvals, inivals), Np = 2000)
# logLik(pf)

# Set the parameters to estimate (i.e., those to vary) --------------------

curr_theta <- c(parvals, inivals)
params_to_estimate <- names(curr_theta)

params_perts <- rw.sd(log_beta_s = 0.05,
                      trans_e = 0.05,
                      trans_a = 0.05,
                      trans_c = 0.05,
                      trans_h = 0.05,
                      beta_reduce = 0.05,
                      log_g_e = 0.05,
                      log_g_a = 0.05,
                      log_g_su = 0.05,
                      log_g_sd = 0.05,
                      log_g_c = 0.05, 
                      log_g_h = 0.05, 
                      log_diag_speedup = 0.05,
                      detect_0 = 0.05,
                      detect_1 = 0.05,
                      frac_asym = 0.05,
                      frac_hosp = 0.05,  
                      frac_dead = 0.05,  
                      log_theta_cases = 0.1,
                      log_theta_hosps = 0.1,  
                      log_theta_deaths = 0.1,
                      E1_0 = ivp(0.2), 
                      E2_0 = ivp(0.2),
                      E3_0 = ivp(0.2),
                      E4_0 = ivp(0.2), 
                      Ia1_0 = ivp(0.2), 
                      Ia2_0 = ivp(0.2),
                      Ia3_0 = ivp(0.2),
                      Ia4_0 = ivp(0.2), 
                      Isu1_0 = ivp(0.2),
                      Isu2_0 = ivp(0.2), 
                      Isu3_0 = ivp(0.2),
                      Isu4_0 = ivp(0.2), 
                      Isd1_0 = ivp(0.1), 
                      Isd2_0 = ivp(0.1), 
                      Isd3_0 = ivp(0.1),
                      Isd4_0 = ivp(0.1)
)

length(curr_theta)  
length(names(params_perts@call)) - 1  # 1 empty name 

fixed_params <- c(t_int1 = 12, t_int2 = 12,t_int3 = 12,
                  S_0 = 10600000,
                  C1_0 = 2, C2_0 = 2, C3_0 = 2, C4_0 = 2,
                  H1_0 = 2, H2_0 = 2, H3_0 = 2, H4_0 = 2, 
                  R_0 = 0,
                  D_0 = 0)

curr_theta <- c(curr_theta, fixed_params)


# Define "proposal" function for starting values --------------------------

prop_func <- function(theta) {
  rnorm(length(theta), theta, sd = 1)  # this works because everthing is scaled appropriately
}


# Run MIF from different starting points ----------------------------------

num_particles <- 20
num_mif_iterations1 <- 15
num_mif_iterations2 <- 10
#num_particles <- 2000
#num_mif_iterations1 <- 150
#num_mif_iterations2 <- 100
num_cores <- parallel::detectCores() - 2  # alter as needed
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)
foreach (i = 1:num_cores, 
         .packages = c("pomp"),
         .combine = c, 
         .export = c("params_perts", 
                     "prop_func", 
                     "curr_theta")) %dopar% {
                       
                       theta_guess <- curr_theta
                       theta_guess[params_to_estimate] <- prop_func(curr_theta[params_to_estimate])
                       
                       pomp::mif2(pomp_object, Nmif = num_mif_iterations1, params = theta_guess, 
                                  Np = num_particles, cooling.fraction.50 = 0.8, 
                                  cooling.type = "geometric", rw.sd = params_perts) -> mf
                       
                       mf <- pomp::continue(mf, Nmif = num_mif_iterations2,
                                            cooling.fraction.50 = 0.65, cooling.type = "geometric")
                       
                       return(mf)
                     } -> mifs
stopCluster(cl)


# Use particle filter to get the likelihood at the end of MIF run ---------

cl <- parallel::makeCluster(length(mifs))
registerDoParallel(cl)
pf1 <- foreach(mf = mifs, .combine = rbind, .packages = c("pomp")) %dopar% {
  pf <- replicate(n = 10, pfilter(mf, Np = 5000, max.fail = Inf))
  ll <- sapply(pf, logLik)
  ll <- logmeanexp(ll, se = TRUE)
}
parallel::stopCluster(cl)

mif_coefs <- data.frame(matrix(unlist(sapply(mifs, coef)),
                               nrow = length(mifs), byrow = T))
colnames(mif_coefs) <- names(coef(mifs[[1]]))

pf_logliks <- as_tibble(pf1) %>%
  rename("LogLik" = V1,
         "LogLik_SE" = se) %>%
  mutate(MIF_ID = 1:n()) %>%
  dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
  bind_cols(mif_coefs) %>%
  arrange(-LogLik)



# Save output -------------------------------------------------------------

mifRets <- list(mif_objects = mifs, loglik_dfs = pf_logliks)
outfile <- here('output/mif-results.RDS')
saveRDS(object = mifRets, file = outfile)




# Cache -------------------------------------------------------------------

# mifs %>%
#   traces() %>%
#   melt() %>%
#   filter(variable %in% c("loglik", params_to_estimate)) %>%
#   # filter(iteration > 100) %>%
#   ggplot(aes(x=iteration,y=value,group=L1,color=as.factor(L1)))+
#   geom_line()+
#   facet_wrap(~variable,scales="free_y")+
#   scale_color_brewer(type = "qual") +
#   guides(color=FALSE)
# 
# sims <- pomp::simulate(mifs[[6]],
#                        nsim=1, format="data.frame",
#                        include.data=TRUE)
# 
# 
# sims %>%
#   dplyr::select(time, .id, cases, hosps, deaths) %>%
#   tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
#   ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y") +
#   guides(color = FALSE)
# 
# sims %>%
#   dplyr::select(time, .id, H1, hosps) %>%
#   tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
#   ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y") +
#   guides(color = FALSE)

# 
# 

# Extract and save best parameter set for MCMC ----------------------------
# 
# mf1 <- mifs[[which.max(pf1)]]
# theta_mif <- coef(mf1)
# saveRDS("../output/mif-mles.RDS")





# Cache -------------------------------------------------------------------

# 
# # Question: Are there rules of thumb for specifying Nmif, Np, coooling.fraction and rw.sd? Or ways to diagnose if one is choosing them right?
# # Other question: Is this only estimating those parameters that are specified in rw.sd and all others are assumed fixed?
# 
# # pf <- pfilter(covid_ga_pomp, params = coef(covid_ga_pomp), Np = 1000)
# 
# test <- mif2(pomp_object, Nmif = 50, params = theta.guess, 
#              Np = 2000, cooling.fraction = 1,
#              rw.sd = rw.sd(beta_red_factor = 0.02, gamma_u = 0.02,
#                            gamma_d = 0.02, detect_frac_0 = 0.02))
# 
# 
# mifs <- foreach (i = 1:10, .combine = c) %dopar% {   #Inspect from multiple, randomly chosen starting points
#         theta.guess <- theta.true
#         theta.guess[estpars] <- rlnorm(n = length(estpars),
#                                  meanlog = log(theta.guess[estpars]), sdlog = 1)
#         } 

