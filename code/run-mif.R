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
             theta = 100,
             theta_hosp = 100,
             theta_death = 100
)

# pf <- pfilter(pomp_object, params = c(parvals, inivals), Np = 2000)

# Set the parameters to estimate (i.e., those to vary) --------------------

# We have to fix several parameters. E.g. it's impossible to estimate 
# all beta and the reduction factor, they are fully collinear. So, we
# fix all the betas here.

curr_theta <- c(parvals, inivals)
params_to_estimate <- names(parvals)
rmones <- which(params_to_estimate %in% c("t_int1", "t_int2", "t_int3", "rho",
                                          "frac_asym", "frac_hosp", "frac_dead"))
params_to_estimate <- params_to_estimate[-rmones]
params_perts <- rw.sd(log_beta_s = 0.02,
                      trans_e = 0.02,
                      trans_a = 0.02,
                      trans_c = 0.02,
                      beta_reduce = 0.02,
                      log_g_e = 0.02,
                      log_g_a = 0.02,
                      log_g_su = 0.02,
                      log_g_c = 0.02,
                      log_g_h = 0.02,
                      log_diag_speedup = 0.02,
                      detect_0 = 0.02,
                      detect_1 = 0.02,
                      # frac_asym = 0.02,
                      # frac_hosp = 0.02,
                      # frac_dead = 0.02,
                      # rho = 0.02,
                      theta = 0.1,
                      theta_hosp = 0.1,
                      theta_death = 0.1)


# Define "proposal" function for starting values --------------------------

prop_func <- function(theta) {
  rnorm(length(theta), theta, sd = 1)
}


# Run MIF from different starting points ----------------------------------

num_particles <- 5000
num_mif_iterations1 <- 1000
num_mif_iterations2 <- 1000
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
                        cooling.fraction.50 = 0.65, cooling.type = "hyperbolic")
  
  return(mf)
} -> mifs
stopCluster(cl)


# Use particle filter to get the likelihood at the end of MIF run ---------

num_cores <- parallel::detectCores() - 2  # alter as needed
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)
pf1 <- foreach(mf = mifs, .combine = rbind, .packages = c("pomp")) %dopar% {
  pf <- replicate(n = 10, pfilter(mf, Np = 10000, max.fail = Inf))
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
#   filter(iteration > 100) %>%
#   ggplot(aes(x=iteration,y=value,group=L1,color=L1))+
#   geom_line()+
#   facet_wrap(~variable,scales="free_y")+
#   guides(color=FALSE)
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

