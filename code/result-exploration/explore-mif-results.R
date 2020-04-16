# explore-mif-results.R
# This script loads results produced by run-mif for exploration/plotting


#  ---------------------------------------------------------
#To-do ideas
#Haven't done this yet, but it'd be nice to have a function that back transforms the MLEs to the natural scale so we can easily inspect the parameter values
#Correlations of parameters across MIF iterations
#bivariate histograms of likelihoods/posteriors
#  ---------------------------------------------------------



# Clear the decks ---------------------------------------------------------
rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(dplyr)
library(pomp)
library(purrr)
library(ggplot2)
library(here)

# load results produced by mif fitting ----------------------------------------------------
# this is a list of mif objects for each initial condition 
# followed by pfilter objects run a specified number of times after each mif is run
filename = here('output/mif-results.RDS')
mif_res_list <- readRDS(filename)
mifs = mif_res_list$mif_runs
pfs = mif_res_list$pf_runs


# ---------------------------------------------------------
#Traceplots of the mif iterations
# ---------------------------------------------------------
# take list of mifs, get traces, merge into a data frame with a column for mif_run
mif_df <- mifs %>% purrr::map(traces) %>% purrr::map(melt) %>% dplyr::bind_rows( .id = "mif_run") 
  
#make a plot of traces for all mif runs  
pl <- mif_df %>% ggplot(aes(x=iteration,y=value,group= mif_run, color=factor(variable)))+
  geom_line()+
  guides(color=FALSE)+
  facet_wrap(~variable,scales="free_y")+
  theme_bw()
plot(pl)


# ---------------------------------------------------------
# Make a data frame that contains best fit parameter estimates for each mif run, 
# as well as the mean likelihood. The latter comes from the pfilter run at the end of each mif run
# ---------------------------------------------------------

# for each mif run, take the pf runs and compute mean log likelihood
n_ini_cond = length(mifs)
ll = list()
for (i in 1:n_ini_cond) #do last part not in parallel
{
  ll1 <- sapply(pfs[[i]], logLik)
  ll[[i]] <- logmeanexp(ll1, se = TRUE)
}
# convert the list containing the log likelihoods for 
# each run stored in ll into a data frame
ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T))

# extract best fit paramter values for each mif run
coef_est_df <- data.frame(matrix(unlist(sapply(mifs, coef)), 
                               nrow = length(mifs), 
                               byrow = T))
colnames(coef_est_df) <- names(coef(mifs[[1]]))  


# combine the ll_df and coef_est_df data frames. 
# Also do some cleaning/renaming
mif_result_df <- ll_df %>%
  dplyr::rename("LogLik" = X1,
                "LogLik_SE" = X2) %>%
  dplyr::mutate(MIF_ID = 1:n()) %>%
  dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
  bind_cols(coef_est_df) %>%
  dplyr::arrange(-LogLik)

print(mif_result_df)


# Likelihood slices for mif results -------------------------------------------------------
# take best fit parameter values for each mif, run a particle filter to compute likelihood
# scan over various parameters (while keeping others at MLE values)
# produce slices
# is too computationally intensive to do for all parameters, 
# currently experimental
filename = here('output/var-par-definitions.RDS')
par_var_list <- readRDS(filename)
filename = here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)

p1 = par_var_list$allparvals["log_beta_s"]
p2 = par_var_list$allparvals["frac_asym"]

pslice <- sliceDesign(
  center=par_var_list$allparvals,
  log_beta_s = rep(seq(from = 0.1*p1, to = 10*p1, length = 20),each=3),
  frac_asym = rep(seq(from = 0.1*p2, to = 10*p2, length = 20) ,each=3)
) 

library(foreach)
library(doParallel)
library(doRNG)

registerDoParallel()
registerDoRNG(108028909)

slicefit <- foreach (theta=iter(pslice,"row"),
                .combine=rbind,.inorder=FALSE) %dopar% {
                library(pomp)
                pf <- pomp_model %>% pfilter(params=theta,Np=2000) 
                theta$loglik <- logLik(pf)
                return(theta)
         } 

sliceplot <- slicefit %>% 
  gather(variable,value,log_beta_s,frac_asym) %>%
  filter(variable==slice) %>%
  ggplot(aes(x=value,y=loglik,color=variable))+
  geom_point()+
  facet_grid(~variable,scales="free_x")+
  guides(color=FALSE)+
  labs(x="parameter value",color="")+
  theme_bw()

plot(sliceplot)

#for now, save all objects in workspace to a file
#not a good way of saving things, just for now


