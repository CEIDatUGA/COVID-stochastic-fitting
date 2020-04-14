# explore-mif-results.R
# This script loads results produced by run-mif for exploration/plotting

# Clear the decks ---------------------------------------------------------
rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(dplyr)
library(pomp)
library(here)

# load results produced by mif fitting ----------------------------------------------------
# this is a list of mif objects for each initial condition 
# followed by pfilter objects run a specified number of times after each mif is run
filename = here('output/mif-results.RDS')
mif_res_list <- readRDS(filename)
mifs = mif_res_list$mif_runs
pfs = mif_res_list$pf_runs


# Compute some results -------------------------------------------------------
# for each initial condition, take the pf runs and compute mean log likelihood
n_ini_cond = length(mifs)
ll = list()
for (i in 1:n_ini_cond) #do last part not in parallel
{
  ll1 <- sapply(pfs[[i]], logLik)
  ll[[i]] <- logmeanexp(ll1, se = TRUE)
}

# get estimated values for all parameters that were estimated for each run 
mif_coefs <- data.frame(matrix(unlist(sapply(mifs, coef)), 
                               nrow = length(mifs), 
                               byrow = T))
colnames(mif_coefs) <- names(coef(mifs[[1]]))  # names are the same for all mifs

# convert the list containing the log likelihoods for 
# each run stored in ll into a data frame
ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T))

# combine the ll_df and mif_coefs data frames. 
# Also do some cleaning/renaming
pf_logliks <- ll_df %>%
  dplyr::rename("LogLik" = X1,
                "LogLik_SE" = X2) %>%
  dplyr::mutate(MIF_ID = 1:n()) %>%
  dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
  bind_cols(mif_coefs) %>%
  dplyr::arrange(-LogLik)

print(pf_logliks)





mifs[[1]] %>% 
  traces() %>%
  melt() %>%
  filter(variable %in% c("loglik", params_to_estimate)) %>%
  # filter(iteration > 100) %>%
  ggplot(aes(x=iteration,y=value,group=L1,color=as.factor(L1)))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  scale_color_brewer(type = "qual") +
  guides(color=FALSE)

sims <- pomp::simulate(mifs[[1]],
                       nsim=1, format="data.frame",
                       include.data=TRUE)


sims %>%
  dplyr::select(time, .id, cases, hosps, deaths) %>%
  tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
  ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = FALSE)

sims %>%
  dplyr::select(time, .id, H1, hosps) %>%
  tidyr::gather(key = "variable", value = "value", -time, -.id) %>%
  ggplot(aes(x = time, y = value, group = .id, color=.id=="data")) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = FALSE)



