# explore-mif-results.R
# This script loads results produced by run-mif for exploration/plotting

# Clear the decks ---------------------------------------------------------
rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(dplyr)
library(pomp)
library(doParallel)
library(foreach)
library(here)

# Specify if we want to parallelize or not ---------------------------------------------------------------
n_cores = 1 #if not parallel, this should be 1, otherwise for parallel set a different value  below

parallel_run = TRUE #uncomment one or the other to run parallel or not
#parallel_run = FALSE

# Turn on parallel or not ---------------------------------------------------------------
if (parallel_run == TRUE)
{
  library(doParallel)
  library(foreach)
  # Set up parallel structure ----------------------------------------------------
  n_cores = 4
  cl <- makeCluster(n_cores) 
  registerDoParallel(cl)
}

# Load the pomp object ----------------------------------------------------
filename <- here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)

# load values for model parameters and initial conditions ----------------------------------------------------
filename = here('output/parvals.RDS')
allparvals <- readRDS(filename)

# load results produced by mif fitting ----------------------------------------------------
filename = here('output/mif-results.RDS')
mifresults <- readRDS(filename)

mifs = mifresults$mif_objects



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



