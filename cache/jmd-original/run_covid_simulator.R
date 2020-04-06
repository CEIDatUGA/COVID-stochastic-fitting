#short script that runs covid simulator for different settings


scenarios = read.csv('scenariolist.csv')


# to use simulator function, either load DSAIDE or source function
# library(DSAIDE)
source('simulate_covid_stochastic.R')

#number replicates
nreps = 25;
allsims  = list()

for (n1 in 1:nrow(scenarios)) #outer loop over scenarios
{

  sims = list()
  parvals_scenario = scenarios[n1,3:ncol(scenarios)]
  print(sprintf('Starting scenario %d',n1))

  for (n2 in 1:nreps) #innver loop over replicates
  {
    parvals = cbind(parvals_scenario,  tmax = 100, rngseed = 100+n2)
    sims[[n2]] = do.call(simulate_covid_stochastic,as.list(parvals))
  }
  allsims[[n1]] = sims
}

