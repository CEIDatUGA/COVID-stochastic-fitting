# make-complete-pomp.R
#
# This script generates a complete pomp object for fitting to data

# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------
library(pomp)
library(here) #to simplify loading/saving into different folders

# Load cleaned data ---------------------------------------------------------
filename = here('output/clean-data.RDS')
covid_ga_data <- readRDS(filename)



# Define likelihood function ----------------------------------------------

dmeas <- Csnippet(
  "
  lik = dnbinom_mu(cases, theta, rho * C, give_log);
  "
)


# Define process simulator for observations  ------------------------------

rmeas <- Csnippet(
  "
  cases = rnbinom_mu(theta, rho * C);
  "
)

# Load pomp simulator object ---------------------------------------------------------
filename = here('output/covid-pomp-model.RDS')
covid_pomp_model <- readRDS(filename)


# Define the pomp model object --------------------------------------------

covid_pomp_model <- covid_pomp_model %>%
                  pomp(
                data = covid_ga_data[1:25, ],  # currently removes NAs
                times = "days",
                t0 = 0,
                dmeasure = dmeas,
                rmeasure = rmeas
            ) 



# Save the pomp object ----------------------------------------------------
filename = here('output/covid-pomp-complete.RDS')
saveRDS(covid_pomp_model, filename)

