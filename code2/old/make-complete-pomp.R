# make-complete-pomp.R
# This script generates a complete pomp object for fitting to data
# This specifies the actual data and the measures

# Load libraries ----------------------------------------------------------
library(pomp)
library(here) #to simplify loading/saving into different folders




# Load pomp simulator object ---------------------------------------------------------
filename = here('output2/covid-pomp-simulator.RDS')
pomp_model <- readRDS(filename)

browser()


# Define the pomp model object --------------------------------------------

pomp_model <- pomp_model %>%
                  pomp(
                data = pomp_data[1:25, ],  # currently removes NAs
                times = "time",
                t0 = 0,
                dmeasure = dmeas,
                rmeasure = rmeas,
                rinit = rinit,
                rprocess = euler(step.fun = covid_step_C, delta.t = 1/20),
                partrans = param_transforms,
                params = c(parvals, inivals)
                paramnames = c(newpars, currentpars),
                statenames = varnames,
                obsnames = c("cases"),
                accumvars = c("C")
            ) 


covid_ga_pomp <- pomp(
)




# Save the pomp object ----------------------------------------------------
filename = here('output2/complete-pomp-model.RDS')
saveRDS(pomp_model, filename)

