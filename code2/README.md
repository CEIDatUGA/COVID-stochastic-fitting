# Script explanation

load-clean-data.R: loads, cleans/processes and saves data
make-pomp-model.R: builds the pomp model
simulate-pomp-model.R: runs the simulator model to generate synthetic data or for exploration

fit-pomp-with-abc.R: loads the complete pomp object, fits using ABC approach
fit-pomp-with-mif.R: loads the complete pomp object, fits using MIF approach
fit-pomp-with-pmcmc.R: loads the complete pomp object, fits using particle MCMC approach



# Ideas for old setup, not quite working
make-pomp-simulator.R: makes the simulator part of the model. Uses fake/dummy data and rmeas. Can be used for simulation, not fitting. 
make-complete-pomp.R: loads the clean data, specifies measurement model. Saves full pomp object ready for fitting.
