# Script explanation

load-clean-data.R: loads, cleans/processes and saves data
make-pomp-model.R: makes the simulator part of the model. Uses fake/dummy data and rmeas. Can be used for simulation, not fitting. 
simulate-pomp-model.R: runs the simulator model to generate synthetic data or for exploration
make-complete-pomp.R: loads the clean data, specifies measurement model. Saves full pomp object ready for fitting.
