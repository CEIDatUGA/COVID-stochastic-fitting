# COVID-stochastic-fitting

Repository of code and data for fitting state-level stochastic models of SARS-CoV-2 transmission in the United States of America.

## Overview
Please see *website link* for a  description of the model, the fitting procedure, results, and more details.
In brief, this repository contains the data and code needed to reproduce our work fitting an SEIR model to daily reports of SARS-CoV-2 cases and deaths for 50 states in the USA.
Our work relies heavily on the R package **pomp**.

## How to generate results
There are three main routines associated with generating results:

1. Download data and create **pomp** model objects for each state
2. Fit the model to the data using the **pomp**::`mif2` function to perform maximum likelihood estimation by iterated filtering.
3. Simulate and project from the fitted model

Fitting the models for all 50 states is computationally demanding and requires the use of a High Performance Computing cluster.
All scripts in this repository, and the instructions that follow, are set up to run on the University of Georgia's HPC.
These scripts also assume you have access to the Drake Lab covid19_q resources on the GACRC.

To run the three routines above, follow these steps:

1. Transfer the `code/` and `data/` directories to your home directory on Sepalo2.
2. Transfer the `mif-header.sh` and `mif-array.sh` files to your home directoy on Sepalo2.
3. From your home directory on Sapelo2, execute the following commands to make sure the bash files can be run on the Linux machines
   - `dos2unix mif-header.sh`
   - `dos2unix mif-array.sh`
4. Submit the header job to download data and create **pomp** model objects.
   - `qsub mif-header.sh`
   - Check to make sure `output/` and `header/` directories have been made after the job completes.
5. Submit the job to fit and simulate the model.
   - `qsub mif-array.sh`
   - Wait about 5-6 hours
6. Once the job is complete, transfer all files in the `output/` directory to your local machine where this repository resides.
7. Now all reports and web content can be generated.

