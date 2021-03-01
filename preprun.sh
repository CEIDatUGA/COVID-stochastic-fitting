#!/bin/bash
# Run this script from the command line after the mif-array.sh has run on sapelo2
# first cd to top level project directory, then:
# >bash preprun.sh
cd ~/COVID-stochastic-fitting
git pull
cp -r ~/COVID-stochastic-fitting/data/ ~/cov/
cp -r ~/COVID-stochastic-fitting/code/ ~/cov/
cp ~/COVID-stochastic-fitting/sub_slurm_* ~/cov/
