#!/bin/bash
# Run this script from the command line after the mif-array.sh has run on sapelo2
# first cd to top level project directory, then:
# >bash saveoutput.sh
cd ~/COVID-stochastic-fitting  
git pull  
cp -r ~/cov/output/current/ ~/COVID-stochastic-fitting/output/
cd ~/cov/output
LATESTDIR=$(ls -td 202* | head -1)
LATESTDIRFULL="full$(echo $LATESTDIR | sed -e 's/-//g')"
LATESTDIRFULLPATH="/work/covid19lab/full/$LATESTDIRFULL"
cp -r $LATESTDIR ~/COVID-stochastic-fitting/output/
cd /work/covid19lab/full
mkdir $LATESTDIRFULL
cp ~/cov/output/*.rds $LATESTDIRFULLPATH
