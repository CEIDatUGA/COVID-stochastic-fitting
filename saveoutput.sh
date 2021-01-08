#!/bin/bash
# Run this script from the command line after the array job has ended on sapelo2
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
cd ~/COVID-stochastic-fitting
git add --all
git commit -m "Update"
git push origin master
cd ~/cov
rm *.Rout
rm *.err
rm *.out
rm -r [0-9]*

