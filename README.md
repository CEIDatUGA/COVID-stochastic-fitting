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
These scripts also assume you have access to the Drake Lab `covid19_p` resources on the GACRC sapelo2 cluster.

## Workflow 

### Login to GACRC using the sapelo2 cluster using your uga myid and ArchPass

```
ssh [myid]@sapelo2.gacrc.uga.edu
```

### Check to ensure no other job is running on the covid19_p partition (queue). 

This command shows all running and recently completed jobs:

```
sacct-gacrc
```

### Ensure you have most recent unacast data spline fits and code

```
cd ~/COVID-stochastic-fitting
git pull
```

### if you want to change any of the initial R0 or warm start parameters:

```
nano ~/COVID-stochastic-fitting/code/00-CREATE-HEADER.R
```

- Go down to `# specify parameter initialization and mif runs for each state`

- To change wamr start parameters:  
   - Go to `# warm start spec for each state`  
   - `state_full == "[state]" ~ "last",` If previous fit was good, use the last run for the warm start  
   - `state_full == "[state]" ~ "fresh",` For a fresh start for that state  
   - `state_full == "[state]" ~ "YYYY-MM-DD",` to set a specific date for the warm start  

- To change initial R0 from default = 6 to higher or lower depending on how the previous runs went
   - Go to `# R0 at beginning of epidemic for each state`  
   - if it looks like it isn’t ramping up quickly enough: increase R0
   - if it looks like it is ramping up too quickly: decrease R0

- ignore Washington: this is going to be fixed by eliminating stuttering chains at the start

### Copy files to temporary directory:

```
cd ~
cp -r COVID-stochastic-fitting/data/ cov/
cp -r COVID-stochastic-fitting/code/ cov/
```

### Submit via slurm:

1. To modify the email address for notification, edit the line `#SBATCH --mail-user=myid@uga.edu`
in files `sub_slurm_1.sh` and `sub_slurm_2.sh`

```
nano ~/cov/sub_slurm_1.sh
```

To modify job time allocated, edit the following line in `sub_slurm_2.sh` :

`line `#SBATCH --time=1-12:00:00  # Time limit days-hrs:min:sec`

<!--From your home directory on Sapelo2, execute the following commands to make sure the bash files can be run on the Linux machines
   - `dos2unix mif-header.sh`
   - `dos2unix mif-array.sh`
-->

2. Submit header job to download data and create **pomp** model objects.: 

```
cd ~/cov  
sbatch sub_slurm_1.sh
```

This script takes ~7 seconds to run. To check that it is running: 

```
squeue --me
```

Make sure files have shown up:

```
ls 
```

You should see directories `output/` and `header/` 
and files `00-CREATE-HEADER.Rout`, `covstateshdr.*.out`, `covstateshdr.*.err`.

3. Submit array job: 

```
cd ~/cov  
sbatch sub_slurm_1.sh
squeue --me
```

            JOBID    PARTITION   NAME     USER ST  TIME  NODES NODELIST(REASON) 
    xxxxxx_[16-50]   covid19_p   covstate user PD  0:00      1 (Resources) 
          xxxxxx_1   covid19_p   covstate user  R  0:08      1 d2-6 
          xxxxxx_2   covid19_p   covstate user  R  0:08      1 d2-10 
            ...

Make sure files are appearing. 

```
ls
```

You should see several multi-digit numbers, and several files `slurm-*_*.out`

Wait at least 24 hours. You may log off. Slumr will email the user specified in the `sub_slurm_2.sh` once the job ends or fails.  
**Note: the gacrc email notification system is not working as of Nov 10, 2020.**

## Log back in and transfer files :

Make sure job is complete.  

```
sacct-gacrc
```
...to see a list of completed and running jobs, or

```
squeue --me
```
...to see a list of running jobs only.

If all jobs have run successfully:

```
cd ~/COVID-stochastic-fitting  
git pull  
cd ~
cp -r ~/cov/output/current/ ~/COVID-stochastic-fitting/output/
cd ~/cov/output
ls
```

Note the latest directory name in the format `YYYY-MM-DD`, e.g. `2020-10-14`.
Copy that directory to the repo:

```
cp -r ~/cov/output/2020-10-14/ ~/COVID-stochastic-fitting/output/
```

## Push updates

```
cd ~/COVID-stochastic-fitting
git add --all
git commit -m "Update"
git push origin master
```

## Clean up temporary directory

The file `cleandir.sh` can be used to clean up temporary files. 
The script may need to be copied over to the ~/cov from the repo.  
This script must be run from the directory to be cleaned up.

```
cd ~/cov
bash cleandir.sh
```

This is equivalent to 

```
cd ~/cov
cd rm covstates*
rm -r *.sapelo2
rm *.Rout
rm *.err
rm *.out
```

## Generate reports

Basic Report generation is automated and runs twice daily. If a manual run is needed:

From local machine, `cd` into the local repo directory, then pull.

```
cd [path]/COVID-stochastic-fitting/
git pull
cd ./code
Rscript autoupdate_docs.R
git add -A
git commit -m "manual update of docs and shinydata"
git push origin master
```

View `[repo]/docs/plots.html` to see most recent fits and parameters for all states.

## Generate detailed state reports

If fits over time are reuqired for a subset of states, you go can do be editing and sourcing the following script `/COVID-stochastic-fitting/code/plotting/render_diagnostic_plots.R`

Edit the first section to include the states for which you want detailed reports and the start and end dates for the reports. Each report will include pltos of data, fits and covariates for each date.

```
states <- c("Indiana", "Massachusetts", "Michigan", "New Jersey", "Ohio", "Washington")
startdate <- "2020-09-01" # Y-m-d
enddate <- format(Sys.time(), '%Y-%m-%d')
```

Reports appear as dated *.html files in `/COVID-stochastic-fitting/docs/plots/`

Change `00-CREATE-HEADER.R` as needed prior to next run (see above).

Push to repo

```
git push origin master
```
