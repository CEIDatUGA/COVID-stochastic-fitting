#PBS -S /bin/bash
#PBS -q covid19_q
#PBS -N fitcovid_ah
#PBS -l nodes=1:ppn=30
#PBS -l walltime=24:00:00
#PBS -l mem=2gb
#PBS -M ahandel@uga.edu
#PBS -m ae

cd $PBS_O_WORKDIR

module load R/3.6.2-foss-2018a-X11-20180131-GACRC

R CMD BATCH ./code/00-MASTER-RUN-ANALYSIS.R