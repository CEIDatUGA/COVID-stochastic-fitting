#PBS -S /bin/bash
#PBS -q covid19_q
#PBS -N covstates
#PBS -l nodes=1:ppn=32
#PBS -l walltime=24:00:00
#PBS -l pmem=3500mb
#PBS -M tierney6@uga.edu
#PBS -t 1-50

cd $PBS_O_WORKDIR

module load R/3.6.2-foss-2018a-X11-20180131-GACRC

mkdir ${PBS_JOBID}
cd ${PBS_JOBID}
R CMD BATCH "--args a=$PBS_ARRAYID " ../code/00-RUN-CLUSTER-ARRAY.R
