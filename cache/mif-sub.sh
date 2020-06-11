#PBS -S /bin/bash
#PBS -q covid19_q
#PBS -N covstates
#PBS -l nodes=10:ppn=31
#PBS -l walltime=24:00:00
#PBS -l pmem=3500mb
#PBS -M atredennick@west-inc.com
#PBS -m abe

cd $PBS_O_WORKDIR

# module load R/3.6.2-foss-2018a-X11-20180131-GACRC
module load  R/4.0.0-foss-2019b

export OMPI_MCA_mtl=^psm

export OMPI_MCA_mpi_warn_on_fork=0

mpirun -np 1 $EBROOTR/bin/R --slave CMD BATCH ./code/00-MASTER-RUN-CLUSTER.R