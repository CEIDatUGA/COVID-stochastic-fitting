#PBS -S /bin/bash
#PBS -q covid19_q
#PBS -N covstateshdr
#PBS -l nodes=1:ppn=1
#PBS -l walltime=1:00:00
#PBS -l pmem=3500mb
#PBS -M atredennick@west-inc.com

cd $PBS_O_WORKDIR

module load R/3.6.2-foss-2018a-X11-20180131-GACRC

rm -rf header
mkdir header

R CMD BATCH ./code/00-CREATE-HEADER.R