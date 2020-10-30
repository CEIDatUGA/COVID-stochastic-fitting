#!/bin/bash
#SBATCH --job-name=covstateshdr       # Job name
#SBATCH --partition=covid19_p         # Partition (queue) name
#SBATCH --ntasks=1                    # Run on a single CPU
#SBATCH --mem=3500M                  # Job memory request
#SBATCH --time=01:00:00               # Time limit hrs:min:sec
#SBATCH --output=covstateshdr.%j.out  # Standard output log
#SBATCH --error=covstateshdr.%j.err   # Standard error log

#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=emarty@uga.edu    # Where to send mail

cd $SLURM_SUBMIT_DIR

module load R/3.6.2-foss-2019b

rm -rf header
mkdir header

R CMD BATCH ./code/00-CREATE-HEADER.R


