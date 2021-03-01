#!/bin/bash
#SBATCH --job-name=covstates         # Job name
#SBATCH --partition=covid19_p         # Partition (queue) name
#SBATCH --ntasks=1                    # Run on a single CPU
#SBATCH --nodes=1
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=3500M          # Job memory request
#SBATCH --time=3-0:00:00            # Time limit days-hrs:min:sec
#SBATCH --array=1-50
#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=emarty@uga.edu    # Where to send mail

cd $SLURM_SUBMIT_DIR

module load R/3.6.2-foss-2019b

mkdir ${SLURM_JOB_ID}
cd ${SLURM_JOB_ID}


R CMD BATCH "--args a=$SLURM_ARRAY_TASK_ID" ../code/00-RUN-CLUSTER-ARRAY.R
