#!/bin/bash

#SBATCH -J rich_zone
#SBATCH --time=16:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4

# Output files should ideally go to /work instead of /home
#SBATCH -o /work/%u/%x-%j.out

module load foss/2019b R/3.6.2-2
export OFILE=/work/$USER/Seed-Bank-Map/$SLURM_JOB_NAME-$SLURM_JOB_ID.Rdata
export LANG=en_US.UTF-8
Rscript --vanilla /home/ladouceu/projects/Seed-Bank-Map/code/cluster/rich_zone.R


