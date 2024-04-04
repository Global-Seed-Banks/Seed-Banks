#!/bin/bash

#SBATCH -J rich_trop
#SBATCH --time=60:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4

# Output files should ideally go to /work instead of /home
#SBATCH -o /work/%u/%x-%j.out

module load foss/2022b R/4.2.2
export OFILE=/work/$USER/Seed-Banks/$SLURM_JOB_NAME-$SLURM_JOB_ID.Rdata
export LANG=en_US.UTF-8
Rscript --vanilla /home/ladouceu/projects/Seed-Banks/code/cluster/rich_trop.R


