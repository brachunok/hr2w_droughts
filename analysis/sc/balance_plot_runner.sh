#!/bin/bash

#SBATCH --job-name=plot_maker
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=8G


# now run the R portion
module load R/4.0.2

Rscript ./water_balance.R
