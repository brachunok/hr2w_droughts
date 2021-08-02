#!/bin/bash

#SBATCH --job-name=sim_runner
#
#SBATCH --cpus-per-task=10
#SBATCH --mem-per-cpu=1G



module load python/3.6.1
module load py-pandas
python3 sim.py

# now run the R portion
module load R/3.5.1

Rscript ../../analysis/sc/process_outputs.R
