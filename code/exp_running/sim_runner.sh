#!/bin/bash

#SBATCH --job-name=sim_runner
#SBATCH --cpus-per-task=5
#SBATCH --mem-per-cpu=2G

module load python/3.9.0
module load py-pandas
python3 sim.py

# now run the R portion
module load R/4.0.2

Rscript ../../analysis/sc/process_outputs.R
