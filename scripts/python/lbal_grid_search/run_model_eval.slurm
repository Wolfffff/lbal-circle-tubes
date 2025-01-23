#!/bin/bash
#BATCH --job-name=model_eval                            # Name of the job
#SBATCH --output=logs/model_eval_%j_%A.out              # STDOUT file
#SBATCH --error=logs/model_eval_%j_%A.err               # STDERR file
#SBATCH --nodes=1                                       # Node count
#SBATCH --ntasks=1                                      # Number of tasks across all nodes
#SBATCH --cpus-per-task=4                               # Cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=64G                               # total memory per node
#SBATCH --time=23:59:59                                 # Run time limit (HH:MM:SS)
#SBATCH --mail-type=all                                 # Email on job start, end, and fault
#SBATCH --mail-user=mw0425@princeton.edu

module load anaconda3/2022.10
conda init bash
conda activate sleap_dev

python model_evaluation.py

