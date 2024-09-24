#!/bin/bash
#BATCH --job-name=bat_int_que_wor                       # Name of the job
#SBATCH --output=logs/bat_int_que_wor_%j.out            # STDOUT file
#SBATCH --error=logs/bat_int_que_wor_%j.err             # STDERR file
#SBATCH --nodes=1                                       # Node count
#SBATCH --ntasks=1                                      # Number of tasks across all nodes
#SBATCH --cpus-per-task=8                               # Cores per Task (>1 if Multithreaded)
#SBATCH --mem=64G                                       # total memory per node
#SBATCH --array=1-24                                    # Number of jobs % Max number of jobs to consume
#SBATCH --time=01:00:00                                 # Run time limit (HH:MM:SS)
#SBATCH --mail-type=all                                 # Email on job start, end, and fault
#SBATCH --mail-user=mw0425@princeton.edu

module load anaconda3/2022.10
source activate
conda activate naps_dev

FILE=$(sed "${SLURM_ARRAY_TASK_ID}q;d" list_of_files_queen_worker.txt)

python naps_interactions.py --sleap-file ../naps_track_round2/${FILE}_naps_tracked.slp --yaml-model naps.yaml --out-file ${FILE}_queen_worker.txt

