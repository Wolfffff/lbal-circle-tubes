#!/bin/bash
#BATCH --job-name=bat_deg_que_que                       # Name of the job
#SBATCH --output=logs/bat_deg_que_que_%j_%A.out         # STDOUT file
#SBATCH --error=logs/bat_deg_que_que_%j_%A.err          # STDERR file
#SBATCH --nodes=1                                       # Node count
#SBATCH --ntasks=1                                      # Number of tasks across all nodes
#SBATCH --cpus-per-task=1                               # Cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=10G                                       # total memory per node
#SBATCH --array=1-13                                    # Number of jobs % Max number of jobs to consume
#SBATCH --time=0:59:59                                  # Run time limit (HH:MM:SS)
#SBATCH --mail-type=all                                 # Email on job start, end, and fault
#SBATCH --mail-user=mw0425@princeton.edu

module load anaconda3/2022.10
source activate

FILE=$(sed "${SLURM_ARRAY_TASK_ID}q;d" list_of_files_queen_queen.txt)

python analyze_network.py ../interactions/${FILE}_queen_queen.txt ${FILE}_queen_queen_hth_bouts.csv Head_to_Head True
python analyze_network.py ../interactions/${FILE}_queen_queen.txt ${FILE}_queen_queen_htb_bouts.csv Head_to_Body True
