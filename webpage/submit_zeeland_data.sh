#!/bin/bash
#-----------------------------Mail address-----------------------------
#SBATCH --mail-user=rutger.dankers@wur.nl
#SBATCH --mail-type=ALL
#-----------------------------Output files-----------------------------
#SBATCH --output=output_%j.txt
#SBATCH --error=error_output_%j.txt
#-----------------------------Other information------------------------
#SBATCH --comment=5200044888
#SBATCH --job-name=Zeeland_data.R
#-----------------------------Required resources-----------------------
#SBATCH --qos=low
#SBATCH --time=5
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4000
#SBATCH --begin=17:00

#-----------------------------Environment, Operations and Job steps----
#load modules
module load R/3.5.3

#export variables

#your job
srun Rscript /home/WUR/danke010/mycode/zeeland_hittestress/webpage/Zeeland_data.R >& /lustre/scratch/WUR/ESG/danke010/Zeeland/latest_run.Rout

# resubmit the job for the next execution
sbatch $0

# sbatch /home/WUR/danke010/mycode/zeeland_hittestress/webpage/submit_zeeland_data.sh
