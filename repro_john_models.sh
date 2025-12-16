#!/bin/bash -l

# Set SCC project
#$ -P dietzelab

# Request buyin nodes so Mike doesn't kill you
#$ -l buyin

# Specify array job with tasks
#$ -t 1-34

# Specify hard time limit of hours for the job (if you have a short runtime the SCC gives you priority)
#$ -l h_rt=60:00:00

# Assign cores and cores per node
#$ -pe omp 3 -l mem_per_core=16G # This assigns 3 cores per task; 1 core per markov chain

# Send an email when the job finishes or if it is aborted 
#$ -m ea
#
#
# Below is what would get passed to the command line - you can test just these lines to make sure they work

cd /projectnb/dietzelab/ebeasley/TickForecast

module load R/4.4.0

Rscript /projectnb/dietzelab/ebeasley/TickForecast/R/workflow_process_models.R $SGE_TASK_ID
