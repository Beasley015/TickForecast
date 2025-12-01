#!/bin/bash -l

# Set SCC project
#$ -P dietzelab

# Request buyin nodes so Mike doesn't kill you
#$ -l buyin

# Specify array job with 5 tasks numbered 1-5
#$ -t 1-5

# Specify hard time limit of hours for the job (if you have a short runtime the SCC gives you priority)
#$ -l h_rt=5:00:00

# Assign cores and cores per node
#$ -pe omp 1 -l mem_per_core=16G

# Send an email when the job finishes or if it is aborted (by default no email is sent).<- might not be right
#$ -m ea

# Merge stderr into the stdout log file, to reduce clutter
#$ -j y

# Give the log file a name 
#$ -o /projectnb/dietzelab/ebeasley/repro_john_models.log

#
#
# Below is what would get passed to the command line - you can test just these lines to make sure they work

cd /projectnb/dietzelab/ebeasley/TickForecast

module load R/4.2.1

Rscript /projectnb/dietzelab/ebeasley/TickForecast/R/workflow_process_models.R $SGE_TASK_ID
