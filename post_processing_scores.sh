#!/bin/bash -l

# Set SCC project
#$ -P dietzelab

# Request buyin nodes so Mike doesn't kill you
#$ -l buyin

# Specify array job with tasks

# Specify hard time limit of hours for the job (if you have a short runtime the SCC gives you priority)
#$ -l h_rt=72:00:00

# Assign cores and cores per node
#$ -pe omp 16 # This assigns a full node (16 cores) for a total of 128G of RAM

# Send an email when the job finishes or if it is aborted 
#$ -m ea
#
#
# Below is what would get passed to the command line - you can test just these lines to make sure they work

cd /projectnb/dietzelab/ebeasley/TickForecast

module load R/4.4.0

Rscript /projectnb/dietzelab/ebeasley/TickForecast/R/ScoresProcessing.R
