#############################################
# Model post-processing: scores             #
# Intermediate output to create figures     #
# Comparing models                          #
#                                           #
# E.M. Beasley                              #
# Fall 2026                                 #
#############################################

# Load packages -------------------------
library(tidyverse)
library(lubridate)

# Get file names -------------------------
# Define directories and get raw files
dir.out <- "./out/"
dir.analysis <- "./analysis/"

analysis.files <- list.files(dir.analysis, recursive = T)

# Prune file list to only get files with scores
score.files <- analysis.files[!str_detect(analysis.files, "allDays")]
score.files <- score.files[!str_detect(score.files, "Weather.csv")]
score.files <- score.files[!str_detect(score.files, "Score")]

for(i in 14:length(score.files)){
  score <- read_csv(file=file.path(dir.analysis, score.files[i])) %>%
    suppressMessages()
  
  if(nrow(score)==0){next}
  
  score <- score %>%
    filter(year(time) >= 2018 & year(time) <= 2022) %>%
    select(lifeStage, time, siteID, species, model, crps) %>%
    mutate(species = str_replace(species, " ", "_")) %>%
    group_by(lifeStage, time, siteID, species, model) %>%
    summarise(crps = mean(crps)) %>%
    suppressMessages()
  
  write_csv(score, file = paste0(dir.analysis, "Score", paste(unique(score$siteID), 
                                                     unique(score$species), 
                                    unique(score$model), sep = "_"), ".csv"))
  rm(score)
  
  print(i/length(score.files))
}


# Clean data and save output ------------------