library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)
library(utils)
library(ggpubr)
library(MetBrewer)

# Load folders
dir.top <- getwd()
dir.out <- file.path(dir.top, "out")
dir.analysis <-  file.path(dir.top, "analysis")
if(!dir.exists(dir.analysis)) dir.create(dir.analysis, 
                                         recursive = TRUE, showWarnings = FALSE)

# Forecasts for all days -------------
out.files <- list.files(dir.out, recursive = TRUE)
process.samples <- grep("stateSamples.csv", out.files, value = TRUE)
rm(out.files)

find_model <- function(x){
  if(grepl("Weather", x)){
    m <- "Weather"
  } 
  if(grepl("WeatherAndMice", x)){
    m <- "Mice & Weather"
  } 
  m
}

find_hierarchy <- function(x){
  if(grepl("Full", x)){
    h <- "FullHierarchical"
  } else if(grepl("Intercept",x)){
    h <- "HierarchicalIntercept"
  }
}

species <- c("Ixodes_scapularis", "Amblyomma_americanum")

for(j in 1:length(species)){
  # Blank df
  df.process <- tibble()
  
  # extract files for a particular species
  quantScore <- grep(species[j], process.samples, value = T)
  quantScore <- quantScore[!str_detect(quantScore, "Main")]
  
  for(i in seq_along(quantScore)){
    # Extract constants
    st <- str_extract(quantScore[i], "\\d{4}-\\d{2}-\\d{2}")
    spp <- species[j]
    m <- find_model(quantScore[i])
    h <- find_hierarchy(quantScore[i])
    
    # Skip pre-2018 outputs- essentially a burn-in period
    if(year(as.Date(st, format = "%Y-%m-%d")) < 2018){next}
    
    # Load file and clean up
    dfi <- read_csv(file.path(dir.out, quantScore[i])) %>% 
      suppressMessages()
    
    df.summary <- dfi %>% 
      pivot_longer(cols = Larva:Adult, names_to = "lifeStage") %>%
      group_by(time, lifeStage, siteID) %>%
      summarise(lower95 = quantile(value, 0.025),
                lower75 = quantile(value, 0.125),
                median = median(value),
                mean = mean(value),
                upper75 = quantile(value, 0.875),
                upper95 = quantile(value, 0.975), 
                variance = var(value)) %>% 
      mutate(model = m, species = spp, start.date = st, hierarchy=h) %>% 
    ungroup() %>% 
    suppressMessages()
  
   df.process <- bind_rows(df.process, df.summary)
  
    if(i %% 10 == 0) message(i, " of ", length(quantScore), " complete ", round(i/length(quantScore)*100), "%")
  }

  write_csv(df.process, file = file.path(dir.analysis, paste(species[j], "allDays.csv", sep = "_")))
  
  print(paste(species[j], "Complete", sep = " "))
}

# Process model quant scores -------------------

# File names for quant scores
out.files <- list.files(dir.out, recursive = TRUE)
quantScore <- grep("fxQuantScore.csv", out.files, value = TRUE)

models <- c("Weather_hierarchicalIntercept", "WeatherMice_hierarchicalIntercept",
            "Weather_hierarchicalFull", "WeatherMice_hierarchicalFull")
species <- c("Ixodes_scapularis", "Amblyomma_americanum")

# Create all possible combos
jobs <- expand_grid(
  model = models,
  species = species
)

# Process outputs
for(j in 1:nrow(jobs)){
  # Get subset of jobs
  strings <- c(as.character(jobs[j,1]),as.character(jobs[j,2]))
  string.check <- sapply(quantScore, str_detect, strings)
  quant.files <- quantScore[which(colSums(string.check)==2)]

  # empty tibble
  df.process <- tibble()

  for(i in seq_along(quant.files)){
      dfi <- read_csv(file.path(dir.out, quant.files[i])) %>%
        mutate(start.date = str_extract(quant.files[i], "\\d{4}-\\d{2}-\\d{2}")) %>%
        filter(lifeStage=="Nymph") %>%
        dplyr::select(-c(nlcd, percentBias, rmse, bayesP)) %>%
        suppressMessages()

      df.process <- bind_rows(df.process, dfi)
      if(i %% 10 == 0) message(i, " of ", length(quant.files), " complete ", round(i/length(quant.files)*100), "%")
  }

  write_csv(df.process, file=paste(dir.analysis, "/", as.character(jobs[j,2]),
                        as.character(jobs[j,1]), ".csv", sep = ""))

  print(paste("Job = ", j))

  rm(df.process)
}
