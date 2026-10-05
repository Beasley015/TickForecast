library(mclm)
library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)
library(utils)
library(ggpubr)
library(MetBrewer)

dir.top <- getwd()
dir.out <- file.path(dir.top, "out")
dir.analysis <-  file.path(dir.top, "analysis")
if(!dir.exists(dir.analysis)) dir.create(dir.analysis, recursive = TRUE, showWarnings = FALSE)

# Forecasts for all days -------------
out.files <- list.files(dir.out, recursive = TRUE)
process.samples <- grep("stateSamples.csv", out.files, value = TRUE)
rm(out.files)

find_model <- function(x){
  if(grepl("Weather", x)){
    m <- "Weather"
  } 
  if(grepl("WithWeatherAndMiceGlobal", x)){
    m <- "Mice & Weather"
  } 
  if(str_detect(x, "PlotLevel")){
    m <- "PlotLevel"
  }
  m
}

find_species <- function(x){
  species <- if_else(grepl("Amblyommaamericanum", x),
                     "Amblyomma americanum",
                     "Ixodes scapularis")
  species
}

iscap.sites <- str_remove(read_txt("./Data/ix_sites_single.txt"), "\\*")
ambly.sites <- str_remove(read_txt("./Data/am_sites_single.txt"), "\\*")
sites <- unique(c(iscap.sites, ambly.sites))
sites <- sites[sites != "DELA"]

for(j in 1:length(sites)){ 
  # Blank df for each site
  df.process <- tibble()
  
  # extract files for a particular site
  quantScore <- grep(sites[j], process.samples, value = T)
  
  for(i in seq_along(quantScore)){
    # Extract constants
    st <- str_extract(quantScore[i], "\\d{4}-\\d{2}-\\d{2}")
    spp <- find_species(quantScore[i])
    m <- find_model(quantScore[i])
    site <- sites[j]
    
    # Skip pre-2018 outputs- essentially a burn-in period
    if(year(as.Date(st, format = "%Y-%m-%d")) < 2018){next}
    
    # Load file and clean up
    dfi <- read_csv(file.path(dir.out, quantScore[i])) %>% 
      suppressMessages()
    
    df.summary <- dfi %>% 
      group_by(time, lifeStage, siteID) %>%
      summarise(lower95 = quantile(value, 0.025),
                lower75 = quantile(value, 0.125),
                median = median(value),
                mean = mean(value),
                upper75 = quantile(value, 0.875),
                upper95 = quantile(value, 0.975), 
                variance = var(value)) %>% 
      mutate(model = m,species = spp, start.date = st) %>% 
    ungroup() %>% 
    suppressMessages()
  
   df.process <- bind_rows(df.process, df.summary)
  
    if(i %% 10 == 0) message(i, " of ", length(quantScore), " complete ", round(i/length(quantScore)*100), "%")
  }
  
  df.process <- df.process %>% 
    mutate(mice = if_else(grepl("Mice", model), "Mice", "No mice"),
           weather = if_else(grepl("Weather", model), "Weather", "No weather"))
  
  write_csv(df.process, file = file.path(dir.analysis, paste(site, "allDays.csv", sep = "_")))
  
  print(paste(sites[j], "Complete", sep = " "))
}

# Process model quant scores -------------------

# File names for quant scores
out.files <- list.files(dir.out, recursive = TRUE)
quantScore <- grep("fxQuantScore.csv", out.files, value = TRUE)

# models <- c("Weather", "WithWeatherAndMiceGlobal", "PlotLevel")
models <- "PlotLevel"
species <- c("Ixodes_scapularis", "Amblyomma_americanum")

iscap.sites <- str_remove(read_txt("./Data/ix_sites_single.txt"), "\\*")
ambly.sites <- str_remove(read_txt("./Data/am_sites_single.txt"), "\\*")
sites <- unique(c(iscap.sites, ambly.sites))
sites <- sites[sites != "DELA"]

iscap.sites <- str_remove(read_txt("./Data/ix_sites_single.txt"), "\\*")
ambly.sites <- str_remove(read_txt("./Data/am_sites_single.txt"), "\\*")

# Create all possible combos
iscap.jobs <- data.frame(site = iscap.sites, species = "Ixodes_scapularis")
ambly.jobs <- data.frame(site = ambly.sites, species = "Amblyomma_americanum")

jobs <- bind_rows(iscap.jobs, ambly.jobs) %>%
  mutate(model = models) %>%
  filter(site != "DELA")

# Process outputs
for(j in 1:nrow(jobs)){
  # Get subset of jobs
  strings <- c(as.character(jobs[j,1]),as.character(jobs[j,2]), as.character(jobs[j,3]))
  string.check <- sapply(quantScore, str_detect, strings)
  quant.files <- quantScore[which(colSums(string.check)==3)]

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

  df.process <- df.process %>%
    mutate(mice = if_else(grepl("Mice", jobs$model[j]), "Mice", "No mice"),
           weather = if_else(grepl("Weather", jobs$model[j]), "Weather", "No weather"))

  write_csv(df.process, file=paste(dir.analysis, "/", as.character(jobs[j,3]), as.character(jobs[j,2]),
                                   as.character(jobs[j,1]), ".csv", sep = ""))

  print(paste("Job = ", j))

  rm(df.process)
}
