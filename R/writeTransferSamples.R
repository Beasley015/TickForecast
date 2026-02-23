library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)
library(utils)
library(ggpubr)
library(MetBrewer)

# Forecasts for all days -------------
dir.top <- getwd()
dir.out <- file.path(dir.top, "out")
dir.analysis <-  file.path(dir.top, "analysis")
if(!dir.exists(dir.analysis)) dir.create(dir.analysis, recursive = TRUE, showWarnings = FALSE)

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
  m
}

find_species <- function(x){
  species <- if_else(grepl("Amblyommaamericanum", x),
                     "Amblyomma americanum",
                     "Ixodes scapularis")
  species
}

sites <- c("BLAN","HARV","KONZ","LENO","OSBS","SCBI","SERC","TALL","TREE","UKFS",
           "GREN","HNRY","TEA")

for(j in 1:length(sites)){
  # Blank df for each site
  df.process <- tibble()
  
  # extract files for a particular site
  quantScore <- grep(sites[j], process.samples, value = T)
  
  for(i in seq_along(quantScore)){
    dfi <- read_csv(file.path(dir.out, quantScore[i])) %>% 
      suppressMessages()
    
    st <- str_extract(quantScore[i], "\\d{4}-\\d{2}-\\d{2}")
    spp <- find_species(quantScore[i])
    m <- find_model(quantScore[i])
    site <- sites[j]
    
    if(year(as.Date(st, format = "%Y-%m-%d")) < 2018){next}
  
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

models <- c("Weather", "WithWeatherAndMiceGlobal")
species <- c("Ixodesscapularis", "Amblyommaamericanum")
neon.sites <- c(
  "BLAN",
  "HARV",
  "KONZ",
  "LENO",
  "OSBS",
  "SCBI",
  "SERC",
  "TALL",
  "TREE",
  "UKFS"
)

cary.sites <- c(
  "GREN",
  "HNRY",
  "TEA"
)

# Create all possible combos
jobs <- expand_grid(
  model = models,
  species = species,
  site = c(neon.sites, cary.sites)
)

# Not all sites have both tick species
jobs <- jobs %>%
  filter(
    !(site == "HARV" & species == "Amblyommaamericanum"),
    !(site == "TREE" & species == "Amblyommaamericanum"),
    !(site == "KONZ" & species == "Ixodesscapularis"),
    !(site == "OSBS" & species == "Ixodesscapularis"),
    !(site == "TALL" & species == "Ixodesscapularis"),
    !(site == "UKFS" & species == "Ixodesscapularis"),
    !(site == "GREN" & species == "Amblyommaamericanum"),,
    !(site == "HNRY" & species == "Amblyommaamericanum"),,
    !(site == "TEA" & species == "Amblyommaamericanum"),
  )

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

  write_csv(df.process, file=paste(dir.analysis, as.character(jobs[j,3]), as.character(jobs[j,2]),
                                   as.character(jobs[j,1]), ".csv", sep = ""))

  print(paste("Job = ", j))

  rm(df.process)
}


