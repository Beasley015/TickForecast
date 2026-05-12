library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)
library(utils)
library(ggpubr)
library(MetBrewer)



# Forecasts for all days -------------
dir.top <- "/projectnb/dietzelab/ebeasley/TickForecast/outUpdate"
dir.out <- "/usr4/ugrad/neochatt/TickForecast/Data"
dir.analysis <-  file.path(dir.top, "analysis")
if(!dir.exists(dir.analysis)) dir.create(dir.analysis, recursive = TRUE, showWarnings = FALSE)

out.files <- list.files(dir.top, recursive = TRUE)
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

sites <- c("HARV", "KONZ", "LENO", "OSBS", "SCBI", "SERC", "TALL", "TREE", "UKFS")

for(j in 1:length(sites)){
  # Blank df for each site
  df.process <- tibble()
  
  # extract files for a particular site
  quantScore <- grep(jobs$sites[j], process.samples, value = T)
  
  for(i in seq_along(quantScore)){
    # Extract constants
    st <- str_extract(quantScore[i], "\\d{4}-\\d{2}-\\d{2}")
    spp <- find_species(quantScore[i])
    m <- find_model(quantScore[i])
    site <- sites[j]
    
    # Skip pre-2018 outputs- essentially a burn-in period
    if(year(as.Date(st, format = "%Y-%m-%d")) < 2018){next}
    
    # Load file and clean up
    dfi <- read_csv(file.path(dir.top, quantScore[i])) %>% 
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
  
  if(nrow(df.process) == 0){
    message("No data for site: ", sites[j], " — skipping.")
    next
  }
  
  df.process <- df.process %>% 
    mutate(mice = if_else(grepl("Mice", model), "Mice", "No mice"),
           weather = if_else(grepl("Weather", model), "Weather", "No weather"))
  
  write_csv(df.process, file = file.path(dir.out, paste(site, "allDays.csv", sep = "_")))
  
  print(paste(sites[j], "Complete", sep = " "))
}

# Process model params -------------------

# File names for params
out.files <- list.files(dir.top, recursive = TRUE)
quantScore <- grep("fxQuantScore.csv", out.files, value = TRUE)
params <- grep("parameterSummary.csv", out.files, value = TRUE)

quantScore <- as.character(quantScore)  

models <- c("WithWeatherAndMiceGlobal")
species <- c("Ixodesscapularis", "Amblyommaamericanum")

neon.sites <- c("HARV", "KONZ", "LENO", "OSBS", "SCBI", "SERC", "TALL", "TREE", "UKFS"
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
  site = c(neon.sites)
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
  param.files <- params[vapply(params, function(f) all(vapply(strings, grepl, logical(1), x = f)), logical(1))]
  
  
  # empty tibble
  df.process <- tibble()
  
  for(i in seq_along(param.files)){
    dfi <- read_csv(file.path(dir.top, param.files[i])) %>%
      mutate(start.date = str_extract(param.files[i], "\\d{4}-\\d{2}-\\d{2}")) %>%
      suppressMessages()
    
    df.process <- bind_rows(df.process, dfi)
    if(i %% 10 == 0) message(i, " of ", length(param.files), " complete ", round(i/length(param.files)*100), "%")
  }
  
  if(nrow(df.process) == 0){
    message("No data for site: ", sites[j], " — skipping.")
    next
  }
  
  df.process <- df.process %>% 
    mutate(mice = if_else(grepl("Mice", jobs$model[j]), "Mice", "No mice"),
           weather = if_else(grepl("Weather", jobs$model[j]), "Weather", "No weather"))
  
  
  write_csv(
    df.process,
    file = paste0(dir.out, "/", jobs$site[j], jobs$species[j], jobs$model[j], "_parameterSummary.csv")
  )
  
  
  
  print(paste("Job = ", j))
  
  rm(df.process)
}