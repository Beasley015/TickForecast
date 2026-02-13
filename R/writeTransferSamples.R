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




