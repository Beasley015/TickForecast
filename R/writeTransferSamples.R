library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)
library(utils)
library(ggpubr)
library(MetBrewer)

dir.top <- "/projectnb/dietzelab/fosterj"
dir.out <- file.path(dir.top, "FinalOut/Chapter3/outConstraintForestUpdate")
dir.analysis <-  file.path(dir.top, "FinalOut/Chapter3/analysisConstraintForestUpdate")
if(!dir.exists(dir.analysis)) dir.create(dir.analysis, recursive = TRUE, showWarnings = FALSE)


jobs <- c(
  "/ic/",
  "/ic_parameter/",
  "/ic_parameter_driver/",
  "/ic_parameter_driver_process/"  
)

job.num <- as.numeric(Sys.getenv("SGE_TASK_ID"))
if(is.na(job.num)) job.num <- 3

out.files <- list.files(dir.out, recursive = TRUE)
process.samples <- grep("stateSamples.csv", out.files, value = TRUE)
quantScore <- grep(jobs[job.num], process.samples, value = TRUE) 


jobnames <- c(
  "ic",
  "ic_parameter",
  "ic_parameter_driver",
  "ic_parameter_driver_process"  
)

job.sub <- jobnames[job.num]

find_ua <- function(x){
  if(grepl("ic", x)){
    ua <- "IC"
  } 
  if(grepl("ic_parameter", x)){
    ua <- "+ Parameter"
  } 
  if(grepl("ic_parameter_driver", x)){
    ua <- "+ Driver"
  } 
  if(grepl("ic_parameter_driver_process", x)){
    ua <- "+ Process"
  }
  ua
}

find_model <- function(x){
  if(grepl("Static", x)){
    m <- "Static"
  }
  if(grepl("WithMNAMice", x)){
    m <- "Mice"
  } 
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

df.process <- tibble()
for(i in seq_along(quantScore)){
  dfi <- read_csv(file.path(dir.out, quantScore[i])) %>% 
    suppressMessages()
  st <- str_extract(quantScore[i], "\\d{4}-\\d{2}-\\d{2}")
  spp <- find_species(quantScore[i])
  m <- find_model(quantScore[i])
  u <- find_ua(quantScore[i])
  
  df.summary <- dfi %>% 
    # group_by(node, ua, model, siteID) %>% 
    group_by(time, lifeStage, siteID) %>%
    summarise(lower95 = quantile(value, 0.025),
              lower75 = quantile(value, 0.125),
              median = median(value),
              mean = mean(value),
              upper75 = quantile(value, 0.875),
              upper95 = quantile(value, 0.975), 
              variance = var(value)) %>% 
    mutate(model = m,
           ua = u,
           species = spp,
           start.date = st) %>% 
    ungroup() %>% 
    suppressMessages()
  
  df.process <- bind_rows(df.process, df.summary)
  
  if(i %% 100 == 0) message(i, " of ", length(quantScore), " complete ", round(i/length(quantScore)*100), "%")
}

df.mutate <- df.process %>% 
  mutate(mice = if_else(grepl("Mice", model), "Mice", "No mice"),
         weather = if_else(grepl("Weather", model), "Weather", "No weather"))



write_csv(df.mutate, file = file.path(dir.analysis, paste0(job.sub, "_allDaysQuants.csv")))




