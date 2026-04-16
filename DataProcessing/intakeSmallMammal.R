# this script is for processing neon small mammal data
# download with neonstore
# clean data into usable csv 
# most identifiers kept, removing body condition stuff
# keeping records of animals found in traps, dead or alive
# removes individuals only identified to Order or Class
# marked individuals have same tag if tag was replaced


library(tidyverse)
library(lubridate)
library(uuid)

# NEONSTORE_HOME, NEONSTORE_DB, and NEON_TOKEN defined in .Renviron
Sys.setenv("NEONSTORE_HOME" = "/home/ebeez/Documents/TickForecast/Data/neonstore")
Sys.setenv("NEONSTORE_DB" = "/home/ebeez/Documents/TickForecast/Data/neonstore")
library(neonstore)

mouse_intake <- function(check.neon){
  
  if(check.neon){
    smam.product <- "DP1.10072.001"
    neon_download(product = smam.product)
  }
  
  # tables - can join by "nightuid"
  # plot.night <- neon_read("mam_perplotnight") # per trapping grid data
  trap.night <- neon_read("mam_pertrapnight") # per trap night data
  df.raw <- trap.night
  
  smam.data <- df.raw %>% 
    mutate(collectYear = year(collectDate),
           collectMonth = month(collectDate),
           collectYearMonth = paste(collectYear, collectMonth, sep = "-")) %>% 
    select(all_of(c("uid", # unique id
                    "nightuid", # night id, maps to plot.night
                    "collectDate",
                    "collectYear",
                    "collectMonth",
                    "collectYearMonth",
                    "domainID",
                    "siteID",
                    "plotID",
                    "nlcdClass", # land cover classification
                    "decimalLatitude",
                    "decimalLongitude",
                    "trapStatus", 
                    "trapCoordinate", # trap location on grid - A1
                    "tagID",
                    "taxonID", # four letter species/ID code??
                    "scientificName",
                    "taxonRank",
                    "identificationQualifier", # not sure
                    "recapture",
                    "fate",
                    "replacedTag",
                    "lifeStage",
                    "tickNumber",
                    "larvalTicksAttached",
                    "nymphalTicksAttached",
                    "adultTicksAttached"))) %>% 
    filter((!taxonRank %in% c("order", "class"))) %>% # get rid of high taxon classes
    mutate(animalInTrap = if_else(trapStatus == "4 - more than 1 capture in one trap" | 
                                  trapStatus == "5 - capture",
                                  1, 0), # 1 = alive individual, 0 = no capture
           animalInTrap = if_else(is.na(trapStatus), # very few records, but need the 0s in animalInTrap
                                  0, animalInTrap)) 
  
  # as of 2021-10-06 nightuid has an error (mostly at ABBY in 2017)
  # where each trap has it's own nightuid, instead of the same nightuid
  # for the plotID_collectDate combination that it should be
  # we can just create a new column for our own use
  smam.data <- smam.data %>% 
    group_by(plotID, collectDate) %>% 
    mutate(captureNightUID = UUIDgenerate()) %>% 
    ungroup() 
  
  # want the number of unique animals each day
  # and need to keep track of each individual
  # will go by unique tag
  # some tags have been replaced
  # the majority of tags replaced are just left or right
  # and the tag number is the same
  # Some have a new tag number, need to demarcate those
  # the tagID in the replacedTag column is the old tag
  
  # arrange by collect date within plots to make replacing easier
  smam.data <- smam.data %>% 
    arrange(plotID, collectDate) 
  
  tag.pattern <- "[[:upper:]]\\d{4}" # end of tagID - what is replaced; old tag
  new.tag.rows <- grep(tag.pattern, smam.data$replacedTag) # rows when new tags used
  old.tag.id <- smam.data$replacedTag[new.tag.rows] # old tag Ids
  new.tag.id <- smam.data$tagID[new.tag.rows] # new tags
  
  for(i in seq_along(new.tag.id)){
    
    smam.subset <- smam.data %>%
      filter(tagID == new.tag.id[i])
    
    if(nrow(smam.subset) > 0){ # revert back to old tag
      index <- which(smam.data$tagID == new.tag.id[i])
      smam.data$tagID[index] <- gsub(tag.pattern, old.tag.id[i], new.tag.id[i]) 
    }
  }
  
  # there are thousands of trap nights were 0 animals were captured
  # for capture history matrix we need every day, but do not need each empty trap
  
  # zero capture nights
  zero.captured <- smam.data %>% 
    group_by(captureNightUID) %>% 
    summarise(n = sum(animalInTrap)) %>% 
    ungroup() %>% 
    filter(n == 0) %>% 
    select(-n)
  
  # keep one record for each 0 night
  smam.0 <- smam.data %>% 
    filter(captureNightUID %in% zero.captured$captureNightUID) %>% 
    distinct(captureNightUID, .keep_all = TRUE)
  
  # mark dead animals 
  smam.data <- smam.data %>% 
    filter(trapStatus %in% c("4 - more than 1 capture in one trap", 
                             "5 - capture")) %>%  # keep only captured individuals
    mutate(fate = if_else(is.na(fate), "noFateRecorded", fate), # change NAs, needed for next mutate
           animalInTrap = if_else(fate == "dead", 
                                  2, animalInTrap)) %>%  # 2 = dead
    bind_rows(smam.0)
  
  
  smam.data <- smam.data %>% 
    separate(scientificName, into = c("genusName", "speciesName"), sep = "\\s", extra = "merge") %>% 
    arrange(siteID, plotID, collectDate) 
  
  write_csv(smam.data, "Data/allSmallMammals.csv")
}

mouse_intake(TRUE)
