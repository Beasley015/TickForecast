library(tidyverse)
library(lubridate)
library(daymetr)
library(curl)


site.coord <- readr::read_csv("./Data/siteLatLon.csv") %>% suppressMessages()

mouse.data <- read_csv("./Data/allSmallMammals.csv")
mouse.plots <- mouse.data %>%
  group_by(plotID) %>%
  select(plotID, decimalLatitude, decimalLongitude) %>%
  distinct() %>%
  mutate(plotID = paste0("smam", plotID))

tick.data <- read_csv("./Data/tickLong.csv")
tick.plots <- tick.data %>%
  group_by(plotID) %>%
  select(plotID, decimalLatitude, decimalLongitude) %>%
  distinct() %>%
  mutate(plotID = paste0("tick", plotID))

plot.df <- bind_rows(mouse.plots, tick.plots)
plot.df <- plot.df %>%
  filter(grepl("HARV", site),
         grepl("tick", site)) %>% 
  rename_with( ~ c("site", "latitude", "longitude"), everything())
write_csv(plot.df, path = "./Data/plotLatLon.csv")

dm <- download_daymet_batch(
  file_location = './Data/plotLatLon.csv',
  start = 2020,
  end = 2021,
  internal = TRUE
)

write_csv(dm, path = "./Data/daymet.csv")

variables <- c(
  "dayl..s.",      # day length
  "prcp..mm.day.", # precipitation
  "srad..W.m.2.",  # shortwave radiation
  "swe..kg.m.2.",  # snow-water equivalent
  "tmax..deg.c.",  # maximum temperature
  "tmin..deg.c.",  # minimum temperature
  "vp..Pa."        # vapor pressure
)

variable.name <- c(
  "dayLength",
  "precipitation",
  "shortwaveRadiation",
  "snowWaterEquivalent",
  "maxTemperature",
  "minTemperature",
  "vaporPressure"
)

make_csvs <- function(org){
  
  if(org == "mice"){
    data <- read_csv("./Data/daymetSite.csv")  
    size <- "Site"
  } else if (org == "tick"){
    data <- read_csv("./Data/daymet.csv")
    size <- "Plot"
  }
  
  message(org)
  
  df <- data %>% 
    mutate(Date = as.Date(yday-1, 
                          origin = paste0(year, "-01-01")))
  
  for(s in seq_along(variables)){
    message(paste("  ", variables[s]))
    
    df.save <- df %>% 
      filter(measurement == variables[s]) %>% 
      select(-measurement) 
    
    for(i in seq_along(unique(df.save$site))){
      # message(paste("     ", unique(df.save$site)[i]))
      
      leap.df <- df.save %>% 
        filter(site == unique(df.save$site)[i], 
               Date == "2016-12-30" | Date == "2017-01-01")
      
      leap.value <- leap.df %>% 
        summarise(value = mean(value)) %>% 
        pull(value)
      
      leap.df <- leap.df %>%   
        add_row(value = leap.value,
                Date = ymd("2016-12-31"),
                site = leap.df$site[1],
                tile = leap.df$tile[1],
                latitude = leap.df$latitude[1],
                longitude = leap.df$longitude[1],
                altitude = leap.df$altitude[1],
                year = leap.df$year[1],
                yday = 366) %>% 
        filter(yday == 366)
      
      df.save <- bind_rows(df.save, leap.df)
    }
    
    if(org == "mice"){
      df.save <- df.save %>% 
        rename_at(vars(value), ~ variable.name[s])  %>% 
        rename("siteID" = site) %>% 
        arrange(siteID, Date)
    }
    
    if(org == "tick"){
      df.save <- df.save %>% 
        separate(site, c("data", "plotID"), sep = 4) %>%
        filter(plotID != "ORNL_006") %>% 
        rename_at(vars(value), ~ variable.name[s])  %>% 
        arrange(plotID, Date)
    }
    
    
    write_csv(df.save, 
              path = file.path("./Data", paste0("daymet", size, "_", variable.name[s], ".csv")))
  }  
}

message("-- Mouse Sites --")
make_csvs("mice")

message("-- Tick Plots --")
make_csvs("tick")

message("-- DONE --")



