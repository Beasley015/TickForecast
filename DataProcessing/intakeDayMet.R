# Load packages
library(tidyverse)
library(lubridate)
library(daymetr)
library(curl)

# Read in site coords if they already exist
site.coord <- readr::read_csv("./Data/siteLatLon.csv") %>% suppressMessages()

# Read in mouse data (plots)
mouse.data <- read_csv("./Data/allSmallMammals.csv")
mouse.plots <- mouse.data %>%
  group_by(plotID) %>%
  select(plotID, decimalLatitude, decimalLongitude) %>%
  distinct() %>%
  mutate(plotID = paste0("smam", plotID))

# Read in tick data (plots)
tick.data <- read_csv("./Data/tickLong.csv", show_col_types=F)
tick.plots <- tick.data %>%
  group_by(plotID) %>%
  select(plotID, decimalLatitude, decimalLongitude) %>%
  distinct() %>%
  mutate(plotID = paste0("tick", plotID))

# Write csv for plot coords
plot.df <- bind_rows(mouse.plots, tick.plots)
plot.df <- plot.df %>%
  filter(!grepl("tick", plotID)) %>%
  rename_with( ~ c("site", "latitude", "longitude"), everything())
write_csv(plot.df, file = "./Data/plotLatLon.csv")

# Daymet download
dm <- download_daymet_batch(
  file_location = './Data/siteLatLon.csv',
  # file_location = './Data/plotLatLon.csv', #uncomment for plot level
  start = 2016,
  end = 2021,
  internal = TRUE
)

dm_unlist <- function(x){
  dat <- x$data
  dat$site <- x$site
  dat$lat <- x$latitude
  dat$long <- x$longitude
  dat$alt <- x$altitude

  return(dat)
}

dm_tst <- lapply(dm, dm_unlist)

dm_df <- do.call(rbind, dm_tst)

write_csv(dm_df, file = "./Data/daymetSite.csv")

variables <- c(
  "dayl..s.",      # day length
  "prcp..mm.day.", # precipitation
  #"srad..W.m.2.",  # shortwave radiation
  #"swe..kg.m.2.",  # snow-water equivalent
  "tmax..deg.c.",  # maximum temperature
  "tmin..deg.c.",  # minimum temperature
  "vp..Pa."        # vapor pressure
)

variable.name <- c(
  "dayLength",
  "precipitation",
  #"shortwaveRadiation",
  #"snowWaterEquivalent",
  "maxTemperature",
  "minTemperature",
  "vaporPressure"
)

make_csvs <- function(size){
  
  if(size == "Site"){
    data <- read_csv("./Data/daymetSite.csv") 
  } else if (size == "Plot"){
    data <- read_csv("./Data/daymet.csv")
  }
  
  df <- data %>% 
    mutate(Date = as.Date(yday-1, 
                          origin = paste0(year, "-01-01")))
  
  for(s in seq_along(variables)){
    message(paste("  ", variables[s]))
    
    measurement <- variables[s]
    
    df.save <- df #%>% 
      # select(-all_of(measurement)) 
    
    for(i in seq_along(unique(df.save$site))){
      # message(paste("     ", unique(df.save$site)[i]))
      leap.df <- df.save %>%
        filter(site == unique(df.save$site)[i],
               Date %in% c("2016-12-30", "2017-01-01",
                           "2020-12-30", "2021-01-01")) # leap years?
    
      leap.value <- leap.df %>%
        mutate(group = case_when(year %in% c(2016, 2017) ~ 1,
                                 TRUE ~ 2)) %>%
        group_by(group) %>%
        summarise_at(vars(variables[s]), mean) %>%
        select(-group)
        
      leap.row <- data.frame(as.vector(leap.value[,1]), 
                             c(ymd("2016-12-31"),ymd("2020-12-31")),
                             leap.df$site[1],
                    # leap.df$latitude[1], leap.df$longitude[1],
                    # leap.df$altitude[1],
                    c(2016,2020), 366)
      
      colnames(leap.row) <- c(variables[s], 'Date', 'site',
                           # 'latitude', 'longitude', 'altitude',
                           'year', 'yday')

      leap.df <- leap.df %>%
        add_row(!!! leap.row) %>%
        filter(yday == 366)

      df.save <- bind_rows(df.save, leap.df)
    }
    
    if(size == "Site"){
      df.save <- df.save %>% 
        rename_at(variables[s], ~ variable.name[s])  %>% 
        rename("siteID" = site) %>% 
        arrange(siteID, Date)
    }
    
    if(size == "Plot"){
      df.save <- df.save %>% 
        separate(site, c("data", "plotID"), sep = 4) %>%
        filter(plotID != "ORNL_006") %>%
        rename_at(variables[s], ~ variable.name[s])  %>% 
        arrange(plotID, Date)
    }
    
    
    write_csv(df.save, 
              file = file.path("./Data", paste0("daymet", size, "_", variable.name[s], ".csv")))
  }  
}

make_csvs("Site")

make_csvs(size = "Plot")
