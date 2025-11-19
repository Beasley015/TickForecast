# Script to wrangle meteorological data from Cary
# into a format that matches daymet

# Output .csv files are inputs for the script daymet_downscale.R

library(tidyverse)
library(lubridate)

raw.met <- read.csv("./Data/Cary_Met_Data_Daily.csv")

# Max Temperature --------------------
maxtemp <- raw.met %>%
  select(DATE, MAX_TEMP) %>%
  rename("maxTemperature" = "MAX_TEMP", "Date" = "DATE") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(year=year(Date)) %>%
  mutate(yday=yday(Date))

write.csv(maxtemp, "./Data/Cary_maxTemperature.csv", row.names = F)  

# Min Temperature -------------------
mintemp <- raw.met %>%
  select(DATE, MIN_TEMP) %>%
  rename("minTemperature" = "MIN_TEMP", "Date" = "DATE") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(year=year(Date)) %>%
  mutate(yday=yday(Date))

write.csv(mintemp, "./Data/Cary_minTemperature.csv", row.names = F) 

# Relative humidity -----------------------------
rh <- raw.met %>%
  select(DATE, MAX_RH, MIN_RH) %>%
  rename("maxRH"=MAX_RH, minRH = MIN_RH, "Date" = "DATE") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(year=year(Date)) %>%
  mutate(yday=yday(Date))

write.csv(rh, "./Data/Cary_vaporPressure.csv", row.names = F) 

# Precipitation -----------------