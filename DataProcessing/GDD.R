library(dplyr)
library(lubridate)
library(degday)

#--------------TEMP DATA-----------------
met <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/daymetSite.csv")

met <- met |>
  dplyr::select(year = X2016, DOY = X1, maxTemp = X6.45, minTemp = X1.24, siteID = BLAN)

met$maxTemp <- as.numeric(met$maxTemp)
met$minTemp <- as.numeric(met$minTemp)

met <- met |> 
  mutate(date = as.Date(as.numeric(DOY) - 1, origin = paste0(year, "-01-01"))) |>
  mutate(avgTemp = (maxTemp + minTemp)/2) |>
  arrange(by = date)

met <- met |>
  arrange(siteID)

met <- na.omit(met)


#----------------DOYs--------------
doy <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/DOYs.csv")

#---------------ticks--------------
site <- "GREN"
y = 2021


ticks <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv") |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Nymph")




#------------space------------
dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_FINAL_bounders.csv") |>
  dplyr::select(siteID, latitude = latitude_top_left)

dat <- left_join(doy, dat, by= "siteID")

dat <- dat |>
  group_by(siteID) |>
  mutate(
    greenup_mean = mean(greenup, na.rm = TRUE),
    greenup_anom = greenup - greenup_mean
  ) |>
  ungroup()

dat <- dat |>
  mutate(
    lat_c = latitude - mean(latitude, na.rm = TRUE)
  )


dat <- dat |>
  group_by(siteID) |>
  mutate(tick_anom = tick_15 - mean(tick_15)) |>
  ungroup()


cols <- rainbow(length(unique(dat$siteID)))
sites <- unique(dat$siteID)
lats <- unique(dat$latitude)

n <- 1

for(s in unique(dat$siteID)){
  
  if(s == "GREN"){
    sub <- dat |> filter(siteID == s)
    plot(sub$year, sub$tick_anom, col = cols[n], pch = 16, ylim = c(-20, 20),
         xlim = c(2016, 2024))
    lines(sub$year, sub$tick_anom, col = cols[n])
    
    n <- n + 1
  } else {
    
    sub <- dat |> filter(siteID == s)
    points(sub$year, sub$tick_anom, col = cols[n], pch = 16)
    lines(sub$year, sub$tick_anom, col = cols[n])
    
    n <- n + 1
  }
  
}










