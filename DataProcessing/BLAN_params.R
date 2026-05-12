library(dplyr)
library(lubridate)

deer <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/BLANIxodesscapularisWithWeatherAndMiceGlobal_parameterSummary.csv")
star <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/BLANAmblyommaamericanumWithWeatherAndMiceGlobal_parameterSummary.csv")
lai <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/all_LAIs.csv")


lai$date <- as.Date(lai$date, format = "%m/%d/%Y")
lai <- lai |> filter(siteID == "BLAN")
lai <- lai |>
  arrange(date)

dat <- deer |>
  filter(deer$node == "phi.n.mu")

dat$start.date <- as.Date(dat$start.date)



#compute weekly means
lai <- lai |> 
  mutate(week = floor_date(date, "week"))

lais <- lai |>
  group_by(week) |>
  summarise(weekly_lai = mean(lai_mean, na.rm = TRUE))

dat <- dat |>
  mutate(week = floor_date(start.date, "week"))

deer_ticks <- dat |>
  group_by(week) |>
  summarise(weekly_phi = mean(mean, na.rm = TRUE))




kat <- inner_join(lais, deer_ticks, by = "week")
names(kat)[2] <- "lai" 
names(kat)[3] <- "phi"


kat$lai_z <- scale(kat$lai)
kat$phi_z <- scale(kat$phi)