library(dplyr)

lai <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/full_LAIs.csv")

lai <- lai |>
  mutate(week = floor_date(date,  "week"))

ticks <- read.csv("/projectnb/dietzelab/ebeasley/TickForecast/Data/tickLong.csv")
ticks$collectDate <- as.Date(ticks$collectDate)

ixodes <- ticks |>
  mutate(week = floor_date(collectDate, "week")) |>
  filter(scientificName %in% c("Ixodes scapularis"))

