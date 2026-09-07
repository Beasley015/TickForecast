library(dplyr)
library(lubridate)
library(tidyr)

ticks <- read.csv(
  "/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv"
) |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(
    scientificName == "Ixodes scapularis",
    lifeStage == "Nymph"
  )


deer <- ticks |>
  arrange(siteID, collectDate) |>
  group_by(siteID, collectDate) |>
  summarise(
    count = sum(processedCount, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    year = year(collectDate),
    DOY = yday(collectDate)
  ) |>
  filter(year > 2015) |>
  drop_na(siteID, year, DOY, count)

#-----------MET----------------------
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

met$DOY <- as.numeric(met$DOY)

met <- na.omit(met)

met <- met |>
  group_by(siteID, year) |>
  mutate(GDD = pmax(0, avgTemp-4),
         CGDD = cumsum(GDD))
#---------------- DAT SETUP ----------------
doy <- read.csv(
  "/usr4/ugrad/neochatt/TickForecast/Data/DOYs.csv"
)

retreat <- read.csv(
  "/usr4/ugrad/neochatt/TickForecast/Data/RETREAT.csv"
)

lat <- read.csv(
  "/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_FINAL_bounders.csv"
) |>
  group_by(siteID) |>
  summarise(
    latitude = mean(
      c(latitude_top_left, latitude_bottom_right),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

dat <- doy |>
  left_join(retreat) |>
  inner_join(lat, by = "siteID")


deer <- deer |> filter(siteID %in% unique(dat$siteID))

deer <- deer |>
  arrange(siteID, collectDate)

#---------------ANALYZE----------------------------

s <- "BLAN"

blan <- deer |>
  filter(siteID == s)

blan$collectDate <- as.Date(blan$collectDate)

synthetic <- blan |>
  distinct(siteID, year) |>
  tidyr::crossing(DOY = c(1, 365)) |>
  anti_join(blan, by = c("siteID", "year", "DOY")) |>
  mutate(count = 0)

blan <- bind_rows(blan, synthetic) |>
  mutate(
    collectDate = as.Date(paste0(year, "-01-01")) + DOY - 1
  ) |>
  arrange(year, DOY)
  




gdd <- met |> filter(siteID == s)

gdd <- gdd |>
  mutate(GDD = pmax(0, avgTemp - 4)) |>
  group_by(year) |>
  mutate(CGDD = cumsum(GDD))

gdd$year <- as.numeric(gdd$year)


dat <- gdd |>
  ungroup() |>
  left_join(
    blan |>
      select(siteID, year, DOY, collectDate, count),
    by = c("siteID", "year", "DOY")
  ) |>
  arrange(year, DOY)




plot(dat$date, dat$avgTemp, type = "l", col = "red")
non_na <- !is.na(dat$count)

lines(
  dat$date[non_na],
  dat$count[non_na]
)




kat <- dat |>
  group_by(siteID, year) |>
  arrange(DOY, .by_group = TRUE) |>
  mutate(
    temp_7day = slider::slide_dbl(
      avgTemp,
      mean,
      .before = 3,
      .after = 3,
      .complete = FALSE,
      na.rm = TRUE
    ),
    peak_temp_DOY = DOY[which.max(temp_7day)],
    days_from_temp_peak = DOY - peak_temp_DOY,
    relative_temp = temp_7day / max(temp_7day, na.rm = TRUE),
    temp_slope_7day = temp_7day - lag(temp_7day, 7)
  ) |>
  ungroup()


samples <- kat |>
  filter(!is.na(count)) |>
  mutate(count_on = count > 0)

plot(
  samples$days_from_temp_peak,
  samples$count,
  pch = 16,
  xlab = "Days from annual temperature peak",
  ylab = "Observed count"
)

abline(v = 0, col = "red", lty = 2)




