library(dplyr)
library(lubridate)

#===================LAI======================
lai <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/full_LAIs.csv")
lai <- lai |>
  mutate(date = as.Date(date),
         week = floor_date(date,  "week"))


amp <- lai |>
  mutate(year = year(date)) |>
  group_by(siteID, year) |>
  summarize(delta_LAI = max(lai_median) - min(lai_median)) 

amp <- amp |>
  filter(year != 2015) 

amp <- amp |>
  group_by(siteID) |>
  summarize(litter = mean(delta_LAI))





#=================Ticks============
ticks <- read.csv("/projectnb/dietzelab/ebeasley/TickForecast/Data/tickLong.csv")
ticks$collectDate <- as.Date(ticks$collectDate)

ixodes <- ticks |>
  mutate(week = floor_date(collectDate, "week")) |>
  filter(scientificName %in% c("Ixodes scapularis"))

amblyomma <- ticks |>
  mutate(week = floor_date(collectDate, "week")) |>
  filter(scientificName %in% c("Amblyomma americanum"))

stages <- c("Adult")

ixodes <- ixodes |>
  filter(!is.na(collectDate)) |>
  filter(lifeStage %in% stages) |>
  group_by(siteID, collectDate) |>
  summarise(total_count = sum(processedCount, na.rm = TRUE), .groups = "drop") |>
  group_by(siteID) |>
  summarise(mean_count = mean(total_count, na.rm = TRUE), .groups = "drop")

amblyomma <- amblyomma |>
  filter(!is.na(collectDate)) |>
  filter(lifeStage %in% stages) |>
  group_by(siteID, collectDate) |>
  summarise(total_count = sum(processedCount, na.rm = TRUE), .groups = "drop") |>
  group_by(siteID) |>
  summarise(mean_count = mean(total_count, na.rm = TRUE), .groups = "drop")



#=================ANALYZE==============


dat <- inner_join(amp, ixodes)
kat <- inner_join(amp, amblyomma)










#====================CCF + lagged regression======================
library(dplyr)
library(lubridate)
library(tidyr)

# LAI amplitude
lai <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/full_LAIs.csv") |>
  mutate(date = as.Date(date),
         year = year(date))

amp <- lai |>
  filter(year != 2015) |>
  group_by(siteID, year) |>
  summarize(
    delta_LAI = max(lai_median, na.rm = TRUE) - min(lai_median, na.rm = TRUE),
    .groups = "drop"
  )

# ticks
ticks <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv") |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(scientificName == "Amblyomma americanum")

ticks_annual <- ticks |>
  group_by(siteID, year, lifeStage) |>
  summarize(annual_mean = mean(processedCount, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = lifeStage,
    values_from = annual_mean,
    values_fill = 0
  )

# test lags
lag_results <- data.frame()

for(L in 0:3){
  
  dat_lag <- amp |>
    mutate(tick_year = year + L) |>
    inner_join(ticks_annual, by = c("siteID", "tick_year" = "year"))
  
  fit_nymph <- lm(Nymph ~ delta_LAI + siteID, data = dat_lag)
  fit_adult <- lm(Adult ~ delta_LAI + siteID, data = dat_lag)
  
  lag_results <- rbind(
    lag_results,
    data.frame(
      lag = L,
      stage = "Nymph",
      beta = coef(fit_nymph)[2],
      R2 = summary(fit_nymph)$r.squared,
      p = summary(fit_nymph)$coefficients[2, 4]
    ),
    data.frame(
      lag = L,
      stage = "Adult",
      beta = coef(fit_adult)[2],
      R2 = summary(fit_adult)$r.squared,
      p = summary(fit_adult)$coefficients[2, 4]
    )
  )
}

lag_results


