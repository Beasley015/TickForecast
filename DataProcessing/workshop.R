library(dplyr)
library(lubridate)
library(tidyr)


#---------DATA SETUP-----------------

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
  filter(year <= 2025) |>
  drop_na(siteID, year, DOY, count)


met <- read.csv(
  "/usr4/ugrad/neochatt/TickForecast/Data/daymetSite.csv"
)

met <- met |>
  select(
    year = X2016,
    DOY = X1,
    maxTemp = X6.45,
    minTemp = X1.24,
    siteID = BLAN
  ) |>
  mutate(
    year = as.numeric(year),
    DOY = as.numeric(DOY),
    maxTemp = as.numeric(maxTemp),
    minTemp = as.numeric(minTemp),
    
    date = as.Date(
      paste(
        year,
        DOY,
        sep = "-"
      ),
      format = "%Y-%j"
    ),
    
    avgTemp = (
      maxTemp +
        minTemp
    ) / 2
  ) |>
  filter(
    year >= 2016,
    year <= 2025
  ) |>
  drop_na(
    siteID,
    year,
    DOY,
    date,
    avgTemp
  ) |>
  arrange(
    siteID,
    year,
    DOY
  ) |>
  group_by(
    siteID,
    year
  ) |>
  mutate(
    GDD = pmax(
      0,
      avgTemp - 4
    ),
    
    CGDD = cumsum(GDD)
  ) |>
  ungroup()


vpd <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/daymetSite_vaporPressure.csv")
vpd$date <- as.Date(vpd$Date)
vpd$VPD <- pmax(
  0,
  (
    0.6108 * exp(17.27 * vpd$tmax..deg.c. / (vpd$tmax..deg.c. + 237.3)) +
      0.6108 * exp(17.27 * vpd$tmin..deg.c. / (vpd$tmin..deg.c. + 237.3))
  ) / 2 -
    vpd$vaporPressure / 1000
)
vpd <- na.omit(vpd)


precip <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/daymetSite_precipitation.csv")
precip$date <- as.Date(precip$Date)
#------------------------------------------

s <- "BLAN"

blan <- deer |> filter(siteID == s)
temp <- vpd |> filter(siteID == s)
rain <- precip |> filter(siteID == s)


blan <- blan |>
  group_by(year) |>
  mutate(norm_count = norm(count))

blan <- blan |>
  group_by(year) |>
  mutate(peak_date = collectDate[which.max(count)])



temp <-temp |> 
  mutate(VPD_7d = rollmean(VPD, k = 7, align = "right", fill = NA))

temp <- na.omit(temp)

temp <- left_join(temp, blan, by = c("siteID", "date" = "collectDate", "year"))

temp <- temp |>
  group_by(year) |>
  mutate(
    CVPD = cumsum(VPD),
    peak_date = as.Date(
      round(
        mean(
          as.numeric(peak_date),
          na.rm = TRUE
        )
      ),
      origin = "1970-01-01"
    ),
    peak_CVPD = CVPD[
      which.min(
        abs(
          as.numeric(date - first(peak_date))
        )
      )
    ]
  ) |>
  ungroup()


stat <- temp |>
  group_by(year) |>
  summarise(peak_CVPD = mean(peak_CVPD))

threshold_dates <- temp |>
  group_by(year) |>
  arrange(date) |>
  filter(
    CVPD >= 80,
    is.na(lag(CVPD)) | lag(CVPD) < 80
  ) |>
  pull(date)


#---------------EXPLORE--------------------
plot(blan$collectDate, norm(blan$count), type = "l", ylim = c(0, 3))
lines(temp$date, (temp$VPD_7d), col = "blue")



plot(blan$collectDate, (blan$count), type = "l")
par(new = TRUE)
plot(temp$date, temp$CVPD, col = "blue", type = "l", 
     axes = FALSE)
axis(side = 4)

abline(
  v = as.numeric(threshold_dates),
  col = "blue",
  lty = 2
)



both <- left_join(blan, temp, by = c("collectDate" = "date", "siteID", "year"))




plot(both$collectDate, norm(both$count), type = "l")
lines(both$collectDate, norm(both$VPD), col = "blue")



#--------BY-YEAR------------------------------

yrs <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

bear <- blan |> filter(year %in% yrs)
wat <- temp |> filter(year %in% yrs)
drop <- rain |> filter(year %in% yrs)



plot(bear$collectDate, norm(bear$count), type = "l", ylim = c(0, 3))
#lines(wat$date, wat$VPD, col = "blue")




wat <- wat |>
  arrange(date) |>
  mutate(
    date_num = as.numeric(date)
  )

vpd_loess <- loess(
  VPD ~ date_num,
  data = wat,
  span = 0.15,
  na.action = na.exclude
)

wat$VPD_smooth <- predict(
  vpd_loess,
  newdata = data.frame(
    date_num = wat$date_num
  )
)

lines(
  wat$date,
  wat$VPD_smooth,
  col = "blue",
  lwd = 2
)






  
  
  




