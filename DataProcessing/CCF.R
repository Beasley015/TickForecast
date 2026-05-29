#This script generates CCF plots comparing the *differenced* tick (larva, nymph, 
#or adult) and vegetation (LAI, NDVI, or EVI) time series for a particular site; it
#also plots the said time series simultaneously for visual comparison. 


#================Obtain site-specific tick data 
library(dplyr)
library(lubridate)
library(tidyr)

site <- "HARV"

ticks <- read.csv("/projectnb/dietzelab/ebeasley/TickForecast/Data/tickLong.csv", stringsAsFactors = FALSE)
ticks$collectDate <- as.Date(ticks$collectDate)
ticks <- ticks[ticks$scientificName %in% c("Ixodes scapularis", "Amblyomma americanum"), ]


ticks_wide <- ticks |> 
  pivot_wider(names_from = lifeStage, values_from = processedCount)


site_ticks <- ticks_wide |> filter(siteID == site)

d <- 1

#===============================LAI================================
lai <- read.csv("/projectnb/dietzelab/ebeasley/TickForecast/Data/full_LAIs.csv", stringsAsFactors = FALSE)
lai$date <- as.Date(lai$date, format = "%m/%d/%Y")
lai <- lai |> filter(siteID == site)
lai <- lai |>
  arrange(date)

floor <- "2015-12-27" 

site_ticks <- site_ticks |> filter(collectDate >= as.Date("2015-12-27"))
site_ticks <- site_ticks |>
  arrange(collectDate)


site_ticks <- site_ticks |> 
  mutate(week = floor_date(collectDate, "week"))

lai <- lai |> 
  mutate(week = floor_date(date, "week"))

site_ticks <- site_ticks |>
  filter(format(week, "%m-%d") >= "02-15",
         format(week, "%m-%d") <= "05-31")

lai <- lai |>
  filter(format(week, "%m-%d") >= "02-15",
         format(week, "%m-%d") <= "05-31")


#Disaggregate by life stage 
larvae <- site_ticks |> 
  group_by(week) |>
  summarise(weekly_mean = mean(as.numeric(Larva), na.rm = TRUE))

nymphs <- site_ticks |>
  group_by(week) |>
  summarise(weekly_mean = mean(as.numeric(Nymph), na.rm = TRUE))

adults <- site_ticks |>
  group_by(week) |>
  summarise(weekly_mean = mean(as.numeric(Adult), na.rm = TRUE))

lais <- lai |>
  group_by(week) |>
  summarise(weekly_lai = mean(lai_mean, na.rm = TRUE))

final <- inner_join(larvae, lais, by = "week")
final2 <- inner_join(nymphs, lais, by = "week")
final3 <- inner_join(adults, lais, by = "week")

plot(final$week, final$weekly_mean, lwd = 1.5, col = "blue", type = "l", xlab = "Date", ylab = "Weekly mean tick count", main = paste(site, "Larvae vs LAI"))
points(final$week, final$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

#==============LARVAE
plot(final$week, final$weekly_lai,
     type = "l", lwd = 1.5,
     axes = FALSE,      # suppress default axes
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final$week, final$weekly_lai, pch = 16, cex = 0.7, col = adjustcolor("orange", alpha.f = 0.5))


legend("bottomright", 
       legend = c("Ticks (left axis)", "LAI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

# Add right-side axis for LAI
axis(side = 4)

# Label right axis
mtext("Weekly mean LAI", side = 4, line = 3)



ccf(
  diff(final$weekly_lai, differences = 1),
  diff(final$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "",
  ylim = c(-d, d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "LAI vs Larva Count (CCF) (differenced)")
)


#==============NYMPHS

plot(final2$week, final2$weekly_mean, lwd = 1.5, col = "blue", type = "l", xlab = "Date", ylab = "Weekly mean tick count", main = paste(site, "Nymphs vs LAI"))
points(final2$week, final2$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final2$week, final2$weekly_lai,
     type = "l", lwd = 1.5,
     axes = FALSE,      # suppress default axes
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final2$week, final2$weekly_lai, pch = 16, cex = 0.7, col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "LAI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

# Add right-side axis for LAI
axis(side = 4)

# Label right axis
mtext("Weekly mean LAI", side = 4, line = 3)




ccf(
  diff(final2$weekly_lai, differences = 1),
  diff(final2$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "", ylim = c(-d,d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "LAI vs Nymph Count (CCF) (differenced)")
)




#==============ADULTS

plot(final3$week, final3$weekly_mean, lwd = 1.5, col = "blue", type = "l", xlab = "Date", ylab = "Weekly mean tick count", main = paste(site, "Adults vs LAI"))
points(final3$week, final3$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final3$week, final3$weekly_lai,
     type = "l", lwd = 1.5,
     axes = FALSE,      # suppress default axes
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final3$week, final3$weekly_lai, pch = 16, cex = 0.7, col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "LAI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

# Add right-side axis for LAI
axis(side = 4)

# Label right axis
mtext("Weekly mean LAI", side = 4, line = 3)



ccf(
  diff(final3$weekly_lai, differences = 1),
  diff(final3$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "", ylim = c(-d,d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "LAI vs Adult Count (CCF) (differenced)")
)




#===============================NDVI================================
site_ticks <- ticks_wide |> filter(siteID == site)

ndvi <- read.csv("/projectnb/dietzelab/ebeasley/TickForecast/Data/full_VIs.csv", stringsAsFactors = FALSE)

ndvi$date_raw <- ndvi$date

ndvi$date <- parse_date_time(
  as.character(ndvi$date_raw),
  orders = c("mdY", "m/d/Y", "ymd")
)

ndvi <- ndvi |> filter(siteID == site)
ndvi <- ndvi |> arrange(date)

floor <- "2015-12-27" 

site_ticks <- site_ticks |> 
  filter(collectDate >= as.Date("2015-12-27")) |>
  arrange(collectDate) |>
  mutate(week = floor_date(collectDate, "week"))

ndvi <- ndvi |> 
  mutate(week = floor_date(date, "week"))

larvae <- site_ticks |> 
  group_by(week) |>
  summarise(weekly_mean = mean(Larva, na.rm = TRUE))

nymphs <- site_ticks |>
  group_by(week) |>
  summarise(weekly_mean = mean(Nymph, na.rm = TRUE))

adults <- site_ticks |>
  group_by(week) |>
  summarise(weekly_mean = mean(Adult, na.rm = TRUE))

ndvis <- ndvi |>
  group_by(week) |>
  summarise(weekly_ndvi = mean(ndvi_mean, na.rm = TRUE))

final <- inner_join(larvae, ndvis, by = "week")
final2 <- inner_join(nymphs, ndvis, by = "week")
final3 <- inner_join(adults, ndvis, by = "week")



#====================Larvae
plot(final$week, final$weekly_mean, lwd = 1.5, col = "blue", type = "l",
     xlab = "Date", ylab = "Weekly mean tick count",
     main = paste(site, "Larvae vs NDVI"))
points(final$week, final$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final$week, final$weekly_ndvi,
     type = "l", lwd = 1.5,
     axes = FALSE,
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final$week, final$weekly_ndvi, pch = 16, cex = 0.7,
       col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "NDVI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

axis(side = 4)
mtext("Weekly mean NDVI", side = 4, line = 3)

ccf(
  diff(final$weekly_ndvi, differences = 1),
  diff(final$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "",
  ylim = c(-d, d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "NDVI vs Larva Count (CCF) (differenced)")
)


#====================Nymphs

plot(final2$week, final2$weekly_mean, lwd = 1.5, col = "blue", type = "l",
     xlab = "Date", ylab = "Weekly mean tick count",
     main = paste(site, "Nymphs vs NDVI"))
points(final2$week, final2$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final2$week, final2$weekly_ndvi,
     type = "l", lwd = 1.5,
     axes = FALSE,
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final2$week, final2$weekly_ndvi, pch = 16, cex = 0.7,
       col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "NDVI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

axis(side = 4)
mtext("Weekly mean NDVI", side = 4, line = 3)

ccf(
  diff(final2$weekly_ndvi, differences = 1),
  diff(final2$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "",
  ylim = c(-d, d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "NDVI vs Nymph Count (CCF) (differenced)")
)


#====================Adults

plot(final3$week, final3$weekly_mean, lwd = 1.5, col = "blue", type = "l",
     xlab = "Date", ylab = "Weekly mean tick count",
     main = paste(site, "Adults vs NDVI"))
points(final3$week, final3$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final3$week, final3$weekly_ndvi,
     type = "l", lwd = 1.5,
     axes = FALSE,
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final3$week, final3$weekly_ndvi, pch = 16, cex = 0.7,
       col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "NDVI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

axis(side = 4)
mtext("Weekly mean NDVI", side = 4, line = 3)

ccf(
  diff(final3$weekly_ndvi, differences = 1),
  diff(final3$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "",
  ylim = c(-d, d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "NDVI vs Adult Count (CCF) (differenced)")
)



#===============================EVI================================
site_ticks <- ticks_wide |> filter(siteID == site)

evi <- read.csv("full_VIs.csv", stringsAsFactors = FALSE)

evi$date_raw <- evi$date

evi$date <- parse_date_time(
  as.character(evi$date_raw),
  orders = c("mdY", "m/d/Y", "ymd")
)

evi <- evi |> filter(siteID == site)
evi <- evi |> arrange(date)

floor <- "2015-12-27" 

site_ticks <- site_ticks |> 
  filter(collectDate >= as.Date("2015-12-27")) |>
  arrange(collectDate) |>
  mutate(week = floor_date(collectDate, "week"))

evi <- evi |> 
  mutate(week = floor_date(date, "week"))

larvae <- site_ticks |> 
  group_by(week) |>
  summarise(weekly_mean = mean(Larva, na.rm = TRUE))

nymphs <- site_ticks |>
  group_by(week) |>
  summarise(weekly_mean = mean(Nymph, na.rm = TRUE))

adults <- site_ticks |>
  group_by(week) |>
  summarise(weekly_mean = mean(Adult, na.rm = TRUE))

evis <- evi |>
  group_by(week) |>
  summarise(weekly_evi = mean(evi_mean, na.rm = TRUE))

final <- inner_join(larvae, evis, by = "week")
final2 <- inner_join(nymphs, evis, by = "week")
final3 <- inner_join(adults, evis, by = "week")


#====================Larvae
plot(final$week, final$weekly_mean, lwd = 1.5, col = "blue", type = "l",
     xlab = "Date", ylab = "Weekly mean tick count",
     main = paste(site, "Larvae vs EVI"))
points(final$week, final$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final$week, final$weekly_evi,
     type = "l", lwd = 1.5,
     axes = FALSE,
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final$week, final$weekly_evi, pch = 16, cex = 0.7,
       col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "EVI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

axis(side = 4)
mtext("Weekly mean EVI", side = 4, line = 3)

ccf(
  diff(final$weekly_evi, differences = 1),
  diff(final$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "",
  ylim = c(-d, d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "EVI vs Larva Count (CCF) (differenced)")
)


#====================Nymphs

plot(final2$week, final2$weekly_mean, lwd = 1.5, col = "blue", type = "l",
     xlab = "Date", ylab = "Weekly mean tick count",
     main = paste(site, "Nymphs vs EVI"))
points(final2$week, final2$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final2$week, final2$weekly_evi,
     type = "l", lwd = 1.5,
     axes = FALSE,
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final2$week, final2$weekly_evi, pch = 16, cex = 0.7,
       col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "EVI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

axis(side = 4)
mtext("Weekly mean EVI", side = 4, line = 3)

ccf(
  diff(final2$weekly_evi, differences = 1),
  diff(final2$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "",
  ylim = c(-d, d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "EVI vs Nymph Count (CCF) (differenced)")
)


#====================Adults

plot(final3$week, final3$weekly_mean, lwd = 1.5, col = "blue", type = "l",
     xlab = "Date", ylab = "Weekly mean tick count",
     main = paste(site, "Adults vs EVI"))
points(final3$week, final3$weekly_mean, pch = 16, cex = 0.7, col = "blue")

par(new = TRUE)

plot(final3$week, final3$weekly_evi,
     type = "l", lwd = 1.5,
     axes = FALSE,
     xlab = "", ylab = "",
     col = adjustcolor("orange", alpha.f = 0.5))
points(final3$week, final3$weekly_evi, pch = 16, cex = 0.7,
       col = adjustcolor("orange", alpha.f = 0.5))

legend("bottomright", 
       legend = c("Ticks (left axis)", "EVI (right axis)"), 
       col = c("blue", adjustcolor("orange", alpha.f = 0.5)),
       lty = 1,
       cex = 0.6)

axis(side = 4)
mtext("Weekly mean EVI", side = 4, line = 3)

ccf(
  diff(final3$weekly_evi, differences = 1),
  diff(final3$weekly_mean, differences = 1),
  lag.max = 52,
  xlab = "", ylab = "", main = "",
  ylim = c(-d, d)
)

title(
  xlab = "Lag (weeks)",
  ylab = "Cross-correlation",
  main = paste(site, "EVI vs Adult Count (CCF) (differenced)")
)