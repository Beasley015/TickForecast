library(dplyr)

dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_FINAL_bounders.csv")


#=========================SPATIAL

library(lubridate)

lai <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/MODIS_site_LAIs.csv")
lai$year <- year(lai$date)

vi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/MODIS_site_VIs.csv")
vi$year <- year(vi$date)

ticks <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv")


leaf <- lai |>
  group_by(siteID) |>
  summarise(lai = mean(lai_median))




veg <- vi |>
  group_by(siteID) |>
  summarise(ndvi = mean(ndvi_median),
            evi = mean(evi_median))

deer <- ticks |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Nymph") |>
  group_by(siteID, collectDate) |>
  summarise(total_count = mean(processedCount)) |>
  group_by(siteID) |>
  summarise(count = mean(total_count))

adult <- ticks |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Adult") |>
  group_by(siteID, collectDate) |>
  summarise(total_count = mean(processedCount)) |>
  group_by(siteID) |>
  summarise(adult = mean(total_count))


kat <- inner_join(leaf, deer)
kat <- inner_join(kat, veg)


amp_leaf <- lai |>
  filter(year > 2015) |>
  group_by(siteID, year) |>
  summarise(amp = mean(max(lai_median) - min(lai_median))) |>
  group_by(siteID) |>
  summarise(lai_delta = mean(amp))


amp_veg <- vi |>
  filter(year > 2015) |>
  group_by(siteID, year) |>
  summarise(amp_ndvi = mean(max(ndvi_median) - min(ndvi_median)),
            amp_evi = mean(max(evi_median) - min(evi_median))) |>
  group_by(siteID) |>
  summarise(ndvi_delta = mean(amp_ndvi),
            evi_delta = mean(amp_evi))

kat <- inner_join(kat, amp_leaf)
kat <- inner_join(kat, amp_veg)
kat <- inner_join(kat, adult)




#=============TEMPORAL==============
library(lubridate)
lai$date <- as.Date(lai$date)
ticks$collectDate <- as.Date(ticks$collectDate)

site <- "SERC"


c1 <- lai |>
  filter(siteID == site) |>
  mutate(week = floor_date(date, "week")) |>
  group_by(week) |>
  summarise(lai = mean(lai_median))

c2 <- ticks |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Nymph") |>
  filter(siteID == site) |>
  mutate(week = floor_date(collectDate, "week")) |>
  group_by(week) |>
  summarise(tick = mean(processedCount))


norm <- function(c){
  return((c - min(c)) / (max(c) - min(c)))
}




met <- read.csv("/projectnb/dietzelab/ebeasley/TickForecast/Data/daymetSite.csv")

met <- met |>
  select(year = X2016, DOY = X1, maxTemp = X6.45, minTemp = X1.24, siteID = BLAN)

met$maxTemp <- as.numeric(met$maxTemp)
met$minTemp <- as.numeric(met$minTemp)

met <- met |> 
  mutate(date = as.Date(as.numeric(DOY) - 1, origin = paste0(year, "-01-01"))) |>
  mutate(avgTemp = (maxTemp + minTemp)/2) |>
  arrange(by = date)


c3 <- met |>
  filter(siteID == site) |>
  mutate(week = floor_date(date, "week")) |>
  group_by(week) |>
  summarise(temp = mean(avgTemp))


c2 <- c2 |>
  filter(year(week) >= 2016)

c4 <- inner_join(c1, c2, by = "week")
c4 <- inner_join(c4, c3, by = "week")


c4$lai_bucket <- cut(
  c4$lai,
  breaks = seq(min(c4$lai, na.rm = TRUE),
               max(c4$lai, na.rm = TRUE),
               length.out = 5),   # 4 buckets
  include.lowest = TRUE
)

boxplot(
  tick ~ lai_bucket,
  data = c4,
  xlab = "LAI",
  ylab = "Mean tick count",
  main = "",
  col = c("white", "white", "limegreen", "tomato")
)





plot(c2$week, norm(c2$tick), type = "l")
lines(c1$week, norm(c1$lai), col = "green")
lines(c3$week, norm(c3$temp), col = "red")


d1 <- lai |>
  filter(siteID == site) |>
  mutate(DOY = yday(date))

d2 <- ticks |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Nymph") |>
  filter(siteID == site) |>
  group_by(collectDate) |>
  summarise(count = sum(processedCount)) |>
  mutate(DOY = yday(collectDate))

d3 <- met |>
  filter(siteID == site)

d2$year <- year(d2$collectDate)

d2 <- d2 |>
  filter(year >= 2016)



plot(d2$collectDate, norm(d2$count), type = "l")
lines(d1$date, norm(d1$lai_median), col = "green")
lines(d3$date, norm(d3$avgTemp), col = "red")



years <- 2016:2025
pheno <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/pheno.csv")

for(y in years){
  t3 <- d3 |>
    filter(year == y)
  t1 <- d1 |>
    filter(year == y)
  t2 <- d2 |>
    filter(year == y)
  
  p <- pheno |>
    filter(siteID == site) |>
    filter(year == y)
  
  g <- as.Date(p$greenup - 1, origin = paste0(y, "-01-01"))
  
  
  plot(t3$date, norm(t3$avgTemp), col = "red", type = "l", main = paste0(site, " ", y), xlab = "Date", ylab = "Normalized temperature, LAI, or ticks")
  lines(t1$date, norm(t1$lai_median), col = "green")
  lines(t2$collectDate, norm(t2$count), type = "l")
  abline(v = g, col = "forestgreen", lty = 2)
  
  legend("topright", 
         legend = c("Avg. temperature", "LAI", paste0("Greenup DOY = ", p$greenup),"Ixodes nymph count"), 
         col = c("red", "green", "forestgreen", "black"), 
         lty = c(1, 1, 2, 1),
         cex = 0.6)
}









d5 <- inner_join(d1, d3, by = "date")



plot(d2$collectDate, (d2$count), type = "l", xlab = "Date", ylab = "Ixodes nymph count")

for(y in years){
  
  p <- pheno |>
    filter(siteID == site) |>
    filter(year == y)
  
  g <- as.Date(p$greenup - 1, origin = paste0(y, "-01-01"))
  
  abline(v = g, col = "forestgreen", lty = 2)
  
}



legend("topright", 
       legend = c("Ixodes nymph count", "Greenup"), 
       col = c("black", "forestgreen"), 
       lty = c(1, 2),
       cex = 0.7)
