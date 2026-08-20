library(dplyr)
library(lubridate)
library(mgcv)

sites <- c("GREN", "HNRY", "TEA", "BLAN", "SCBI", "SERC", "ORNL", "MLBS", "TREE", "HARV")

#---------------TICKS
deer <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv") |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Nymph") |>
  arrange(collectDate) |>
  group_by(siteID, collectDate) |>
  summarise(count = sum(processedCount)) |>
  mutate(year = year(collectDate)) |>
  filter(siteID %in% sites) |>
  mutate(DOY = as.numeric(format(collectDate, "%j")))


deer <- deer[deer$year > 2015, ]
deer <- na.omit(deer)

#----------VEG
evi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/MODIS_site_VIs.csv") |>
  dplyr::select(siteID, date, evi_mean, evi_median, evi_sd) |>
  mutate(date = as.Date(date)) |>
  mutate(DOY = as.numeric(format(date, "%j"))) |>
  mutate(year = year(date)) |>
  filter(year >= 2016)

# interpolate
evi365 <- evi |>
  group_by(siteID, year) |>
  group_modify(~{
    xout <- 1:365
    
    interp <- approx(
      x = .x$DOY,
      y = .x$evi_median,
      xout = xout,
      rule = 2
    )
    
    data.frame(
      date = as.Date(xout - 1, origin = paste0(.y$year, "-01-01")),
      DOY = xout,
      EVI = interp$y
    )
  }) |>
  ungroup()


#-------------PHENO
pheno <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/phenology_DOYs.csv")
pheno <- pheno |>
  arrange(siteID, year) |>
  group_by(siteID) |>
  mutate(mean_greenup = cummean(greenup))


dat <- left_join(deer, evi365, by = c("siteID", "year", "DOY"))
dat <- left_join(dat, pheno, by = c("siteID", "year"))




dat <- dat %>%
  group_by(siteID) %>%
  mutate(
    mean_greenup = if_else(
      year == 2025,
      mean(greenup[year >= 2016 & year <= 2024], na.rm = TRUE),
      mean_greenup
    )
  ) %>%
  ungroup()

dat$rel_greenup <- dat$DOY - dat$mean_greenup

#-----------TRAIN AND PREDICT---------------
except <- "ORNL"
train <- dat |> filter(year <= 2024) |> 
  filter(siteID != except)

train$siteID <- factor(train$siteID)

mod <- gam(
  count ~ s(rel_greenup) + s(EVI) + s(siteID, bs = "re"),
  family = nb(),
  data = train
)

kat <- read.csv("/usr4/ugrad/neochatt/TickForecast/DataProcessing/SAVE.csv")
#------AMBLYOMMA SWITCH------
#bat <- dat |> filter(year == 2025)
#kat <- bat

#-----------------------------
kat$pred <- predict(mod, newdata = kat, type = "response")
kat$date <- as.Date(kat$date)

site <- "GREN"


#----observed
obsv <- deer |>
  filter(siteID == site) |>
  filter(year == 2025)

#----test predictions
b25 <- kat |> filter(siteID == site) |> arrange(date)


#----trained predictions
dat$pred <- predict(mod, newdata = dat, type = "response")
samp <- dat |> filter(siteID == site) |> filter(year < 2025)

plot(samp$date, samp$pred,
     type = "l",
     col = "red",
     lwd = 2,
     xlab = "Date",
     ylab = "Predicted count",
     ylim = range(samp$pred, samp$count,
                  b25$pred, b25$count),
     xlim = range(samp$date, b25$date),
     main = site)
points(samp$date, samp$count, pch = 16)
lines(samp$date, samp$count, lty =2)


lines(b25$date, b25$pred,
     type = "l",
     lwd = 2,
     col = "blue",
     xlab = "Date",
     ylab = "Predicted count")

#---observed

points(obsv$collectDate, obsv$count, pch = 16)
lines(obsv$collectDate, obsv$count, lty =2)






#--------------ALL SITES
par(
  mfrow = c(2,3),
  mar = c(3,3,3,1),
  oma = c(1,1,1,1)
)



for(site in sites){
  #----observed
  obsv <- deer |>
    filter(siteID == site) |>
    filter(year == 2025)
  
  #----test predictions
  b25 <- kat |> filter(siteID == site) |> arrange(date)
  
  
  #----trained predictions
  dat$pred <- predict(mod, newdata = dat, type = "response")
  samp <- dat |> filter(siteID == site) |> filter(year < 2025)
  
  if(site != except){
    plot(samp$date, samp$pred,
         type = "l",
         col = "red",
         lwd = 2,
         xlab = "Date",
         ylab = "Predicted count",
         ylim = range(samp$pred, samp$count,
                      b25$pred, obsv$count),
         xlim = range(samp$date, b25$date),
         main = site)
    points(samp$date, samp$count, pch = 16, cex = 0.7)
    lines(samp$date, samp$count, lty =2)
    
    
    lines(b25$date, b25$pred,
          type = "l",
          lwd = 2,
          col = "blue",
          xlab = "Date",
          ylab = "Predicted count")
    points(obsv$collectDate, obsv$count, pch = 16, cex = 0.7)
    lines(obsv$collectDate, obsv$count, lty =2)
  } else {
    plot(samp$date, samp$pred,
         type = "l",
         col = "gold",
         lwd = 2,
         xlab = "Date",
         ylab = "Predicted count",
         ylim = range(samp$pred, samp$count,
                      b25$pred, b25$count),
         xlim = range(samp$date, b25$date),
         main = site)
    points(samp$date, samp$count, pch = 16, cex = 0.7)
    lines(samp$date, samp$count, lty =2)
    
    
    lines(b25$date, b25$pred,
          type = "l",
          col = "gold",
          lwd = 2,
          xlab = "Date",
          ylab = "Predicted count")
    points(obsv$collectDate, obsv$count, pch = 16, cex = 0.7)
    lines(obsv$collectDate, obsv$count, lty =2)
  }
  
  
  if(site == "GREN"){
    legend(
      "topleft",
      legend = c("Observed", "Predicted (2016-24)", "Predicted (2025)", paste0(except)),
      col = c("black", "red", "blue", "gold"),
      pch = c(16, NA, NA, NA),
      lty = c(NA, 1, 1, 1),
      cex = 0.65
    )
  }
  

}





#----------------STAGE structure------------------



#======================BLAN DETOUR=============================

blan <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/BLAN_allDays.csv")
blan <- blan |>
  arrange(time) |>
  mutate(date = as.Date(time)) |>
  filter(species == "Ixodes scapularis") |>
  filter(model == "Mice & Weather") |>
  mutate(year = year(date)) |>
  filter(start.date == "2020-07-27")

obsv <- deer |>
  filter(siteID == "BLAN")


pheno <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/phenology_DOYs.csv") |>
  filter(siteID == "BLAN")

pheno$greenup_date <- as.Date(pheno$greenup - 1,
                              origin = paste0(pheno$year, "-01-01"))
pheno$mid_greenup_date <- as.Date(pheno$midgreenup - 1,
                              origin = paste0(pheno$year, "-01-01"))
pheno$maturity_date <- as.Date(pheno$maturity - 1,
                              origin = paste0(pheno$year, "-01-01"))

#=====plot=========
plot(blan$date[blan$lifeStage == "Nymph"], blan$mean[blan$lifeStage == "Nymph"], type = "l")
lines(obsv$collectDate, obsv$count, col = "red")
lines(blan$date[blan$lifeStage == "Dormant"], blan$mean[blan$lifeStage == "Dormant"], col = "blue")


years <- unique(year(blan$date))

for(y in years){
  
  abline(v = pheno$greenup_date[pheno$year == y], col = "gold", lty = 2)
  abline(v = pheno$mid_greenup_date[pheno$year == y], col = "green", lty = 2)
  abline(v = pheno$maturity_date[pheno$year == y], col = "forestgreen", lty = 2)
}



#========blan years========
y <- 2021

b21 <- blan |>
  filter(year(date) == y)

o21 <- obsv |>
  filter(year == y)

plot(b21$date[b21$lifeStage == "Nymph"], b21$mean[b21$lifeStage == "Nymph"], type = "l",
     xlim = range(c(pheno$greenup_date[pheno$year == y], b21$date, o21$collectDate)),
     ylim = range(o21$count, b21$mean[b21$lifeStage == "Nymph"],
                  b21$mean[b21$lifeStage == "Dormant"]))
lines(o21$collectDate, o21$count, col = "red")
lines(b21$date[blan$lifeStage == "Dormant"], b21$mean[blan$lifeStage == "Dormant"], col = "blue")


abline(v = pheno$greenup_date[pheno$year == y], col = "gold")
abline(v = pheno$mid_greenup_date[pheno$year == y], col = "green")
abline(v = pheno$maturity_date[pheno$year == y], col = "forestgreen")


