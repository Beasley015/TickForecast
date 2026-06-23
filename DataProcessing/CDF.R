library(dplyr)
library(lubridate)

site = "SERC"

ticks <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv") |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(scientificName == "Ixodes scapularis")


deer <- ticks |>
  filter(siteID == site) |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Nymph") |>
  arrange(collectDate) |>
  group_by(collectDate) |>
  summarise(count = sum(processedCount)) 
deer$year <- year(deer$collectDate)
deer$DOY <- as.numeric(format(deer$collectDate, "%j"))

deer <- deer[deer$year > 2015, ]
deer <- na.omit(deer)


years <- min(deer$year):2024


lags <- data.frame(
  year = years,
  greenup = vector(mode = "numeric", length = length(years)),
  mid_greenup = vector(mode = "numeric", length = length(years)),
  maturity = vector(mode = "numeric", length = length(years)),
  tick_15 = vector(mode = "numeric", length = length(years))
)

i <- 1


pheno <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/phenology_DOYs.csv")
pheno <- pheno |>
  filter(siteID == site)


par(mfrow = c(2, 5))

for(y in years){

  tick <- deer |>
    filter(year == y)
  
  if(nrow(tick) < 3){
    next
  }
  
  temp <- pheno |>
    filter(year == y)

  
  
  library(MASS)
  
  
  # weigh tick DOYs by count
  tick_doy <- rep(tick$DOY, times = tick$count)
  tick_fit <- fitdistr(tick_doy, "normal")
  
  # locate percentile
  c1 <- qnorm(0.15, mean = tick_fit$estimate["mean"], sd = tick_fit$estimate["sd"]) 
  
  #lags <- c1 - c2
  d1 <- temp$greenup
  d2 <- temp$midgreenup
  d3 <- temp$maturity
  
  curve(pnorm(x, mean = tick_fit$estimate["mean"], sd = tick_fit$estimate["sd"]), col = "black", 
        from = 1, to = 250, xlab = "DOY", ylab = "CDF", main = y)
  abline(v = temp$greenup, col = "forestgreen", lty = 2)
  
  
  lags$greenup[i] <- d1
  lags$mid_greenup[i] <- d2
  lags$maturity[i] <- d3
  lags$tick_15[i] <- c1
    
  i <- i + 1
  
}



























