library(dplyr)
library(lubridate)
library(MASS)

site = "BLAN"

#===============tick data
ticks <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv") |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(scientificName == "Ixodes scapularis") |>
  filter(lifeStage == "Nymph")

deer <- ticks |>
  filter(siteID == site) |>
  arrange(collectDate) |>
  group_by(collectDate) |>
  summarise(count = sum(processedCount)) 
deer$year <- year(deer$collectDate)
deer$DOY <- as.numeric(format(deer$collectDate, "%j"))

deer <- deer[deer$year > 2015, ]
deer <- na.omit(deer)


#===========leaf phenology DOYs
pheno <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/phenology_DOYs.csv")
pheno <- pheno |>
  filter(siteID == site)


years <- min(deer$year):2024



#============START HERE
lags <- data.frame(
  year = years,
  greenup = vector(mode = "numeric", length = length(years)),
  mid_greenup = vector(mode = "numeric", length = length(years)),
  maturity = vector(mode = "numeric", length = length(years)),
  tick_15 = vector(mode = "numeric", length = length(years))
)

# MLE function & starting parameters
estim <- function(theta, data){
  mu <- theta[1]
  sigma <- theta[2]
  
  p <- pnorm(data$DOY, mu, sigma)
  
  fitted <- p * max(data$count)
  
  return(-sum(dpois(data$count, fitted, log = TRUE)))
}

start <- c(120, 10)

i <- 1

par(
  mfrow = c(2,3),
  mar = c(3,3,3,1),
  oma = c(1,1,1,1)
)

for(y in years){

  #=============filter to year y
  temp <- deer |>
    filter(year == y)
  
  if(!any(temp$count >= 5)){
    lags$greenup[i] <- NA
    lags$mid_greenup[i] <- NA
    lags$maturity[i] <- NA
    lags$tick_15[i] <- NA
    i <- i+1
    next
  }
  
  #=============Locate first peak 
  local_max <- which(
    temp$count > c(-Inf, head(temp$count, -1)) &
      temp$count >= c(tail(temp$count, -1), -Inf)
  )
  first_peak <- local_max[which(temp$count[local_max] >= 5)[1]]
  peak_DOY <- temp$DOY[first_peak]
  
  #=============Find ramp + locate 15th percentile
  ramp <- temp |>
    filter(DOY <= peak_DOY) |>
    mutate(
      cum_count = cumsum(count),
      total = sum(count),
      prop = cum_count/total
    )
  
  #if(sum(ramp$count != 0) == 1){
    #lags$greenup[i] <- NA
    #lags$mid_greenup[i] <- NA
    #lags$maturity[i] <- NA
    #lags$tick_15[i] <- NA
    #i <- i+1
    #next
  #}
  
  # weigh tick DOYs by count
  fit <- optim(start, fn = estim, data = ramp)
  
  mu <- fit$par[1]
  sigma <- fit$par[2]
  
  #locate percentile 
  c1 <- qnorm(0.15, mean = mu, sd = sigma)
  
  
  #===========leaf phenology
  d1 <- pheno$greenup[pheno$year == y]
  d2 <- pheno$midgreenup[pheno$year == y]
  d3 <- pheno$maturity[pheno$year == y]
  
  #==========plot
  curve(pnorm(x, mean = fit$par[1], sd = fit$par[2]), col = "black", 
        from = min(d1, min(ramp$DOY)), to = max(d3, ramp$DOY), xlab = "DOY", ylab = "CDF", main = paste0(site, " ", y))
  abline(v = d1, col = "red", lty = 2)
  abline(v = d2, col = "forestgreen", lty = 2)
  abline(v = d3, col = "blue", lty = 2)
  abline(v = c1, col = "gold", lty = 2)
  abline(v = peak_DOY, lty = 2)
  
  
  lags$greenup[i] <- d1
  lags$mid_greenup[i] <- d2
  lags$maturity[i] <- d3
  lags$tick_15[i] <- c1
    
  i <- i + 1
  
}




#=========filter to year
y <- 2023

temp <- deer |>
  filter(year == y)


#========convert to dates
greenup <- as.Date(pheno$greenup[pheno$year == y] - 1,
                        origin = paste0(y, "-01-01"))

mid_greenup <- as.Date(pheno$midgreenup[pheno$year == y] - 1,
                       origin = paste0(y, "-01-01"))

maturity <- as.Date(pheno$maturity[pheno$year == y] - 1,
                    origin = paste0(y, "-01-01"))


#=============Locate first peak 
local_max <- which(
  temp$count > c(-Inf, head(temp$count, -1)) &
    temp$count >= c(tail(temp$count, -1), -Inf)
)


first_peak <- local_max[which(temp$count[local_max] >= 5)[1]]
peak_date <- temp$collectDate[first_peak]
peak_DOY <- temp$DOY[first_peak]


#===============Locate tick emergence
ramp <- temp |>
  filter(collectDate <= peak_date)



# clip to first peak
ramp <- temp |>
  filter(collectDate <= peak_date) |>
  mutate(
    cum_count = cumsum(count),
    total = sum(count)
  )





# obtain fit
mu <- (ramp$DOY[first_peak-1] + ramp$DOY[first_peak])/2

kiyoung <- function(theta, data){
  sigma <- theta[1]
  
  p <- pnorm(data$DOY, mu, sigma)
  
  fitted <- p * max(data$count)
  
  return(-sum(dpois(data$count, fitted, log = TRUE)))
}

start <- c(0.5)

fit <- optim(par = start, 
             fn = kiyoung, 
             data = ramp, 
             method = "Brent",
             lower = 0,
             upper = 10)

# Diagnose fit
sigma <- fit$par[1]

ramp$fitted <- pnorm(ramp$DOY, mu, sigma) * max(ramp$count)

plot(ramp$DOY, ramp$count, pch = 16,
     xlab = "DOY", ylab = "Count")

lines(ramp$DOY, ramp$fitted, col = "red", lwd = 2)



# locate tick emergence DOY
c1 <- qnorm(0.15, mean = mu, sd = sigma)


d1 <- as.Date(c1 - 1,
              origin = paste0(y, "-01-01"))





plot(temp$collectDate, temp$count, 
     type = "l", 
     main = paste0(site, " ", y), 
     ylab = "Ixodes nymph count", 
     xlab = "Date",
     xlim = range(temp$collectDate, greenup, mid_greenup,
                  maturity, d1, peak_date))
abline(v = greenup, col = "red", lty = 2)
abline(v = mid_greenup, col = "forestgreen", lty = 2)
abline(v = maturity, col = "blue", lty = 2)
abline(v = d1, col = "gold", lty = 2)
abline(v = peak_date, lty = 2)


legend(
  "topright",
  legend = c("greenup", "mid-greenup", "maturity", "15th percentile", "first peak"),
  col = c("red", "forestgreen", "blue", "gold", "black"),
  lty = 2,
  cex = 0.7
)

#=======plot CDF
curve(pnorm(x, mean = mu, sd = sigma), col = "black", 
      from = min(pheno$greenup[pheno$year == y], min(ramp$DOY)), 
      to = max(pheno$maturity[pheno$year == y], ramp$DOY), 
      xlab = "DOY", 
      ylab = "CDF", 
      main = paste0(site, " ", y))
abline(v = pheno$greenup[pheno$year == y], col = "red", lty = 2)
abline(v = pheno$midgreenup[pheno$year == y], col = "forestgreen", lty = 2)
abline(v = pheno$maturity[pheno$year == y], col = "blue", lty = 2)
abline(v = c1, col = "gold", lty = 2)
abline(v = peak_DOY, lty = 2)

legend(
  "topleft",
  legend = c("greenup", "mid-greenup", "maturity", "15th percentile", "first peak"),
  col = c("red", "forestgreen", "blue", "gold", "black"),
  lty = 2,
  cex = 0.8
)


ramp$cum_count <- cumsum(ramp$count)
ramp$total <- sum(ramp$count)

ramp$ecdf <- ramp$cum_count / ramp$total

lines(ramp$DOY, ramp$ecdf, type = "l", col = "orange")
lines(ramp$DOY, ramp$ecdf,
               type = "s",
               col = "orange",
               lwd = 2)