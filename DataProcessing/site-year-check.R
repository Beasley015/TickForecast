library(dplyr)
library(lubridate)


ticks <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv") |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(scientificName == "Ixodes scapularis")

syear <- data.frame()

sites <- c("GREN", "HNRY", "TEA", "BLAN", "SCBI", "SERC", "ORNL", "MLBS", "TREE", "HARV")

for(site in sites){
  
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

  
  years <- min(deer$year):2025
  
  
  for(y in years){
    
    #=============filter to year y
    temp <- deer |>
      filter(year == y)
    
    if(!any(temp$count >= 5)){
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
    
    if(sum(ramp$count != 0) == 1){
      
      penultimate_interval <- if (nrow(ramp) >= 2) {
        ramp$DOY[nrow(ramp)] - ramp$DOY[nrow(ramp) - 1]
      } else {
        NA_real_
      }
      
      mean_interval <- if (nrow(ramp) >= 2) {
        mean(diff(ramp$DOY), na.rm = TRUE)
      } else {
        NA_real_
      }
      
      this <- data.frame(
        siteID = site,
        year = y,
        anomalous = TRUE,
        n = nrow(ramp),
        mean_interval = mean_interval,
        penultimate_interval = penultimate_interval,
        max_count = max(ramp$count),
        total_count = sum(ramp$count),
        first_DOY = ramp$DOY[1],
        peak_DOY = ramp$DOY[nrow(ramp)]
      )
      
      syear <- rbind(syear, this)

    } else {
      
      penultimate_interval <- if (nrow(ramp) >= 2) {
        ramp$DOY[nrow(ramp)] - ramp$DOY[nrow(ramp) - 1]
      } else {
        NA_real_
      }
      
      mean_interval <- if (nrow(ramp) >= 2) {
        mean(diff(ramp$DOY), na.rm = TRUE)
      } else {
        NA_real_
      }
      
      this <- data.frame(
        siteID = site,
        year = y,
        anomalous = FALSE,
        n = nrow(ramp),
        mean_interval = mean_interval,
        penultimate_interval = penultimate_interval,
        max_count = max(ramp$count),
        total_count = sum(ramp$count),
        first_DOY = ramp$DOY[1],
        peak_DOY = ramp$DOY[nrow(ramp)]
      )
       
      syear <- rbind(syear, this)
    }

  }
}




