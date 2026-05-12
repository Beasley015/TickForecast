library(xml2)
library(dplyr)
library(purrr)

load("/projectnb/dietzelab/dongchen/NEON_SDA_Files/obs.mean.Rdata")
xml <- read_xml("/projectnb/dietzelab/dongchen/NEON_SDA_Files/pecan.xml")
coords <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/siteLatLon.csv")

mat <- obs.mean[[1]][["1000004944"]]


site_nodes <- xml_find_all(xml, "//run/*/site")

dat <- map_dfr(site_nodes, function(site) {
  tibble(
    siteID = xml_text(xml_find_first(site, "id")),
    lat = as.numeric(xml_text(xml_find_first(site, "lat"))),
    lon = as.numeric(xml_text(xml_find_first(site, "lon")))
  )
})

dat


dist <- function(x1, y1, x2, y2){
  sqrt((x1-x2)^2+(y1-y2)^2)
}

dat$siteID <- as.character(dat$siteID)

d <- 10000

dat$site <- NA

for (i in 1:nrow(dat)) {
  
  d_best <- Inf
  site_best <- NA
  
  for (j in 1:nrow(coords)) {
    
    d_new <- dist(dat$lat[i], dat$lon[i], coords$decimalLatitude[j], coords$decimalLongitude[j])
    
    if (d_new < d_best) {
      d_best <- d_new
      site_best <- coords$siteID[j]
    }
  }
  
  dat$site[i] <- site_best
}


dates <- as.Date(paste0(yrs, "-01-01"))
yrs <- 2012:2021
yrs <- as.character(yrs)

dat$SOC <- NA

SOC <- obs.mean[[1]]

for(r in 1:nrow(dat)){
  dat$SOC[r] <- SOC[[dat$siteID[r]]]$TotSoilCarb
}



dat <- dat |>
  group_by(site) |>
  summarize(
    SOC = mean(SOC),
    .groups = "drop"
  )




