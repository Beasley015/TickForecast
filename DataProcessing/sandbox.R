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
  filter(lifeStage == "Larva") |>
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


nlcd <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/full_NLCD.csv")
frag <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/fragstats.csv")
kat <- inner_join(kat, nlcd)
kat <- inner_join(kat, frag)

#=============TEMPORAL==============
library(terra)
library(sf)


files <- list.files("/projectnb/dietzelab/skanee/MSLP/ancillary/aspect",
                    full.names = TRUE)
path <- files[1350]
path <- "	/projectnb/dietzelab/skanee/MSLP/ancillary/aspect/aspect_17VNR.tif"

r <- terra::rast(path)

plot(r)

# center in raster CRS
x <- mean(c(xmin(r), xmax(r)))
y <- mean(c(ymin(r), ymax(r)))

# convert to lon/lat
pt <- vect(matrix(c(x, y), ncol = 2),
           type = "points",
           crs = crs(r))

crds(project(pt, "EPSG:4326"))











files <- list.files(
  "/projectnb/dietzelab/skanee/MSLP/ancillary/aspect",
  full.names = TRUE
)

results <- data.frame(
  file = character(),
  lon_min = numeric(),
  lon_max = numeric(),
  lat_min = numeric(),
  lat_max = numeric()
)

for (f in files) {
  r <- rast(f)
  
  # four corners in native CRS
  corners <- rbind(
    c(xmin(r), ymin(r)),
    c(xmin(r), ymax(r)),
    c(xmax(r), ymin(r)),
    c(xmax(r), ymax(r))
  )
  
  pts <- vect(corners,
              type = "points",
              crs = crs(r))
  
  coords <- crds(project(pts, "EPSG:4326"))
  
  results <- rbind(results, data.frame(
    file = basename(f),
    lon_min = min(coords[,1]),
    lon_max = max(coords[,1]),
    lat_min = min(coords[,2]),
    lat_max = max(coords[,2])
  ))
}

results




