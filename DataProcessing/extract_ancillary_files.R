library(FedData)
library(sf)
library(terra)

dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_FINAL_bounders.csv")
r <- 1

ul_lat <- dat$latitude_top_left[r]
ul_lon <- dat$longitude_top_left[r]
lr_lat <- dat$latitude_bottom_right[r] 
lr_lon <- dat$longitude_bottom_right[r] 

site <- st_as_sfc(st_bbox(c(
  xmin = ul_lon, ymin = lr_lat,
  xmax = lr_lon, ymax = ul_lat), 
  crs = 4326))



#===========================HLS rasters

files <- list.files("/projectnb/dietzelab/neochatt/MSLP/HLS/GREN", full.names = TRUE)
path <- files[14]
rast_native <- terra::rast(path)   # keep og

# crop NED to HLS tile
rast_ll <- terra::project(rast_native, "EPSG:4326")
e <- ext(rast_ll)

bbox <- st_as_sfc(st_bbox(c(
  xmin = xmin(e),
  ymin = ymin(e),
  xmax = xmax(e),
  ymax = ymax(e)
), crs = 4326))

dem <- get_ned(
  template = bbox,
  label = "GREN",
  res = 1,
  force.redo = TRUE
)

# force dem onto HLS grid
dem_hls <- terra::project(dem, rast_native, method = "bilinear")


# (extract control)
files2 <- list.files("/projectnb/dietzelab/ccmmf/data_phen/dem/dem", full.names = TRUE) 
path2 <- files2[30] 
control <- terra::rast(path2)

datatype(control)


#------slope and aspect
slope_r <- terra::terrain(dem_hls, v = "slope", unit = "degrees")
aspect_r <- terra::terrain(dem_hls, v = "aspect", unit = "degrees")

slope_path <- list.files("/projectnb/dietzelab/ccmmf/data_phen/dem/slope", full.names = TRUE)[30] 
aspect_path <- list.files("/projectnb/dietzelab/ccmmf/data_phen/dem/aspect", full.names = TRUE)[30] 

slope_control <- terra::rast(slope_path)
aspect_control <- terra::rast(aspect_path)



#reverse engineer 

dem <- control

slope_rad  <- terrain(dem, v = "slope",  unit = "radians")
aspect_rad <- terrain(dem, v = "aspect", unit = "radians")

slope_test  <- round(slope_rad * 10000)
aspect_test <- round(aspect_rad * 10000)





#=================Plotting
plot(rast_ll)

cent <- st_centroid(site)
#pinpoint site 
plot(cent, add = TRUE, pch = 16, col = "red", cex = 3)

legend(
  "topleft",
  inset = c(0.08, 0),
  legend = "Location of site",
  pch = 16,
  pt.cex = 2,
  col = "red"
)
plot(dem_hls)










