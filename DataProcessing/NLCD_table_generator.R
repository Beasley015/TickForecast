library(terra)
library(sf)

#unzip NLCD data
unzip("Annual_NLCD_LndCov_2021_CU_C1V1.zip", exdir = "C:/NLCD_2021")

#load raster
nlcd <- rast("C:/NLCD_2021/Annual_NLCD_LndCov_2021_CU_C1V1.tif")

#load bounding boxes
dat <- read.csv("FINAL_bounders.csv")

#define site IDs
site_ids <- c("BLAN","HARV","KONZ","LENO","OSBS","SCBI","SERC","TALL","TREE","UKFS")
rows <- c(4, 14, 19, 21, 29, 31, 32, 38, 41, 42)

#readable NLCD class names
nlcd_classes <- c(
  "11" = "open_water_pct",
  "21" = "developed_open_pct",
  "22" = "developed_low_pct",
  "23" = "developed_medium_pct",
  "24" = "developed_high_pct",
  "41" = "deciduous_forest_pct",
  "42" = "evergreen_forest_pct",
  "43" = "mixed_forest_pct",
  "52" = "shrub_pct",
  "71" = "grassland_pct",
  "81" = "pasture_pct",
  "82" = "cultivated_crops_pct",
  "90" = "woody_wetland_pct",
  "95" = "emergent_wetland_pct"
)

#create output list
out <- vector("list", length(site_ids))

for (i in seq_along(site_ids)) {
  
  #define bounding box
  ul_lat <- dat$latitude_top_left[rows[i]]
  ul_lon <- dat$longitude_top_left[rows[i]]
  lr_lat <- dat$latitude_bottom_right[rows[i]]
  lr_lon <- dat$longitude_bottom_right[rows[i]]
  
  e_ll <- ext(
    min(ul_lon, lr_lon), max(ul_lon, lr_lon),
    min(lr_lat, ul_lat), max(lr_lat, ul_lat)
  )
  
  #convert into polygon
  site_ll <- as.polygons(e_ll, crs = "EPSG:4326")
  
  #reproject to Cartesian
  site_nlcd <- project(site_ll, crs(nlcd))
  
  #crop and mask to site
  nlcd_crop <- crop(nlcd, site_nlcd)
  nlcd_mask <- mask(nlcd_crop, site_nlcd)
  
  #extract raster values
  vals <- values(nlcd_mask)
  vals <- vals[!is.na(vals)]
  
  #calculate class proportions
  tab <- table(vals)
  prop <- prop.table(tab)
  
  #create row for site
  site_row <- data.frame(siteID = site_ids[i], stringsAsFactors = FALSE)
  for (col_name in nlcd_classes) {
    site_row[[col_name]] <- 0
  }
  
  #fill in class proportions
  for (cls in names(prop)) {
    if (cls %in% names(nlcd_classes)) {
      col_name <- nlcd_classes[[cls]]
      site_row[[col_name]] <- as.numeric(prop[cls])
    }
  }
  
  out[[i]] <- site_row
}

#combine rows into final table
all_NLCD <- do.call(rbind, out)
all_NLCD