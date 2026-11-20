library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# Use any original 18TXM raster as the spatial template.
# Fmask is convenient because it has the same grid as the processed bands.
template_file <- list.files(
  "/projectnb/dietzelab/neochatt/MSLP/input/HLS30/19TBG/images/HLS.L30.T19TBG.2013106T152855.v2.0",
  pattern = "Fmask\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)[1]

template <- rast(template_file)



spat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/250m.csv")



for(r in 1:nrow(spat)){
  
  
  xmin <- spat$longitude_top_left[r]
  xmax <- spat$longitude_bottom_right[r]
  ymin <- spat$latitude_bottom_right[r]
  ymax <- spat$latitude_top_left[r]
  
  bb <- ext(xmin, xmax, ymin, ymax)
  
  # Convert to a polygon
  bb_poly <- as.polygons(bb, crs = "EPSG:4326")
  
  # Project to the HLS CRS
  bb_poly <- project(bb_poly, crs(template))
  
  # Get all raster cells intersecting the box
  site_cells <- cells(template, bb_poly)[, "cell"]
  
  saveRDS(site_cells, paste0("/usr4/ugrad/neochatt/TickForecast/MSLSP/cells/", spat$siteID[r], "_250m_cells.rds"))
}


files <- list.files(
  "/usr4/ugrad/neochatt/TickForecast/MSLSP/cells",
  full.names = TRUE
)

for (f in files) {
  if (grepl("m", basename(f), fixed = TRUE) && !dir.exists(f)) {
    print(f)
    file.remove(f)
  }
}




for(r in 1:nrow(spat)){
  text <- "19TBG"
  
  # Create and write to the text file
  writeLines(text, paste0("/usr4/ugrad/neochatt/TickForecast/MSLSP/SCC/tileLists/", spat$siteID[r], "_250m.txt"))
}
















