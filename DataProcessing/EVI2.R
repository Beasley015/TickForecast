library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_bounding_boxes.csv")
r <- 15

# Use any original 18TXM raster as the spatial template.
# Fmask is convenient because it has the same grid as the processed bands.
template_file <- list.files(
  "/projectnb/dietzelab/neochatt/MSLP/input/HLS30/15TYL/images/HLS.L30.T15TYL.2016023T164010.v2.0",
  pattern = "Fmask\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)[1]

template <- rast(template_file)



# xmin, xmax, ymin, ymax in lon/lat
xmin <- dat$longitude_top_left[r]
xmax <- dat$longitude_bottom_right[r]
ymin <- dat$latitude_bottom_right[r]
ymax <- dat$latitude_top_left[r]

bb <- ext(xmin, xmax, ymin, ymax)


# Convert to a polygon
bb_poly <- as.polygons(bb, crs = "EPSG:4326")

# Project to the HLS CRS
bb_poly <- project(bb_poly, crs(template))

# Get all raster cells intersecting the box
site_cells <- cells(template, bb_poly)[, "cell"]


mega <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/plot_data.csv")

mega <- mega |> filter(siteID != "DELA")

for(r in 26:nrow(mega)){
  
  dirs <- list.dirs(paste0("/projectnb/dietzelab/neochatt/MSLP/input/HLS30/", mega$tile[r], "/images/"))
  
  dir <- dirs[2] 
  
  template_file <- list.files(
    dirs[2],
    pattern = "Fmask\\.tif$",
    recursive = TRUE,
    full.names = TRUE
  )[1]
  
  template <- rast(template_file)
  
  xmin <- mega$longitude_top_left[r]
  xmax <- mega$longitude_bottom_right[r]
  ymin <- mega$latitude_bottom_right[r]
  ymax <- mega$latitude_top_left[r]
  
  bb <- ext(xmin, xmax, ymin, ymax)
  
  # Convert to a polygon
  bb_poly <- as.polygons(bb, crs = "EPSG:4326")
  
  # Project to the HLS CRS
  bb_poly <- project(bb_poly, crs(template))
  
  # Get all raster cells intersecting the box
  site_cells <- cells(template, bb_poly)[, "cell"]
  
  saveRDS(site_cells, paste0("/usr4/ugrad/neochatt/TickForecast/MSLSP/cells/", mega$plotID[r], "_cells.rds"))
}


files <- list.files("/usr4/ugrad/neochatt/TickForecast/MSLSP/cells", full.names = TRUE)

for(f in files){
  
  print(readRDS(f))
  
}




for(r in 1:nrow(mega)){
  text <- c(mega$tile[r])
  
  # Create and write to the text file
  writeLines(text, paste0("/usr4/ugrad/neochatt/TickForecast/MSLSP/SCC/tileLists/", mega$plotID[r], ".txt"))
}






plots <- mega$plotID

plots <- plots[!plots %in% c("GREN_001", "GREN_002", "HNRY_001", "HNRY_002", "TEA_001", "TEA_002", "TALL_001",
                             "OSBS_077")]


beta <- mega$plotID[mega$siteID == "TALL"]

sites <- unique(mega$siteID)

for (plotID in beta) {
  system2(
    "bash",
    args = c(
      "/usr4/ugrad/neochatt/TickForecast/MSLSP/SCC/MSLSP_submitTiles_SCC.sh",
      plotID
    )
  )
}



#-------------------COMPARE---------------

evi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/MODIS_site_VIs.csv") |>
  filter(siteID == "TEA")

evi$date <- as.Date(evi$date)



#------------------ASSEMBLE TIME SERIES--------------------------


files <- list.files(path = "/projectnb/dietzelab/neochatt/MSLP/EVI2/GREN_001", pattern = "\\.csv$", full.names = TRUE)

cells <- unique(sub(
  ".*\\/(cell_[0-9]+)_year_.*",
  "\\1",
  files
))





cell_dfs <- setNames(vector("list", length(cells)), cells)

for (c in cells) {
  
  cell_files <- files[
    grepl(
      paste0(c, "_year_"),
      basename(files),
      fixed = TRUE
    )
  ]
  
  dat <- bind_rows(lapply(cell_files, read.csv))
  
  dat <- unique(dat)
  dat$date <- as.Date(dat$date)
  dat <- dat[order(dat$date), ]
  dat$cell <- c
  
  cell_dfs[[c]] <- dat
}




all_cells <- bind_rows(cell_dfs)

evi2 <- all_cells |>
  group_by(date) |>
  summarise(
    evi2_mean   = mean(evi2, na.rm = TRUE),
    evi2_median = median(evi2, na.rm = TRUE),
    evi2_sd           = sd(evi2, na.rm = TRUE),
    n_pixels     = n_distinct(cell[!is.na(evi2)]),
    .groups = "drop"
  ) |>
  arrange(date)




evi2 <- evi2 |> filter(year(date) >= 2016) |>
  filter(year(date) <= 2025)


evi2$siteID <- "HNRY"

evi2 <- evi2 |> relocate(siteID)

plot(
  evi2$date,
  evi2$evi2_median,
  type = "l",
  col = "black",
  xlab = "Date",
  ylab = "EVI2"
)

points(evi2$date, evi2$evi2_median, pch = 15, cex = 0.5)

points(evi$date, evi$evi_median, col = "green")

lines(
  evi$date,
  evi$evi_median,
  col = "green"
)

legend(
  "topright",
  legend = c("Median HLS EVI2", "Median MODIS EVI"),
  col = c("black", "green"),
  pch = c(15, 16),
  lty = 1,
  cex = 0.8
)



both <- left_join(evi, evi2, by = "date", "siteID")

both <- na.omit(both)





gren_001 <- evi2
hnry_001 <- evi2



plots <- c("GREN_001", "GREN_002", "HNRY_001", "HNRY_002", "TEA_001", "TEA_002")







cary <- data.frame()

for(p in plots){
  
  files <- list.files(path = paste0("/projectnb/dietzelab/neochatt/MSLP/EVI2/", p), pattern = "\\.csv$", full.names = TRUE)
  
  cells <- unique(sub(
    ".*\\/(cell_[0-9]+)_year_.*",
    "\\1",
    files
  ))
  
  
  cell_dfs <- setNames(vector("list", length(cells)), cells)
  
  for (c in cells) {
    
    cell_files <- files[
      grepl(
        paste0(c, "_year_"),
        basename(files),
        fixed = TRUE
      )
    ]
    
    dat <- bind_rows(lapply(cell_files, read.csv))
    
    dat <- unique(dat)
    dat$date <- as.Date(dat$date)
    dat <- dat[order(dat$date), ]
    dat$cell <- c
    
    cell_dfs[[c]] <- dat
  }
  
  
  
  
  all_cells <- bind_rows(cell_dfs)
  
  evi2 <- all_cells |>
    group_by(date) |>
    summarise(
      evi2_mean   = mean(evi2, na.rm = TRUE),
      evi2_median = median(evi2, na.rm = TRUE),
      evi2_sd           = sd(evi2, na.rm = TRUE),
      n_pixels     = n_distinct(cell[!is.na(evi2)]),
      .groups = "drop"
    ) |>
    arrange(date)
  
  
  
  
  evi2 <- evi2 |> filter(year(date) >= 2016) |>
    filter(year(date) <= 2025)
  
  
  evi2$plotID <- p
  evi2 <- evi2 |> relocate(plotID)
  
  evi2$siteID <- sub("_.*$", "", evi2$plotID)
  
  evi2 <- evi2 |> relocate(siteID)
  
  
  cary <- rbind(cary, evi2)
}


plots <- unique(cary$plotID)


cols <- setNames(rainbow(length(plots)), plots)

for (i in seq_along(plots)) {
  p <- plots[i]
  sub <- cary |> filter(plotID == p)
  
  if (i == 1) {
    plot(
      sub$date,
      sub$evi2_median,
      type = "l",
      col = cols[p],
      ylim = c(0, 1),
      xlab = "Date",
      ylab = "Median EVI2"
    )
  } else {
    lines(sub$date, sub$evi2_median, col = cols[p])
  }
}

legend(
  "topright",
  legend = plots,
  col = cols[plots],
  lty = 1,
  bty = "n"
)




#----------------FIX--------------------------

library(sf)

grid_url <- paste0(
  "https://raw.githubusercontent.com/",
  "NASA-IMPACT/hls-land_tiles/master/s2_grid.json"
)

grid_file <- "/usr4/ugrad/neochatt/TickForecast/MSLSP/s2_grid.json"

download.file(
  grid_url,
  destfile = grid_file,
  mode = "wb"
)

hls_tiles <- st_read(grid_file, quiet = TRUE)




bad_plots <- c(
  "HARV_001", "HARV_002", "HARV_004", "HARV_020", "HARV_022",
  "KONZ_001", "KONZ_002", "KONZ_004", "KONZ_007",
  "UKFS_003", "UKFS_018"
)

# Replace tickLong and coordinate names if yours differ
bad_coords <- dat |>
  filter(plotID %in% bad_plots) |>
  select(plotID, longitude_top_left, latitude_top_left) |>
  filter(!is.na(latitude_top_left), !is.na(latitude_top_left)) |>
  distinct(plotID, .keep_all = TRUE)

bad_sf <- st_as_sf(
  bad_coords,
  coords = c("longitude_top_left", "latitude_top_left"),
  crs = 4326,
  remove = FALSE
)

plot_tiles <- st_join(
  st_transform(bad_sf, st_crs(hls_tiles)),
  hls_tiles["identifier"],
  join = st_intersects
) |>
  st_drop_geometry() |>
  transmute(
    plotID,
    tile = paste0("T", sub("^T", "", identifier))
  )

plot_tiles















