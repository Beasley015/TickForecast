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


beta <- mega$plotID[mega$siteID == "LENO"]

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



#------------------ASSEMBLE TIME SERIES--------------------------

plots <- mega$plotID
plots <- plots[!plots %in% c("GREN_001", "GREN_002", "HNRY_001", "HNRY_002", "TEA_001", "TEA_002")]
plot <- c("OSBS_077")


NEON_EVI2 <- data.frame()

for(p in plots){
  
  
  path <- paste0("/projectnb/dietzelab/neochatt/MSLP/EVI2/", p)
  files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)
  
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
  
  
  evi2$siteID <- mega$siteID[mega$plotID == p]
  evi2 <- evi2 |> relocate(siteID)
  
  evi2$plotID <- p
  evi2 <- evi2 |> relocate(plotID)

  NEON_EVI2 <- rbind(NEON_EVI2, evi2)
  
}





evi2 <- NEON_EVI2

evi2 <- evi2 |>
  arrange(siteID)




sub <- evi2 |> filter(plotID == "SERC_012")


evi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/MODIS_site_VIs.csv") |>
  filter(siteID == "SERC")

evi$date <- as.Date(evi$date)






plot(
  sub$date,
  sub$evi2_median,
  type = "l",
  col = "black",
  xlab = "Date",
  ylab = "EVI2",
  ylim = c(0,1)
)

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





cary <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/Cary_EVI2.csv")
cary$date <- as.Date(cary$date)



full_evi2 <- rbind(cary, evi2)






library(dplyr)

# Standardize dates before joining
evi$date <- as.Date(as.character(evi$date))
full_evi2$date <- as.Date(as.character(full_evi2$date))

# Match MODIS site-level EVI with each HLS plot on exact shared dates
matched_evi <- full_evi2 |>
  dplyr::select(siteID, plotID, date, evi2_mean) |>
  dplyr::inner_join(
    evi |>
      dplyr::select(siteID, date, evi_mean),
    by = c("siteID", "date")
  ) |>
  dplyr::filter(
    complete.cases(evi_mean, evi2_mean)
  )

# Compute Pearson's R for each HLS plot
plot_correlations <- matched_evi |>
  dplyr::group_by(siteID, plotID) |>
  dplyr::summarise(
    n_shared_dates = dplyr::n(),
    R = cor(evi_mean, evi2_mean),
    .groups = "drop"
  ) |>
  dplyr::arrange(siteID, plotID)

View(plot_correlations)




prop <- mean(plot_correlations$R > 0.80, na.rm = TRUE)


prop

