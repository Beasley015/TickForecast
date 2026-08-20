library(terra)
library(dplyr)

dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_FINAL_bounders.csv")
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



#------------------get chunk & rows--------------------------

num_chunks <- 48
num_pix <- ncell(template)
chunk_len <- ceiling(num_pix / num_chunks)

cell_info <- data.frame(
  global_cell = site_cells,
  chunk = pmin(ceiling(site_cells / chunk_len), num_chunks)
)

cell_info$local_row <- (
  cell_info$global_cell -
    (cell_info$chunk - 1) * chunk_len
)

cell_info
table(cell_info$chunk)

chunk_num <- unique(cell_info$chunk)

chunk_start <- (chunk_num - 1) * chunk_len + 1
chunk_end <- min(chunk_num * chunk_len, num_pix)

if (chunk_num[1] < num_chunks) {
  chunk_npix <- chunk_len
} else {
  chunk_npix <- num_pix - (num_chunks - 1) * chunk_len
}

local_rows <- cell_info$local_row


#----------------------------EVI2------------------------------

chunk_base_dir <-
  "/projectnb/dietzelab/neochatt/MSLP/work/15TYL/imageChunks"


get_site_pixels <- function(f, local_rows, chunk_npix) {
  
  v <- readRDS(f)
  
  if (length(v) %% chunk_npix != 0) {
    stop(
      "RDS length is not divisible by chunk pixel count: ",
      basename(f)
    )
  }
  
  x <- matrix(
    v,
    nrow = chunk_npix
  )
  
  blue <- x[local_rows, 2] / 1000
  red <- x[local_rows, 3] / 10000
  nir <- x[local_rows, 4] / 10000
  
  pixel_evi2 <- 2.5 * (nir - red) /
    (nir + 2.4 * red + 1)
  
  pixel_evi2[!is.finite(pixel_evi2)] <- NA_real_
  
  #evi2 <- 2.5 * (nir - red) /
    #(nir + 2.4 * red + 1)
  
  #evi2[!is.finite(evi2)] <- NA_real_
  #evi2[evi2 < 0] <- NA_real_
  
  #evi2
  data.frame(
    pixel = seq_along(local_rows),
    blue = blue,
    red = red,
    nir = nir,
    evi2 = pixel_evi2
  )
}


chunk_results <- lapply(
  split(cell_info, cell_info$chunk),
  function(ci) {
    
    chunk_num <- unique(ci$chunk)
    
    if (length(chunk_num) != 1) {
      stop("Expected exactly one chunk number")
    }
    
    chunk_start <- (chunk_num - 1) * chunk_len + 1
    chunk_end <- min(chunk_num * chunk_len, num_pix)
    chunk_npix <- chunk_end - chunk_start + 1
    
    local_rows <- ci$local_row
    
    chunk_path <- file.path(
      chunk_base_dir,
      paste0("c", chunk_num)
    )
    
    files <- sort(
      list.files(
        chunk_path,
        pattern = "\\.Rds$",
        full.names = TRUE
      )
    )
    
    message(
      "Chunk ", chunk_num,
      ": ", length(files), " files"
    )
    
    if (length(files) == 0) {
      stop("No files found in ", chunk_path)
    }
    
    setNames(
      lapply(
        files,
        get_site_pixels,
        local_rows = local_rows,
        chunk_npix = chunk_npix
      ),
      basename(files)
    )
  }
)


acquisitions <- sort(
  unique(
    unlist(
      lapply(chunk_results, names)
    )
  )
)


blueVals <- vapply(
  acquisitions,
  function(acquisition) {
    
    blue_values <- chunk_results$'25'[[acquisition]]$blue
    
    if (length(blue_values) == 0 || all(is.na(blue_values))) {
      return(NA_real_)
    }
    
    median(blue_values, na.rm = TRUE)
  },
  numeric(1)
)

redVals <- vapply(
  acquisitions,
  function(acquisition) {
    
    red_values <- chunk_results$'25'[[acquisition]]$red
    
    if (length(red_values) == 0 || all(is.na(red_values))) {
      return(NA_real_)
    }
    
    median(red_values, na.rm = TRUE)
  },
  numeric(1)
)

nirVals <- vapply(
  acquisitions,
  function(acquisition) {
    
    nir_values <- chunk_results$'25'[[acquisition]]$nir
    
    if (length(nir_values) == 0 || all(is.na(nir_values))) {
      return(NA_real_)
    }
    
    median(nir_values, na.rm = TRUE)
  },
  numeric(1)
)
  
  
  
  


#-----------------------REPACKAGE------------------

#--------------BLUE
scene <- sub("\\.Rds$", "", acquisitions)

blue <- data.frame(
  scene = scene,
  sensor = sub("^HLS_([^_]+)_.*$", "\\1", scene),
  tile = sub("^HLS_[^_]+_([^_]+)_.*$", "\\1", scene),
  year_doy = sub("^.*_([0-9]{7})$", "\\1", scene),
  blue = blueVals
)

blue$year <- as.integer(substr(blue$year_doy, 1, 4))
blue$doy  <- as.integer(substr(blue$year_doy, 5, 7))

blue$date <- as.Date(
  blue$doy - 1,
  origin = paste0(blue$year, "-01-01")
)

blue <- blue[order(blue$date), ]
row.names(blue) <- NULL

blue <- na.omit(blue)

#--------------RED

red <- data.frame(
  scene = scene,
  sensor = sub("^HLS_([^_]+)_.*$", "\\1", scene),
  tile = sub("^HLS_[^_]+_([^_]+)_.*$", "\\1", scene),
  year_doy = sub("^.*_([0-9]{7})$", "\\1", scene),
  red = redVals
)

red$year <- as.integer(substr(red$year_doy, 1, 4))
red$doy  <- as.integer(substr(red$year_doy, 5, 7))

red$date <- as.Date(
  red$doy - 1,
  origin = paste0(red$year, "-01-01")
)

red <- red[order(red$date), ]
row.names(red) <- NULL

red <- na.omit(red)

#--------------NIR
nir <- data.frame(
  scene = scene,
  sensor = sub("^HLS_([^_]+)_.*$", "\\1", scene),
  tile = sub("^HLS_[^_]+_([^_]+)_.*$", "\\1", scene),
  year_doy = sub("^.*_([0-9]{7})$", "\\1", scene),
  nir = nirVals
)

nir$year <- as.integer(substr(nir$year_doy, 1, 4))
nir$doy  <- as.integer(substr(nir$year_doy, 5, 7))

nir$date <- as.Date(
  nir$doy - 1,
  origin = paste0(nir$year, "-01-01")
)

nir <- nir[order(nir$date), ]
row.names(nir) <- NULL

nir <- na.omit(nir)


evi2 <- red %>%
  dplyr::select(scene, date, year, doy, red) %>%
  dplyr::inner_join(
    nir %>% dplyr::select(scene, nir),
    by = "scene"
  ) %>%
  dplyr::inner_join(
    blue %>% dplyr::select(scene, blue),
    by = "scene"
  )

evi2 <- evi2[evi2$red < 1, ]

evi2$evi2 <- 2.5 * (evi2$nir - evi2$red) / (evi2$nir + 2.4 * evi2$red + 1)


#-------------------COMPARE---------------

evi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/MODIS_site_VIs.csv") |>
  filter(siteID == "TREE")

evi$date <- as.Date(evi$date)







#------------------DESPIKE--------------------------





fit <- readRDS("CHECK_THIS_FIT.rds")







