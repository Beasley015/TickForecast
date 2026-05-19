#This script generates dataframes for a *particular* site's LAI, NDVI, and EVI, 
#with the latter two condensed into one block of code since they derive from the 
#same data product.


#===============================LAI==============================

library(terra)
library(sf)

outdir <- "C:/UKFS" # path specific to my machine, in this 
# case the directory containing LAI data for the UKFS site
files <- list.files(outdir, pattern="\\.hdf$", full.names=TRUE)

# obtain desired product
get_layer <- function(hdf, pattern) {
  srcs <- sources(sds(hdf))
  src  <- srcs[grepl(pattern, srcs, ignore.case=TRUE)][1]
  if (is.na(src)) stop("No subdataset matching '", pattern, "' in ", basename(hdf))
  rast(src)
}

# build site bounding box 
dat <- read.csv("FINAL_bounders.csv")
ul_lat <- dat$latitude_top_left[42] #coordinates for UKFS site
ul_lon <- dat$longitude_top_left[42]
lr_lat <- dat$latitude_bottom_right[42]
lr_lon <- dat$longitude_bottom_right[42]

e_ll <- ext(
  min(ul_lon, lr_lon), max(ul_lon, lr_lon),
  min(lr_lat, ul_lat), max(lr_lat, ul_lat)
)
site_ll <- as.polygons(e_ll, crs="EPSG:4326")

# parse dates from filenames (YYYYDOY)
doy  <- sub("^.*\\.A(\\d{7})\\..*$", "\\1", basename(files))
year <- substr(doy, 1, 4)
j1   <- as.Date(paste0(year, "-01-01"))
dates <- j1 + (as.integer(substr(doy, 5, 7)) - 1)

# create dataframe 
lai <- data.frame(
  date = dates,
  lai_mean = NA_real_,
  lai_median = NA_real_,
  lai_sd = NA_real_,
  n_pixels = NA_real_
)

#set minimum number of pixels for a layer to be counted 
min_pixels <- 5

# grab LAI data from each file and store it in lai
for (i in seq_along(files)) {
  cat(i, "/", length(files), " ", basename(files[i]), "\n")
  
  #extract LAI and quality control layer
  lai_r <- get_layer(files[i], "Lai_500m")
  qc_r  <- get_layer(files[i], "FparLai_QC")
  
  #switch bounding box to CRS of raster
  site_sinu <- project(site_ll, crs(lai_r))
  
  #crop LAI and quality control rasters to bounding box 
  lai_r <- crop(lai_r, site_sinu)
  qc_r   <- crop(qc_r, site_sinu)
  
  #only include high-quality pixels 
  ok <- (qc_r %% 4) %in% c(0,1)
  lai_ok <- mask(lai_r, ok, updatevalue = NA)
  
  #extract pixel values 
  vals <- terra::extract(lai_ok, site_sinu)[,2]
  vals <- vals[!is.na(vals)]
  
  #count pixels 
  lai$n_pixels[i] <- length(vals)
  
  #compute summary statistics 
  if (length(vals) > 0) {
    lai$lai_mean[i]   <- mean(vals)
    lai$lai_median[i] <- median(vals)
    lai$lai_sd[i]     <- if (length(vals) > 1) sd(vals) else 0
  }
  
  if (!is.na(lai$n_pixels[i]) && lai$n_pixels[i] < min_pixels) {
    lai$lai_mean[i]   <- NA_real_
    lai$lai_median[i] <- NA_real_
    lai$lai_sd[i]     <- NA_real_
  }
  
  #remove objects from environment
  rm(lai_r, qc_r, lai_ok, ok, site_sinu, vals)
}

# order 
lai <- lai[order(lai$date), ]


# clean up lai  
lai <- lai[!is.na(lai$lai_mean) & !is.na(lai$lai_sd) & !is.na(lai$n_pixels), ]






#========================NDVI & EVI==========================
#for EVI extraction, some additional legwork is required to update the column
#names; besides the line (167) where we select our data product, however, nothing 
#else changes 

outdir <- "C:/UKFS_VI" # path specific to my machine, in this 
# case the directory containing VI data for the UKFS site
files <- list.files(outdir, pattern="\\.hdf$", full.names=TRUE)

# obtain desired product
get_layer <- function(hdf, pattern) {
  srcs <- sources(sds(hdf))
  src  <- srcs[grepl(pattern, srcs, ignore.case=TRUE)][1]
  if (is.na(src)) stop("No subdataset matching '", pattern, "' in ", basename(hdf))
  rast(src)
}

# build site polygon 
dat <- read.csv("FINAL_bounders.csv")
ul_lat <- dat$latitude_top_left[42] #coordinates for UKFS site
ul_lon <- dat$longitude_top_left[42]
lr_lat <- dat$latitude_bottom_right[42]
lr_lon <- dat$longitude_bottom_right[42]

e_ll <- ext(
  min(ul_lon, lr_lon), max(ul_lon, lr_lon),
  min(lr_lat, ul_lat), max(lr_lat, ul_lat)
)
site_ll <- as.polygons(e_ll, crs="EPSG:4326")

# parse dates from filenames (YYYYDOY)
doy  <- sub("^.*\\.A(\\d{7})\\..*$", "\\1", basename(files))
year <- substr(doy, 1, 4)
j1   <- as.Date(paste0(year, "-01-01"))
dates <- j1 + (as.integer(substr(doy, 5, 7)) - 1)

# create dataframe (name immaterial to data product)
ndvi <- data.frame(
  date = dates,
  ndvi_mean = NA_real_,
  ndvi_median = NA_real_,
  ndvi_sd = NA_real_,
  n_pixels = NA_real_
)

min_pixels <- 5

# grab VI data from each file and store it in ndvi
for (i in seq_along(files)) {
  cat(i, "/", length(files), " ", basename(files[i]), "\n")
  
  ndvi_r <- get_layer(files[i], "\"500m 16 days NDVI\"") #IMPORTANT: to switch to 
  #EVI, simply replace 'NDVI' with 'EVI'
  qc_r   <- get_layer(files[i], "\"500m 16 days VI Quality\"")
  
  site_sinu <- project(site_ll, crs(ndvi_r))
  
  ndvi_r <- crop(ndvi_r, site_sinu)
  qc_r   <- crop(qc_r, site_sinu)
  
  ok <- (qc_r %% 4) %in% c(0,1)
  ndvi_ok <- mask(ndvi_r, ok, updatevalue = NA)
  
  vals <- terra::extract(ndvi_ok, site_sinu)[,2]
  vals <- vals[!is.na(vals)] * 1e-8
  
  ndvi$n_pixels[i] <- length(vals)
  
  if (length(vals) > 0) {
    ndvi$ndvi_mean[i]   <- mean(vals)
    ndvi$ndvi_median[i] <- median(vals)
    ndvi$ndvi_sd[i]     <- if (length(vals) > 1) sd(vals) else 0
  }
  
  if (!is.na(ndvi$n_pixels[i]) && ndvi$n_pixels[i] < min_pixels) {
    ndvi$ndvi_mean[i]   <- NA_real_
    ndvi$ndvi_median[i] <- NA_real_
    ndvi$ndvi_sd[i]     <- NA_real_
  }
  
  rm(ndvi_r, qc_r, ndvi_ok, ok, site_sinu, vals)
  gc()
}

# order 
ndvi <- ndvi[order(ndvi$date), ]


# clean up ndvi & calculate standard errors 
ndvi <- ndvi[!is.na(ndvi$ndvi_mean) & !is.na(ndvi$ndvi_sd) & !is.na(ndvi$n_pixels), ]









