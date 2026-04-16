#This script plots mean NDVIs for a given site and stores 
#them in a dataframe 

library(terra)
library(sf)

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

# create dataframe 
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
  
  ndvi_r <- get_layer(files[i], "\"500m 16 days NDVI\"")
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
out3 <- ndvi[!is.na(ndvi$ndvi_mean) & !is.na(ndvi$ndvi_sd) & !is.na(ndvi$n_pixels), ]
out3$ndvi_se <- out3$ndvi_sd / sqrt(out3$n_pixels)

# order
ord <- order(out3$date)

# scatterplot
plot(out3$date, out3$ndvi_mean,
     pch = 16, cex = 0.6,
     col = "limegreen",
     xlab = "Date", ylab = "Mean NDVI",
     main = "UKFS Mean NDVI")

# 95% confidence intervals 
arrows(out3$date,
       out3$ndvi_mean - 1.96 * (out3$ndvi_se),
       out3$date,
       out3$ndvi_mean + 1.96 * (out3$ndvi_se),
       angle = 90, code = 3, length = 0.02,
       col = adjustcolor("limegreen", alpha.f = 0.25))

# loess smooth
fit <- loess(ndvi_mean ~ as.numeric(date), data = out3, span = 0.1, na.action = na.exclude)

lines(out3$date[ord], predict(fit)[ord],
      col = "forestgreen", lwd = 2)