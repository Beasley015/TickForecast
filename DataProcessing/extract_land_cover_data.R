#This script can do one of two things, assuming the user has downloaded the 2021 NLCD:
# (1): Create a dataframe that decomposes all Cary and 18 NEON sites (including the main 10) 
        # by land cover and use
# (2): Create another dataframe that contains fragstats (patch area, perimeter, edge, 
        # %-dominant land cover) and relevant tick data (time-averaged nymph, adult counts) 
        # for all Cary sites and 18 NEON sites, including the main 10

library(terra)
library(sf)


#=======================NLCD========================

# load raster (downloaded manually) and bounding boxes
nlcd <- rast("C:/NLCD_2021/Annual_NLCD_LndCov_2021_CU_C1V1.tif")
dat <- read.csv("UPDATED_FINAL_bounders.csv")

#exclude sites which the NLCD doesn't cover
non_conus <- c(
  "BARR",
  "TOOL",
  "HEAL",
  "PUUM",
  "GUAN",
  "LAJA",
  "DEJU",
  "BONA"
)
dat <- dat[!(dat$site %in% non_conus), ]

# NLCD class mapping
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

# create list of site-level classifications
out <- vector("list", nrow(dat))
list <- list()

for (i in seq_len(nrow(dat))) {
  
  ul_lat <- dat$latitude_top_left[i]
  ul_lon <- dat$longitude_top_left[i]
  lr_lat <- dat$latitude_bottom_right[i]
  lr_lon <- dat$longitude_bottom_right[i]
  
  # bounding box in lon/lat
  e_ll <- ext(
    min(ul_lon, lr_lon), max(ul_lon, lr_lon),
    min(lr_lat, ul_lat), max(lr_lat, ul_lat)
  )
  
  # polygon in EPSG:4326
  site_ll <- as.polygons(e_ll, crs = "EPSG:4326")
  
  # project to raster CRS
  site_nlcd <- project(site_ll, crs(nlcd))
  
  # crop safely
  nlcd_crop <- try(crop(nlcd, site_nlcd), silent = TRUE)
  if (inherits(nlcd_crop, "try-error")) {
    warning(paste("Crop failed for site", dat$site[i]))
    next
  }
  
  # extract values
  vals <- values(nlcd_crop)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) {
    warning(paste("No raster values for site", dat$site[i]))
    next
  }
  
  # proportions by NLCD class
  tab <- table(vals)
  prop <- prop.table(tab)
  
  # initialize row
  site_row <- data.frame(siteID = dat$site[i], stringsAsFactors = FALSE)
  
  #initialize columns
  for (col_name in unname(nlcd_classes)) {
    site_row[[col_name]] <- 0
  }
  
  # fill observed classes
  for (cls in names(prop)) {
    if (cls %in% names(nlcd_classes)) {
      col_name <- nlcd_classes[[cls]]
      site_row[[col_name]] <- as.numeric(prop[cls])
    }
  }
  
  out[[i]] <- site_row
}

# remove failed sites
out <- out[!sapply(out, is.null)]

# combine into final table
full_NLCD <- do.call(rbind, out)




#=======================fragstats========================

library(terra)
library(sf)
library(rlist)
library(landscapemetrics)
library(dplyr)


# load raster and bounding boxes
nlcd <- rast("C:/NLCD_2021/Annual_NLCD_LndCov_2021_CU_C1V1.tif")
non_conus <- c(
  "BARR",
  "TOOL",
  "HEAL",
  "DELA",
  "PUUM",
  "GUAN",
  "LAJA",
  "DEJU",
  "BONA",
  "KONA"
)
dat <- read.csv("UPDATED_FINAL_bounders.csv")
dat <- dat[!(dat$site %in% non_conus), ]



# create output list
sites <- vector("list", nrow(dat))
names(sites) <- unique(dat$site)

for (i in seq_len(nrow(dat))) {
  
  ul_lat <- dat$latitude_top_left[i]
  ul_lon <- dat$longitude_top_left[i]
  lr_lat <- dat$latitude_bottom_right[i]
  lr_lon <- dat$longitude_bottom_right[i]
  
  # bounding box in lon/lat
  e_ll <- ext(
    min(ul_lon, lr_lon), max(ul_lon, lr_lon),
    min(lr_lat, ul_lat), max(lr_lat, ul_lat)
  )
  
  # polygon in EPSG:4326
  site_ll <- as.polygons(e_ll, crs = "EPSG:4326")
  
  # project to raster CRS
  site_nlcd <- project(site_ll, crs(nlcd))
  
  # crop safely
  nlcd_crop <- try(crop(nlcd, site_nlcd), silent = TRUE)
  if (inherits(nlcd_crop, "try-error")) {
    warning(paste("Crop failed for site", dat$site[i]))
    next
  }
  
  sites[[dat$site[i]]] <- nlcd_crop
}

areas <- vector(length = 19)
perims <- vector(length = 19)

#Compute summed patch area and perimeter
for(r in 1:nrow(dat)){
  areas[r] <- sum(lsm_p_area(sites[[r]])$value, na.rm = TRUE) * 10000
}

for(r in 1:nrow(dat)){
  perims[r] <- sum(lsm_p_perim(sites[[r]])$value, na.rm = TRUE)
}

#Initialize dataframe
frag <- data.frame(
  site = names(sites),
  area_sq_m = areas,
  perimeter_m = perims,
  area_ha = areas/10000
)

#Edge = perimeter / area
frag$edge_m_per_ha <- frag$perimeter_m/frag$area_ha

#Add mean nymph counts
stages <- c("Nymph")

tick <- read.csv("tickLong.csv")
nymph <- read.csv("tickLong.csv") |>
  filter(lifeStage %in% stages) |>
  group_by(siteID) |>
  summarise(mean_nymph_count = mean(processedCount, na.rm = TRUE), .groups = "drop")


frag <- left_join(frag, nymph, by = c("site" = "siteID"))

#Add mean adult counts
stages <- c("Adult")

tick <- read.csv("tickLong.csv")
adult <- read.csv("tickLong.csv") |>
  filter(lifeStage %in% stages) |>
  group_by(siteID) |>
  summarise(mean_adult_count = mean(processedCount, na.rm = TRUE), .groups = "drop")


frag <- left_join(frag, adult, by = c("site" = "siteID"))


#load NLCD table
LC <- full_NLCD

#calculate % dominant
prop <- LC |>
  rowwise() |>
  mutate(
    pct_dominant = max(c_across(ends_with("_pct")), na.rm = TRUE),
    lc_dominant = names(pick(ends_with("_pct")))[which.max(c_across(ends_with("_pct")))]) |>
  ungroup() |>
  select(siteID, pct_dominant, lc_dominant)

frag <- left_join(frag, prop, by = c("site" = "siteID"))
