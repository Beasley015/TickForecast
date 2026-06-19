library(MODISTools)
library(dplyr)
library(purrr)
library(tidyr)

dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_FINAL_bounders.csv")

# ==========1. Compute centroid for each site 
site_coords <- dat %>%
  mutate(
    lat = (latitude_top_left + latitude_bottom_right) / 2,
    lon = (longitude_top_left + longitude_bottom_right) / 2
  ) %>%
  select(siteID, lat, lon)

# ===========2. Extraction function 
bands_needed <- c("Greenup.Num_Modes_01",
                  "MidGreenup.Num_Modes_01",
                  "Maturity.Num_Modes_01")

extract_phenology <- function(siteID, lat, lon) {
  mt_subset(
    product   = "MCD12Q2",
    lat       = lat,
    lon       = lon,
    band      = bands_needed,
    start     = "2016-01-01",
    end       = "2025-12-31",
    km_lr     = 0,
    km_ab     = 0,
    site_name = siteID,
    internal  = TRUE,
    progress  = TRUE
  )
}

#=========3. Pull raw data for every site-year
phenology_raw <- pmap_dfr(
  site_coords,
  ~ extract_phenology(siteID = ..1, lat = ..2, lon = ..3)
)

#===============4. Clean up
phenology_df <- phenology_raw %>%
  filter(as.numeric(value) < 32000) %>%
  mutate(
    year       = as.integer(format(as.Date(calendar_date), "%Y")),
    date_value = as.Date(as.numeric(value), origin = "1970-01-01")
  ) %>%
  select(siteID = site, year, band, date_value) %>%
  pivot_wider(
    names_from  = band,
    values_from = date_value
  ) %>%
  rename(
    greenup    = Greenup.Num_Modes_01,
    midgreenup = MidGreenup.Num_Modes_01,
    maturity   = Maturity.Num_Modes_01
  ) %>%
  arrange(siteID, year)

phenology_df <- phenology_df %>%
  mutate(
    greenup    = as.integer(format(greenup, "%j")),
    midgreenup = as.integer(format(midgreenup, "%j")),
    maturity   = as.integer(format(maturity, "%j"))
  )

phenology_df









#====================OSBS=====================

osbs_lat_new <-  29.69483
osbs_lon_new <- -81.9368


osbs_retry <- mt_subset(
  product   = "MCD12Q2",
  lat       = osbs_lat_new,
  lon       = osbs_lon_new,
  band      = bands_needed,
  start     = "2016-01-01",
  end       = "2025-12-31",
  km_lr     = 0,
  km_ab     = 0,
  site_name = "OSBS",
  internal  = TRUE,
  progress  = TRUE
)

# Check raw values before assuming it's fixed
osbs_retry %>%
  select(calendar_date, band, value) %>%
  arrange(band, calendar_date)


#=========3. Pull raw data for every site-year
osbs_coords <- site_coords |>
  filter(siteID == "OSBS")

osbs_coords$lat <- osbs_lat_new
osbs_coords$lon <- osbs_lon_new

osbs_raw <- pmap_dfr(
  osbs_coords,
  ~ extract_phenology(siteID = ..1, lat = ..2, lon = ..3)
)


#===============4. Clean up
osbs_df <- osbs_raw %>%
  filter(as.numeric(value) < 32000) %>%
  mutate(
    year       = as.integer(format(as.Date(calendar_date), "%Y")),
    date_value = as.Date(as.numeric(value), origin = "1970-01-01")
  ) %>%
  select(siteID = site, year, band, date_value) %>%
  pivot_wider(
    names_from  = band,
    values_from = date_value
  ) %>%
  rename(
    greenup    = Greenup.Num_Modes_01,
    midgreenup = MidGreenup.Num_Modes_01,
    maturity   = Maturity.Num_Modes_01
  ) %>%
  arrange(siteID, year)

osbs_df <- osbs_df %>%
  mutate(
    greenup    = as.integer(format(greenup, "%j")),
    midgreenup = as.integer(format(midgreenup, "%j")),
    maturity   = as.integer(format(maturity, "%j"))
  )

osbs_df


pheno <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/pheno.csv")
pheno <- pheno |>
  filter(!(siteID %in% c("OSBS")))

pheno <- rbind(pheno, osbs_df)
