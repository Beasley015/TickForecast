#####################################################
# Script for creating lat/long files for site and   #
# plot levels of NEON tick data                     #
#                                                   #
# E.M. Beasley                                      #
# Spring 2026                                       #
#####################################################

# Load packages ----------------------
library(mclm)
library(tidyverse)
library(sf)

# Get raw data ------------------------
# Site names
aa.sites <- read_txt("./Data/am_sites.txt")
aa.sites <- str_remove(aa.sites, "\\*")

ix.sites <- read_txt("./Data/ix_sites.txt")
ix.sites <- str_remove(ix.sites, "\\*")

# Cary coords
cary.coords <- read.csv("./Data/cary_latlong_final.csv")[,-1] %>%
  rename(decimalLatitude = lat, decimalLongitude = long) %>%
  st_as_sf(coords=c('decimalLatitude', 'decimalLongitude'), remove = F) %>%
  st_set_crs(value = 4326) %>%
  group_by(plotID) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid(cary.coords) %>%
  mutate(siteID = str_extract(plotID, pattern = "[A-Z]+")) %>%
  mutate(decimalLongitude = sf::st_coordinates(.)[,2],
         decimalLatitude = sf::st_coordinates(.)[,1]) %>%
  st_drop_geometry()

# NEON coords
tick.data <- read_csv("./Data/tickLong.csv", show_col_types=F)
tick.plots <- tick.data %>%
  group_by(plotID) %>%
  select(plotID, decimalLatitude, decimalLongitude) %>%
  distinct() %>%
  mutate(siteID = str_extract(plotID, "[A-Z]+"))

# All coords
all.coords <- bind_rows(cary.coords, tick.plots)

# Final plot locations -----------------
coords.filtered <- all.coords %>%
  filter(siteID %in% c(aa.sites, ix.sites)) %>%
  filter(is.na(decimalLatitude)==F, is.na(decimalLongitude)==F)

# write.csv(coords.filtered, "./Data/plotLatLon.csv")

# Get site midpoints --------------------
site.coords <- coords.filtered %>%
  st_as_sf(coords=c('decimalLongitude', 'decimalLatitude')) %>%
  st_set_crs(value = 4326) %>%
  group_by(siteID) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  mutate(siteID = str_extract(siteID, pattern = "[A-Z]+")) %>%
  mutate(decimalLongitude = sf::st_coordinates(.)[,1],
         decimalLatitude = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# write_csv(site.coords, "./Data/siteLatLon.csv")


