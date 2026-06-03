# Script to wrangle mouse capture data from Cary
# into a format that matches NEON

# Output .csv is an input for the script capture_matrix.R

library(tidyverse)

cary.raw <- read.csv("./Data/MNA.PL.450m2.by week.2025.csv") 

cary.save <- cary.raw %>%
  # Extract site and plot names
  mutate(siteID = case_when(str_detect(grid, "Green") ~ "GREN",
                          str_detect(grid, "Henry") ~ "HNRY",
                          str_detect(grid, "Tea") ~ "TEA",
                          TRUE ~ NA)) %>%
  mutate(plot = case_when(str_detect(grid, "C") ~ "001",
                          str_detect(grid, "E") ~ "002",
                          TRUE ~ NA)) %>%
  unite("plotID", siteID, plot, sep = "_", remove=F) %>%
  select(-c(grid, plot)) %>%
  
  # Rename data column to match neon
  rename("collectDate" = "date_start") %>%
  mutate(collectDate = as.Date(collectDate, format = "%m/%d/%y")) %>%
  
  # Remove other columns
  select(-c(year))

write_csv(cary.save, "./Data/cary_mouse_formatted.csv")  
