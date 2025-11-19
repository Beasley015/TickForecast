# Script to wrangle mouse capture data from Cary
# into a format that matches NEON

# Output .csv is an input for the script capture_matrix.R

cary.raw <- read.csv("./Data/Cary_mouse.csv") %>%
  select(Full.Date.2, Grid, Week, Fate, Tag.., day.of.year, Spp)

cary.save <- cary.raw %>%
  # Extract site and plot names
  mutate(siteID = case_when(str_detect(Grid, "Green") ~ "GREN",
                          str_detect(Grid, "Henry") ~ "HNRY",
                          str_detect(Grid, "Tea") ~ "TEA",
                          TRUE ~ NA)) %>%
  mutate(plot = case_when(str_detect(Grid, "Control") ~ "001",
                          str_detect(Grid, "Experimental") ~ "002",
                          TRUE ~ NA)) %>%
  unite("plotID", siteID, plot, sep = "_", remove=F) %>%
  select(-c(Grid, plot)) %>%
  
  # Rename data column to match neon
  rename("collectDate" = "Full.Date.2") %>%
  
  # Rename species column to full name
  mutate(genusName = case_when(Spp == "PL" ~ "Peromyscus",
                         TRUE ~ NA)) %>%
  select(-Spp) %>%
  
  # Rename tag column
  rename(tagID = "Tag..") %>%
  
  # Cary mice have unobserved tick status
  mutate(adultTicksAttached = "U", nymphalTicksAttached = "U",
         larvalTicksAttached = "U") %>%
  
  # Re-code mouse fates to match NEON codes
  mutate(animalInTrap = case_when(Fate %in% c(1,2) ~ 1,
                                  Fate %in% c(3,4) ~ 2,
                                  TRUE ~ 0)) %>%
  select(-Fate)

write_csv(cary.save, "./Data/cary_mouse_formatted.csv")  
