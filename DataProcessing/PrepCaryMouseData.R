# Script to wrangle mouse capture data from Cary
# into a format that matches NEON

# Output .csv is an input for the script capture_matrix.R

cary.raw <- read.csv("./Data/cary_mna_1991-2022.csv") %>%
  select(Grid, Year, Week, MNA, Date, spec)

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
  rename("collectDate" = "Date") %>%
  mutate(collectDate = as.Date(collectDate, format = "%m/%d/%y")) %>%
  
  # Rename species column to full name
  filter(spec == "PL") %>%
  select(-spec) %>%
  
  # Remove other columns
  select(-c(Year, Week))

write_csv(cary.save, "./Data/cary_mouse_formatted.csv")  
