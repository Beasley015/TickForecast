# this script is for processing neon tick data
# download with neonstore
# clean data into usable csv


library(tidyverse)
library(lubridate)
library(uuid)

# NEONSTORE_HOME, NEONSTORE_DB, and NEON_TOKEN defined in .Renviron
Sys.setenv(NEONSTORE_DB = "/home/ebeez/Documents/TickForecast/Data/neonstore")
Sys.setenv(NEONSTORE_HOME = "/home/ebeez/Documents/TickForecast/Data/neonstore")
library(neonstore)

# Last check 2025-11-10
# latest taxon id: 2020-10-06
product <- "DP1.10093.001"
# neon_download(product = product) # Un-comment to redownload

# nindex <- neon_index()
# ntables <- nindex$table %>% unique()

tick.field.raw <- neon_read("tck_fielddata-basic", keep_filename = TRUE)
tick.taxon.raw <- neon_read("tck_taxonomyProcessed-basic")

# Start with neon data -----------------
# there are lots of reasons why sampling didn't occur (logistics, too wet, too cold, etc.)
# so, keep records when sampling occurred
tick.field <- tick.field.raw %>% 
  filter(totalSampledArea > 0) %>% 
  mutate(time = floor_date(collectDate, unit = "day")) %>% 
  unite(namedLocation, time, col = "occasionID", sep = "_")


tick.taxon.wide <- tick.taxon.raw %>% 
  filter(sampleCondition == "OK") %>% # remove taxonomy samples with quality issues
  mutate(sexOrAge = if_else(sexOrAge == "Female" | sexOrAge == "Male", 
                            "Adult",     # convert to Adult
                            sexOrAge),
         time = floor_date(collectDate, unit = "day")) %>% 
  unite(namedLocation, time, col = "occasionID", sep = "_") %>% 
  pivot_wider(id_cols = occasionID, # make wide by species and life stage
              names_from = c(acceptedTaxonID, sexOrAge),
              values_from = individualCount, 
              names_sep = "_",
              values_fn = {sum}, # duplicates occur because of Adults that where F/M - add them 
              values_fill = 0)

# Clean Cary data -------------
cary.tick <- read.csv("./Data/Ticks2006_2021.csv", sep = ",") %>%
  # Rename some cols
  rename("IXOSCA_Adult" = "Adults",
         "IXOSCA_Nymph" = "Nymphs",
         "IXOSCA_Larva" = "Larvae") %>%
  # Create "site" and "plot" columns 
  mutate(site = case_when(str_detect(Grid, "Green") ~ "GREN",
                          str_detect(Grid, "Henry") ~ "HNRY",
                          str_detect(Grid, "Tea") ~ "TEA",
                          TRUE ~ NA)) %>%
  mutate(plot = case_when(str_detect(Grid, "Control") ~ "001",
                          str_detect(Grid, "Experimental") ~ "002",
                          TRUE ~ NA)) %>%
  # Merge site and plot to match neon names
  unite("Loc", site, plot, sep = "_") %>%
  # Add date to match neon names
  unite("occasionID", Loc, Date, sep = "_") %>%
  dplyr::select(-Grid)

# May need to get lat/long, etc. from elsewhere and clean here
# Match format to tick.field and add

# Add Cary data to Neon dataframe -------------
tick.taxon.wide <- bind_rows(tick.taxon.wide, cary.tick)
tick.taxon.wide[is.na(tick.taxon.wide)] <- 0

# Final processing -------------
tick.joined <- left_join(tick.taxon.wide, tick.field, by = "occasionID") %>% 
  select(-NA_NA, -geodeticDatum, -samplingImpractical, -targetTaxaPresent,
         -adultCount, -nymphCount, -larvaCount, -samplingProtocolVersion, 
         -measuredBy, -sampleCode, -biophysicalCriteria, -plotType) %>%
  mutate(siteID = str_extract(occasionID, "([A-Z]+)")) %>%
  mutate(plotID = str_extract(occasionID, "([A-Z]+)_[\\d]+")) 

# all the species column names
spp.cols <- tick.joined %>% 
  select(contains("Larva"), contains("Nymph"), contains("Adult")) %>% 
  colnames()

# get matching taxon ids
taxon.ids <- tick.taxon.raw %>%
  filter(!is.na(acceptedTaxonID)) %>% 
  select(acceptedTaxonID, scientificName, taxonRank) %>% 
  distinct() 

# make longer
tick.long <- tick.joined %>% 
  pivot_longer(cols = all_of(spp.cols), 
               names_to = "taxonAge",
               values_to = "processedCount",
               values_drop_na = TRUE) %>% 
  separate(col = taxonAge, into = c("acceptedTaxonID", "lifeStage"), sep = "_")

# add taxon ids
tick.long <- left_join(tick.long, taxon.ids, by = "acceptedTaxonID") 

tick.out <- tick.long %>% 
  filter(acceptedTaxonID %in% c("AMBAME", "AMBSPP", "AMBSP", "IXOSP", "IXOSCA", "IXOSP2", "IXOSPP")) %>% 
         # siteID %in% c("BLAN", "HARV", "KONZ", "LENO", "ORNL", "OSBS", 
         #               "SCBI", "SERC", "TALL", "TREE", "UKFS")) %>% 
  mutate(time = floor_date(collectDate, unit = "day"),
         time = ymd(time)) %>% 
  select(-collectDate, -uid, -sampleID, -eventID)

# Use the following to add missing data for Cary sites:
tick.out <- mutate(tick.out, 
                   time = str_extract(tick.out$occasionID, 
                                      pattern = "[\\d]+-[\\d]+-[\\d]+")) %>%
  mutate(time = as.Date(time, format = "%Y-%m-%d")) %>%
  mutate(nlcdClass = case_when(siteID %in% c("HNRY", "TEA", "GREN") ~ "Forest",
                               TRUE ~ nlcdClass))

larva <- tick.out %>% 
  filter(lifeStage == "Larva")
nymph <- tick.out %>% 
  filter(lifeStage == "Nymph",
         taxonRank == "species")
adult <- tick.out %>% 
  filter(lifeStage == "Adult",
         taxonRank == "species")

target.ls <- bind_rows(larva, nymph, adult) %>% distinct()

nlcd.tb <- target.ls %>% 
  select(plotID, nlcdClass) %>% 
  distinct()

cols.keep <- target.ls %>% 
  select(occasionID, nlcdClass, time, siteID, plotID, totalSampledArea, lifeStage) %>% 
  distinct()

df.larva.sum <- target.ls %>% 
  filter(lifeStage == "Larva") %>% 
  mutate(scientificName = if_else(acceptedTaxonID == "AMBSPP",
                                  "Amblyomma americanum", 
                                  scientificName),
         scientificName = if_else(acceptedTaxonID %in% c("IXOSP", "IXOSP2", "IXOSPP"), 
                                  "AAorIX", 
                                  scientificName)) %>% 
  group_by(occasionID, scientificName, lifeStage) %>% 
  summarise(processedCount = sum(processedCount)) 

df.nymph.adult <- target.ls %>% 
  filter(lifeStage != "Larva") %>% 
  select(occasionID, scientificName, lifeStage, processedCount)

df.all.stages <- bind_rows(df.larva.sum, df.nymph.adult)
df <- left_join(df.all.stages, cols.keep, by = c("occasionID", "lifeStage"))


# df.standard <- df %>%
#   group_by(siteID, plotID, time, scientificName, lifeStage) %>% 
#   summarise(totalCount = sum(processedCount),
#             totalArea = sum(totalSampledArea),
#             standardCount = totalCount / totalArea * 1600)
# target <- left_join(df.standard, nlcd.tb, by = "plotID")

target <- df 

write_csv(tick.joined, "./Data/tickWide.csv")
write_csv(tick.long, "./Data/tickLong.csv")
write_csv(taxon.ids, "./Data/tickTaxonID.csv")
write_csv(target, "./Data/tickTargets.csv")


