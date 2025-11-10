Sys.setenv("NEONSTORE_HOME" = "/home/ebeez/Documents/TickForecast/Data/neonstore")
Sys.setenv("NEONSTORE_DB" = "/home/ebeez/Documents/TickForecast/Data/neonstore")

library(neonstore)

neon_download("DP1.10093.001")

neon_store() # FIX THIS?
tck_taxonomyProcessed <- neon_table("tck_taxonomyProcessed-basic")
tck_fielddata <- neon_table("tck_fielddata-basic")
