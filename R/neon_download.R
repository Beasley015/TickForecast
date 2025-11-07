# Sys.setenv("NEONSTORE_HOME" = "/projectnb/dietzelab/fosterj/Data/neonstore")
# Sys.setenv("NEONSTORE_DB" = "/projectnb/dietzelab/fosterj/Data/neonstore")

setwd("./Data")

library(neonstore)

neon_download("DP1.10093.001", dir = getwd())

neon_store()
tck_taxonomyProcessed <- neon_table("tck_taxonomyProcessed-basic")
tck_fielddata <- neon_table("tck_fielddata-basic")
