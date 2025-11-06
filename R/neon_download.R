Sys.setenv("NEONSTORE_HOME" = "/projectnb/dietzelab/fosterj/Data/neonstore")
Sys.setenv("NEONSTORE_DB" = "/projectnb/dietzelab/fosterj/Data/neonstore")

library(neonstore)

neon_download("DP1.10093.001")

neon_store()
tck_taxonomyProcessed <- neon_table("tck_taxonomyProcessed-basic")
tck_fielddata <- neon_table("tck_fielddata-basic")
