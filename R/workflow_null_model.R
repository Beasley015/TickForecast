##################################################
# Master script for tick forecasting null model  #
# i.e. weekly density mean & sd                  #
# Written by J.R. Foster                         #
# Updated by E.M. Beasley                        #
# Fall 2025                                      #
##################################################

# Setup ------------------------------
# Load packages
library(tidyverse)
library(lubridate)
library(mgcv)

# Function for processing and plotting NEON tick data
source("./DataProcessing/functions.R")

dir.out <- "./out"

# Specify variables
models <- "Null"
species <- c("Ixodes scapularis", "Amblyomma americanum")
neon.sites <- c(
	"BLAN",
	"HARV",
	"KONZ",
	"LENO",
	"OSBS",
	"SCBI",
	"SERC",
	"TALL",
	"TREE",
	"UKFS"
)

cary.sites <- c(
  "GREN",
  "HNRY",
  "TEA"
)

# Create data frame of all combinations
jobs <- expand_grid(
	model = models,
	species = species,
	site = c(neon.sites, cary.sites)
)

# Some sites don't have both species
jobs <- jobs |>
	filter(
		!(site == "HARV" & species == "Amblyomma americanum"),
		!(site == "TREE" & species == "Amblyomma americanum"),
		!(site == "KONZ" & species == "Ixodes scapularis"),
		!(site == "OSBS" & species == "Ixodes scapularis"),
		!(site == "TALL" & species == "Ixodes scapularis"),
		!(site == "UKFS" & species == "Ixodes scapularis"),
		!(site == "GREN" & species == "Amblyomma americanum"),
		!(site == "HNRY" & species == "Amblyomma americanum"),
		!(site == "TEA" & species == "Amblyomma americanum")
	)

# job.num <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# if(is.na(job.num)) job.num <- 6

# Forecast is weekly mean and sd by site
gam_forecast <- function(df, ls, target_date, site.job) {
	# filter out all "future observations"
	hist <- df |>
		filter(.data$time <= target_date[1], .data$lifeStage == ls) |>
		mutate(doy = yday(time))

	b <- gam(count ~ s(doy), family = "poisson", data = hist, method = "REML")

	new_dat <- tibble(doy = yday(target_date))
	fit <- predict.gam(b, new_dat, type = "response", se.fit = TRUE)
	fx <- as.numeric(fit$fit)
	se_fit <- as.numeric(fit$se.fit)

	fit |>
		map_dfc(as.numeric) |>
		rename(fx = fit) |>
		mutate(
			lifeStage = ls,
			ymin = pmax(0, fx - (1.96 * se.fit)),
			ymax = fx + (1.96 * se.fit),
			var = fx,
			start.date = target_date[1],
			time = target_date,
			model = "Null",
			ua = "Null",
			site = site.job
		) |>
		select(-se.fit)
}

all.scores <- all.samples <- all.quants <- tibble()

for (job.num in 1:nrow(jobs)) {
	message("---------------------------------------------------")

	site.job <- jobs$site[job.num]
	species.job <- jobs$species[job.num]
	model.job <- jobs$model[job.num]

	message("site: ", site.job)
	message("species: ", species.job)

	neon.data <- neon_tick_data(species.job) |> suppressMessages()
	neon.job <- neon.data |>
		filter(siteID == site.job) |>
		arrange(time) |>
		pivot_longer(
			cols = c(Larva, Nymph, Adult),
			names_to = "lifeStage",
			values_to = "count"
		)

	drag.dates <- neon.job |>
		filter(time >= "2018-01-01" & time <= "2022-01-01") |> 
		pull(time) |>
		unique()

	for (i in seq_along(drag.dates)) {
		fx.date <- seq(drag.dates[i], drag.dates[i] + 365, by = "day")

		mm <- paste0(fx.date[1], " (", round(i / length(drag.dates) * 100, 2), "%)")
		message("\t", mm, "\n")

		# get the forecasts we want
		fx.larvae <- gam_forecast(neon.job, "Larva", fx.date, site.job)
		fx.nymphs <- gam_forecast(neon.job, "Nymph", fx.date, site.job)
		fx.adults <- gam_forecast(neon.job, "Adult", fx.date, site.job)

		df.quants <- bind_rows(
			fx.larvae,
			fx.nymphs,
			fx.adults
		)

		tick.obs <- neon.job |>
			filter(time %in% fx.date)

		quants.with.data <- left_join(
			df.quants,
			neon.job,
			by = c("time", "lifeStage")
		)

		all.quants <- bind_rows(all.quants, quants.with.data) |>
			mutate(siteID = site.job)

		pois.df <- quants.with.data |>
			filter(!is.na(count)) |>
			mutate(score = scoringRules::crps_pois(count, fx))

		all.scores <- bind_rows(all.scores, pois.df)
	}
}

dir.base <- file.path(
	dir.out,
	model.job
)

if (!dir.exists(dir.base)) {
	dir.create(dir.base, showWarnings = FALSE, recursive = TRUE)
}

write_csv(all.scores, file = file.path(dir.base, "forecastScores.csv"))
write_csv(all.quants, file = file.path(dir.base, "forecastQuants.csv"))

