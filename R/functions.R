neon_tick_data <- function(species) {
	get_data <- function(i) {
		sub.df <- data |>
			filter(plotID == plots[i]) |>
			arrange(time) |>
			ungroup()

		if (all(sub.df$processedCount == 0)) {
			return(NULL)
		}

		drags <- unique(sub.df$time) |> sort()
		n.drags <- length(drags)
		days.sequence <- seq.Date(drags[1], drags[n.drags], by = 1)
		n.days <- length(days.sequence)

		df.l <- sub.df |>
			filter(lifeStage == "Larva") |>
			group_by(time, totalSampledArea) |>
			summarise(processedCount = sum(processedCount)) |>
			mutate(lifeStage = "Larva")

		df.na <- sub.df |>
			filter(lifeStage != "Larva") |>
			select(time, processedCount, totalSampledArea, lifeStage) |>
			distinct()

		counts <- bind_rows(df.l, df.na) |>
			pivot_wider(names_from = lifeStage, values_from = processedCount) |>
			mutate(
				plotID = plots[i],
				siteID = sub.df$siteID[1],
				species = species,
				nlcd = sub.df$nlcdClass[1],
				n.drags = n.drags,
				n.days = n.days,
				count.flag = nrow(df.l) * 2 == nrow(df.na)
			)
	}

	df <- read_csv(
		"data/tickTargets.csv"
	) |>
		suppressMessages()
	data <- df |>
		filter(scientificName %in% c(species, "AAorIX"))
	# time >= "2018-01-01")

	plots <- unique(data$plotID)
	n.plots <- length(plots)

	plot.info <- tibble()
	for (i in 1:n.plots) {
		plot.info <- bind_rows(plot.info, get_data(i))
	}

	return(plot.info)
}
