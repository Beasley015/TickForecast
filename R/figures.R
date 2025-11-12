library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)
library(utils)
library(ggpubr)
library(MetBrewer)
library(NatParksPalettes)

# Create function for saving plots
save_gg <- function(dest, gg, path) {
	if (!dir.exists(path)) {
		dir.create(path, showWarnings = FALSE, recursive = TRUE)
	}
	ggsave(
		filename = dest,
		plot = gg,
		device = "jpeg",
		path = path,
		width = 7,
		height = 5,
		units = "in",
		dpi = "retina",
		bg = "white"
	)
	# dev.off()
}

# Retrieve null model outputs-----------------
dir.out.null <- "./out/Null"
out.files.null <- list.files(dir.out.null, recursive = TRUE)
null.scores.files <- grep("Scores", out.files.null, value = TRUE)
null.quants.files <- grep("Quants", out.files.null, value = TRUE)

find_species <- function(x) {
	species <- if_else(
		grepl("Amblyommaamericanum", x),
		"Amblyomma americanum",
		"Ixodes scapularis"
	)
	species
}

null.scores <- null.quants <- tibble()
for (i in seq_along(null.scores.files)) {
	null.s <- read_csv(file.path(dir.out.null, null.scores.files[i])) %>%
		suppressMessages()
	sp <- find_species(null.scores.files[i])
	dfs <- null.s %>% mutate(species = sp)
	null.scores <- bind_rows(null.scores, dfs)

	null.q <- read_csv(file.path(dir.out.null, null.quants.files[i])) %>%
		suppressMessages()
	sp <- find_species(null.quants.files[i])
	dfq <- null.q %>% mutate(species = sp)
	null.quants <- bind_rows(null.quants, dfq)
}


df.null <- null.quants %>%
	rename(lower95 = ymin, upper95 = ymax, variance = var, median = fx) %>%
	select(-n.days, -n.drags, -count.flag, -site)

# dir.analysis <- "/projectnb/dietzelab/fosterj/FinalOut/Chapter3/analysisConstraintForestUpdate/"
# all.days <- list.files(dir.analysis, recursive = TRUE)
# all.days <- grep("allDaysQuants.csv", all.days, value = TRUE)
# all.days.proc <- read_csv(file.path(dir.analysis, all.days))

# dir.analysis <- "/projectnb/dietzelab/fosterj/FinalOut/Chapter3/analysisConstraintControl"
# all.days <- list.files(dir.analysis, recursive = TRUE)
# all.days <- grep("allDaysQuants.csv", all.days, value = TRUE)
# all.days.cont <- read_csv(file.path(dir.analysis, all.days))
#
# all.days.df <- bind_rows(
#   all.days.cont %>% mutate(parameters = "Constant"),
#   all.days.proc %>% mutate(parameters = "Updated")
# )

all.days.df <- all.days.proc

# source("Functions/neon_tick_data.R")
# neon.ix <- neon_tick_data("Ixodes scapularis") %>% suppressMessages()
# neon.aa <- neon_tick_data("Amblyomma americanum") %>% suppressMessages()
#
# data <- bind_rows(neon.ix, neon.aa) %>%
#   filter(time >= "2018-01-01") %>%
#   select(-n.drags, -n.days, -count.flag) %>%
#   pivot_longer(cols = c(Larva, Nymph, Adult),
#                names_to = "lifeStage",
#                values_to = "observed") %>%
#   mutate(density = observed / totalSampledArea)

find_ua <- function(x) {
	if (grepl("ic", x)) {
		ua <- "IC"
	}
	if (grepl("ic_parameter", x)) {
		ua <- "+ Parameter"
	}
	if (grepl("ic_parameter_driver", x)) {
		ua <- "+ Driver"
	}
	if (grepl("ic_parameter_driver_process", x)) {
		ua <- "+ Process"
	}
	ua
}

# out.files <- list.files(dir.out, recursive = TRUE)
# quantScore <- grep("fxQuantScore.csv", out.files, value = TRUE)
# length(quantScore)
# df.process <- tibble()
# for(i in seq_along(quantScore)){
#     u <- find_ua(quantScore[i])
#     dfi <- read_csv(file.path(dir.out, quantScore[i])) %>%
#     mutate(start.date = str_extract(quantScore[i], "\\d{4}-\\d{2}-\\d{2}"),
#            ua = u) %>%
#     suppressMessages()
#   df.process <- bind_rows(df.process, dfi)
#   if(i %% 500 == 0) message(i, " of ", length(quantScore), " complete ", round(i/length(quantScore)*100), "%")
# }
#
# df.mutate <- df.process %>%
#   mutate(mice = if_else(grepl("Mice", model), "Mice", "No mice"),
#          weather = if_else(grepl("Weather", model), "Weather", "No weather"))
#
# unique(df.mutate$model)
# write_csv(df.mutate, file = file.path(dir.analysis, "allFXQuants.csv"))
df.mutate <- read_csv(file = file.path(dir.analysis, "allFXQuants.csv"))

# df.control <- read_csv("/projectnb/dietzelab/fosterj/FinalOut/Chapter3/analysisConstraintControl/allFXQuants.csv")
# df.process <- read_csv("/projectnb/dietzelab/fosterj/FinalOut/Chapter3/analysisConstraint/allFXQuants.csv")
#
# df.mutate <- bind_rows(
#   df.control %>% mutate(parameters = "Constant"),
#   df.process %>% mutate(parameters = "Updated")
# )

ls.vec <- c("Larva", "Nymph", "Adult")
site.vec <- unique(df.mutate$siteID)

site.info <- neonstore::neon_sites()
df.site <- site.info %>% filter(siteCode %in% site.vec)

# time series figures -------------------------------------------------------------------
data.density <- df.mutate %>%
	select(
		time,
		lifeStage,
		siteID,
		plotID,
		species,
		totalSampledArea,
		observed
	) %>%
	distinct() %>%
	mutate(density = observed / totalSampledArea * 450)

site <- c("SERC")
ls <- "Nymph"
fx.issue.date <- all.days.df %>%
	filter(siteID == site) %>%
	pull(start.date) %>%
	unique()

i <- 11

dist.cols <- c(
	"Forecast" = "#dd5129",
	"Data" = "#0f7ba2",
	"Analysis" = "#43b284"
)

gg <- all.days.df %>%
	filter(
		ua == "+ Process",
		# parameters == "Updated",
		model == "Mice & Weather",
		siteID %in% site,
		start.date == fx.issue.date[i],
		lifeStage == ls
	) %>%
	mutate(
		ua = if_else(ua == "IC", "IC (1)", ua),
		ua = if_else(ua == "+ Parameter", "+ Parameter (2)", ua),
		ua = if_else(ua == "+ Driver", "+ Driver (3)", ua),
		ua = if_else(ua == "+ Process", "+ Process (4)", ua)
	) %>%
	mutate(
		ua = factor(
			ua,
			levels = c("IC (1)", "+ Parameter (2)", "+ Driver (3)", "+ Process (4)")
		)
	) %>%
	ggplot() +
	aes(x = time) +
	geom_ribbon(
		aes(ymin = lower95, ymax = upper95, fill = ua),
		alpha = 0.7,
		fill = "#0f7ba2"
	) +
	geom_line(aes(y = median)) +
	geom_point(
		data = data.density %>%
			filter(
				siteID == site,
				time >= fx.issue.date[i],
				time <= fx.issue.date[i] + 364,
				lifeStage == ls
			),
		aes(y = density),
		color = "#dd5129",
		size = 3
	) +
	# scale_fill_manual(values = natparks.pals("DeathValley")) +
	# coord_cartesian(ylim = c(0, 80)) +
	labs(x = "Time", y = "Ticks/450m^2", fill = "Uncertainty\nAdded") +
	# scale_y_continuous(limits = c(0, NA)) +
	facet_wrap(~species, scales = "fixed") +
	theme_pubr() +
	theme(
		legend.position = "bottom",
		axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5)
	)
gg
save_gg(
	dest = paste0("timeSeries_Control_", site, "_", ls, ".jpeg"),
	gg = gg,
	path = dir.plot
)


# score figures --------------------------------------------------------------------------

both.plots <- c("BLAN", "LENO", "SCBI", "SERC")
ix.plots <- c("TREE", "HARV")
aa.plots <- c("KONZ", "OSBS", "TALL", "UKFS")

null.crps <- null.scores %>%
	mutate(crps = score, doy = yday(time)) #%>%
	# select(-metric)

ls <- 2

df.model.names <- df.mutate %>%
	mutate(
		start.date = ymd(start.date),
		doy = yday(time),
		model = if_else(grepl("MNA", model), "Mice", model),
		model = if_else(grepl("Global", model), "Weather & Mice", model),
		model = factor(
			model,
			levels = c("Static", "Mice", "Weather", "Weather & Mice")
		)
	)

plot.sub <- ix.plots

plot_doy <- function(ua.sub, plot.sub, ls, spp) {
	rect <- tibble(
		lifeStage = c("Larva", "Nymph", "Adult"),
		xmin = c(yday("2022-07-01"), yday("2022-05-01"), yday("2022-08-01")),
		xmax = c(yday("2022-10-01"), yday("2022-08-01"), yday("2022-05-01"))
	)

	rect.use <- rect %>% filter(lifeStage == ls)
	gg <- df.model.names %>%
		filter(
			ua == ua.sub,
			time != start.date,
			# observed > 0,
			siteID %in% plot.sub,
			species %in% spp,
			# model == "Weather & Mice",
			lifeStage == ls
		) %>%
		# mutate(siteID = factor(siteID, levels = c("TREE", "HARV", "BLAN", "SCBI", "SERC", "LENO"))) %>%
		mutate(
			ua = factor(ua, levels = c("IC", "+ Parameter", "+ Driver", "+ Process"))
		) %>%
		# mutate(crps = log(crps+1)) %>%
		ggplot() +
		aes(x = doy, y = crps) +
		# geom_point() +
		geom_smooth(
			se = FALSE,
			size = 1,
			method = "loess",
			alpha = 0.6,
			aes(color = model)
		) +
		geom_smooth(
			data = null.crps %>%
				select(-ua) %>%
				mutate(
					#crps = log(crps+1),
					start.date = time
				) %>%
				filter(siteID %in% plot.sub, species %in% spp, lifeStage == ls),
			se = FALSE,
			size = 1,
			method = "loess",
			color = "black",
			linetype = "dashed"
		) +
		scale_color_manual(values = natparks.pals("DeathValley")) +
		scale_y_continuous(limits = c(0, NA)) +
		# scale_y_log10() +
		labs(
			x = "Day of Year",
			y = "RMSE",
			title = ls,
			color = "Model",
			linetype = "Parameter DA"
		) +
		theme_pubr() +
		theme(legend.position = "right")
	if (ls == "Adult") {
		gg <- gg +
			annotate(
				geom = "rect",
				xmin = -Inf,
				xmax = rect.use$xmax,
				ymin = 0,
				ymax = Inf,
				alpha = 0.25
			) +
			annotate(
				geom = "rect",
				xmin = rect.use$xmin,
				xmax = Inf,
				ymin = 0,
				ymax = Inf,
				alpha = 0.25
			)
	} else {
		gg <- gg +
			annotate(
				geom = "rect",
				xmin = rect.use$xmin,
				xmax = rect.use$xmax,
				ymin = 0,
				ymax = Inf,
				alpha = 0.25
			)
	}
	if ("BLAN" %in% plot.sub) {
		gg <- gg + facet_grid(siteID ~ species, scales = "fixed")
	} else {
		gg <- gg + facet_grid(species ~ siteID, scales = "fixed")
	}
	dd <- paste0(
		"doy_",
		gsub("+ ", "", ua.sub),
		"_",
		gsub(" ", "", plot.sub),
		"_",
		ls,
		"_",
		".jpeg"
	)
	# print(gg)
	save_gg(
		dest = dd,
		gg = gg,
		path = file.path(dir.plot, "scoresDOY")
	)
}

ua.vec <- c("+ Driver", "+ Process", "IC", "+ Parameter")

for (u in 1:4) {
	for (ls in 1:3) {
		plot_doy(ua.vec[u], ix.plots, ls.vec[ls], "Ixodes scapularis")
		plot_doy(ua.vec[u], aa.plots, ls.vec[ls], "Amblyomma americanum")
		plot_doy(
			ua.vec[u],
			both.plots,
			ls.vec[ls],
			c("Amblyomma americanum", "Ixodes scapularis")
		)
	}
}

fx.scores <- read_csv(
	"/projectnb/dietzelab/fosterj/FinalOut/Chapter2/analysis/allForecastScores.csv"
)

ch2 <- fx.scores %>%
	filter(
		model %in% c("Weather", "WithWeatherAndMiceGlobal"),
		time != start.date,
		metric == "crps",
		!grepl("remove", experiment),
		!grepl("nmme", experiment),
		lifeStage == "Nymphs"
	) %>%
	group_by(model, paramsFrom, ticksFrom) %>%
	summarise(mu = mean(score))


df.model.names %>%
	filter(
		# ua == ua.sub,
		time != start.date,
		observed > 0,
		siteID %in% c(ix.plots, both.plots),
		species == "Ixodes scapularis",
		# model == "Weather & Mice",
		lifeStage == ls
	) %>%
	group_by(siteID, model) %>%
	summarise(mu = mean(crps)) %>%
	mutate(
		siteID = factor(
			siteID,
			levels = c("TREE", "HARV", "BLAN", "SCBI", "SERC", "LENO")
		)
	) %>%
	ggplot() +
	aes(x = siteID, y = mu, color = model) +
	geom_point(position = position_dodge(width = 0.5)) +
	geom_linerange(
		aes(ymin = 0, ymax = mu),
		position = position_dodge(width = 0.5)
	)


plot_pred_obs <- function(ua.sub, plot.sub, ls) {
	gg <- df.model.names %>%
		filter(
			#ua == ua.sub,
			siteID %in% c("HARV", "TREE"),
			model == "Weather & Mice",
			time != start.date,
			lifeStage == ls
		) %>%
		group_by(siteID, time, start.date, ua, model, median) %>%
		mutate(mu = mean(observed)) %>%
		# filter(observed > 0.2*max(observed)) %>%
		ggplot() +
		aes(x = mu, y = mean, color = ua) +
		geom_point(size = 0.6) +
		geom_smooth(method = "loess", se = FALSE) +
		scale_color_manual(values = met.brewer("Egypt")) +
		geom_abline() +
		labs(
			x = "Observed",
			y = "Median Forecast",
			color = "Model",
			linetype = "Parameter DA"
		) +
		facet_grid(species ~ siteID, scales = "free") +
		coord_cartesian(ylim = c(0, NA)) +
		theme_pubr() +
		theme(legend.position = "right")
	dd <- paste0(
		"predObs_",
		gsub("+ ", "", ua.sub),
		"_",
		gsub(" ", "", plot.sub),
		"_",
		ls,
		".jpeg"
	)
	# print(gg)
	save_gg(
		dest = dd,
		gg = gg,
		path = file.path(dir.plot, "predObs")
	)
}

for (u in 1:2) {
	for (ls in 1:3) {
		plot_pred_obs(ua.vec[u], ix.plots, ls.vec[ls])
		plot_pred_obs(ua.vec[u], aa.plots, ls.vec[ls])
		plot_pred_obs(ua.vec[u], both.plots, ls.vec[ls])
	}
}

# time series figures --------------------------------------------------------------------------

site <- "HARV"
start.dates.site <- df.mutate %>%
	filter(siteID == site) %>%
	pull(start.date) %>%
	unique()

df.mutate %>%
	filter(
		lifeStage == "Nymph",
		siteID == "HARV",
		ua == "+ Driver",
		start.date == start.dates.site[4]
	) %>%
	mutate(time = as.character(time)) %>%
	ggplot(aes(x = time, color = ua)) +
	geom_linerange(aes(ymin = lower95, ymax = upper95)) +
	geom_point(aes(y = observed, color = "Data")) +
	facet_wrap(~model) +
	theme_pubclean()


data <- df %>% filter(!is.na(data))

# time series plots
life.stage.vec <- unique(df$lifeStage)
model.vec <- unique(df$model[!is.na(df$model)])
site.vec <- unique(df$siteID[!is.na(df$siteID)])


ls <- "Nymph"

df.spp <- df %>% filter(species == "Ixodes scapularis")
start.dates <- df.spp %>% pull(start.date) %>% unique()
start.dates <- start.dates[!is.na(start.dates)]
df.start <- df.spp %>%
	filter(
		start.date == start.dates[7],
		lifeStage == ls,
		# siteID == "HARV",
		!is.na(model)
	)
fx.time <- df.start %>% pull(time) %>% unique()
data <- df %>%
	filter(
		!is.na(data),
		time %in% fx.time,
		# grepl("HARV", plotID),
		lifeStage == ls,
		species == "Ixodes scapularis"
	) %>%
	select(time, data, plotID)

df.plot <- bind_rows(df.start, data)

df.start %>%
	ggplot() +
	aes(x = time) +
	geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = model), alpha = 0.4) +
	# geom_point(data = data, aes(y = data, shape = plotID)) +
	# geom_line(aes(y = median)) +
	scale_fill_manual(values = met.brewer("Egypt")) +
	facet_grid(siteID ~ model) +
	theme_pubclean()


## Predicted observed
df <- read_csv(file.path(dir.analysis, "stateSummary_ic.csv"))


df %>%
	filter(lifeStage == "Nymph") %>%
	ggplot() +
	aes(x = density, y = median) +
	geom_point() +
	geom_abline(intercept = 0, slope = 1) +
	facet_grid(lifeStage ~ species, scales = "free") +
	theme_pubclean()


scores <- read_csv(file.path(dir.analysis, "stateSamples_ic.csv"))


scores %>%
	group_by(lifeStage) %>%
	summarise(best = min(crps))

scores %>%
	filter(lifeStage == "Nymph")


### ua figures ---------------------------------------------------------------------------
unique(df.mutate$ua)

df.rel.var <- df.mutate %>%
	filter(horizon > 0) %>%
	ungroup() %>%
	select(
		lifeStage,
		time,
		siteID,
		plotID,
		start.date,
		species,
		nlcd,
		observed,
		horizon,
		mice,
		weather,
		ua,
		variance
	) %>%
	pivot_wider(names_from = ua, values_from = variance) %>%
	group_by(
		lifeStage,
		time,
		siteID,
		start.date,
		species,
		nlcd,
		observed,
		horizon,
		mice,
		weather
	) %>%
	summarise(
		max.var = max(c(IC, `+ Process`, `+ Driver`, `+ Parameter`)),
		`IC` = IC / max.var,
		`+ Driver` = `+ Driver` / max.var,
		`+ Parameter` = `+ Parameter` / max.var,
		`+ Process` = `+ Process` / max.var
	) %>%
	pivot_longer(
		cols = c(IC, `+ Driver`, `+ Parameter`, `+ Process`),
		names_to = "ua",
		values_to = "relativeVariance"
	) %>%
	distinct()

n.sites.per.fx <- df.rel.var %>%
	ungroup() %>%
	mutate(siteID = str_extract(plotID, "[[:alpha:]]{4}")) %>%
	select(start.date, siteID) %>%
	distinct() %>%
	group_by(start.date) %>%
	count() %>%
	filter(n > 2)


site <- "SERC"
fx.issue.date <- all.days.df %>%
	filter(siteID == site) %>%
	pull(start.date) %>%
	unique()

i <- 9

gg <- df.rel.var %>%
	filter(
		grepl(site, siteID),
		mice == "Mice",
		weather == "Weather",
		start.date == "2018-08-15"
		# lifeStage == "Nymph"
	) %>%
	mutate(time = as.character(time)) %>%
	mutate(
		ua = if_else(ua == "IC", "IC (1)", ua),
		ua = if_else(ua == "+ Parameter", "+ Parameter (2)", ua),
		ua = if_else(ua == "+ Driver", "+ Driver (3)", ua),
		ua = if_else(ua == "+ Process", "+ Process (4)", ua)
	) %>%
	mutate(
		ua = factor(
			ua,
			levels = c("IC (1)", "+ Parameter (2)", "+ Driver (3)", "+ Process (4)")
		),
		lifeStage = factor(lifeStage, levels = c("Larva", "Nymph", "Adult"))
	) %>%
	ggplot() +
	aes(x = time, y = relativeVariance, fill = ua) +
	scale_fill_manual(values = natparks.pals("DeathValley")) +
	geom_bar(position = "fill", stat = "identity") +
	labs(x = "", y = "Relative variance", fill = "Uncertainty\nsource") +
	facet_grid(lifeStage ~ species) +
	theme_bw() +
	theme(
		axis.text.x = element_text(angle = 90, hjust = 0.5),
		legend.position = "bottom"
	)
gg

save_gg(
	dest = paste0("relVar_2018-08-15_", site, ".jpeg"),
	gg = gg,
	path = dir.plot
)

#### parameter figures

dd <- "/projectnb/dietzelab/fosterj/FinalOut/Chapter3/analysisConstraint"
df.params <- read_csv(file.path(
	dd,
	"ic_parameter_driver_process_allParamQuants.csv"
))
df.params.hindcast <- read_csv(
	"/projectnb/dietzelab/fosterj/FinalOut/Chapter2/analysis/allParameterQuants.csv"
)

r <- df.params.hindcast %>%
	filter(model == "WithWeatherAndMiceGlobal", experiment == "base_mna") %>%
	pull(start.date) %>%
	range()

hindcast.sig <- df.params.hindcast %>%
	filter(
		model == "WithWeatherAndMiceGlobal",
		experiment == "base_mna",
		start.date == r[1] | start.date == r[2]
	) %>%
	rename(
		node = parameter,
		siteID = paramsFrom,
		lower95 = q0.025,
		upper95 = q0.975,
		median = q0.5
	) %>%
	select(node, lower95, median, upper95, start.date, siteID) %>%
	mutate(
		species = "Ixodes scapularis",
		Transfer = "Time",
		start = if_else(start.date == first(start.date), "start", "end")
	) %>%
	select(-start.date)


transfer.sig <- df.params %>%
	filter(model == "Mice & Weather", ua == "+ Process", !grepl("gdd", node)) %>%
	group_by(siteID) %>%
	filter(start.date == last(start.date)) %>%
	select(node, lower95, median, upper95, siteID, species) %>%
	mutate(Transfer = "Space", start = "end")


params.bind <- bind_rows(hindcast.sig, transfer.sig) %>%
	mutate(
		name = if_else(node == "beta[1]", "Max temp (L)", node),
		name = if_else(node == "beta[2]", "Max RH (L)", name),
		name = if_else(node == "beta[3]", "Min RH (L)", name),
		name = if_else(node == "beta[4]", "Precip (L)", name),
		name = if_else(node == "beta[5]", "Max Temp (N)", name),
		name = if_else(node == "beta[6]", "Max RH (N)", name),
		name = if_else(node == "beta[7]", "Min RH (N)", name),
		name = if_else(node == "beta[8]", "Precip (N)", name),
		name = if_else(node == "beta[9]", "Max temp (A)", name),
		name = if_else(node == "beta[10]", "Max RH (A)", name),
		name = if_else(node == "beta[11]", "Min RH (A)", name),
		name = if_else(node == "beta[12]", "Precip (A)", name),
		name = if_else(node == "beta[13]", "Mice (L-N)", name),
		name = if_else(node == "beta[14]", "Mice (N-A)", name),
		name = if_else(node == "phi.l.mu", "Survival (L)", name),
		name = if_else(node == "phi.n.mu", "Survival (N)", name),
		name = if_else(node == "phi.a.mu", "Survival (A)", name),
		name = if_else(node == "theta.ln", "Transition (L-N)", name),
		name = if_else(node == "theta.na", "Transition (N-A)", name),
		name = if_else(node == "sig[1]", "Variance (L)", name),
		name = if_else(node == "sig[2]", "Variance (D)", name),
		name = if_else(node == "sig[3]", "Variance (N)", name),
		name = if_else(node == "sig[4]", "Variance (A)", name)
	)

plot_95 <- function(n, spp) {
	p1 <- params.bind %>%
		filter(siteID == "CARY", grepl(n, node))
	p2 <- params.bind %>%
		filter(grepl(n, node), siteID != "CARY", species == spp)

	size <- if_else(n == "beta", 0.2, 0.5)
	sc <- if_else(n == "sig", "free", "fixed")

	gg <- bind_rows(p1, p2) %>%
		ggplot() +
		aes(
			y = siteID,
			x = median,
			xmax = upper95,
			xmin = lower95,
			color = Transfer,
			linetype = start
		) +
		geom_pointrange(size = size, position = position_dodge(w = 1)) +
		facet_wrap(~name, scales = "fixed") +
		scale_color_manual(values = met.brewer("Egypt")) +
		labs(
			y = element_blank(),
			x = "Value",
			# color = "",
			linetype = ""
		) +
		theme_pubr() +
		theme(legend.position = "bottom") +
		theme(axis.text.y = element_text(size = 6))

	if (n %in% c("beta", "Mice")) {
		gg <- gg + geom_vline(xintercept = 0, linetype = "dashed")
	}
	if (n == "sig") {
		gg <- gg + scale_x_log10()
	}

	save_gg(
		dest = paste0(n, "_", gsub(" ", "", spp), ".jpeg"),
		gg = gg,
		path = file.path(dir.plot, "parameterComparisonToHindcast")
	)
	print(gg)
	return(gg)
}

gg1 <- plot_95("Mice", "Amblyomma americanum") +
	labs(title = "Amblyomma americanum", x = "")
gg2 <- plot_95("Mice", "Ixodes scapularis") + labs(title = "Ixodes scapularis")


gg3 <- ggarrange(gg1, gg2, nrow = 2, common.legend = TRUE, legend = "bottom")
save_gg(
	dest = "miceBothSpecies.jpeg",
	gg = gg3,
	path = file.path(dir.plot, "parameterComparisonToHindcast")
)


plot_95("beta", "Amblyomma americanum")
plot_95("phi", "Ixodes scapularis")
plot_95("theta", "Ixodes scapularis")
plot_95("sig", "Ixodes scapularis")
plot_95("beta", "Ixodes scapularis")
plot_95("phi", "Amblyomma americanum")
plot_95("theta", "Amblyomma americanum")
plot_95("sig", "Amblyomma americanum")


sig <- df.params %>%
	filter(model == "Mice & Weather", ua == "+ Process", !grepl("gdd", node)) %>%
	mutate(
		name = if_else(node == "beta[1]", "Max temp (L)", node),
		name = if_else(node == "beta[2]", "Max RH (L)", name),
		name = if_else(node == "beta[3]", "Min RH (L)", name),
		name = if_else(node == "beta[4]", "Precip (L)", name),
		name = if_else(node == "beta[5]", "Max Temp (N)", name),
		name = if_else(node == "beta[6]", "Max RH (N)", name),
		name = if_else(node == "beta[7]", "Min RH (N)", name),
		name = if_else(node == "beta[8]", "Precip (N)", name),
		name = if_else(node == "beta[9]", "Max temp (A)", name),
		name = if_else(node == "beta[10]", "Max RH (A)", name),
		name = if_else(node == "beta[11]", "Min RH (A)", name),
		name = if_else(node == "beta[12]", "Precip (A)", name),
		name = if_else(node == "beta[13]", "Mice (L-N)", name),
		name = if_else(node == "beta[14]", "Mice (N-A)", name),
		name = if_else(node == "phi.l.mu", "Survival (L)", name),
		name = if_else(node == "phi.n.mu", "Survival (N)", name),
		name = if_else(node == "phi.a.mu", "Survival (A)", name),
		name = if_else(node == "theta.ln", "Transition (L-N)", name),
		name = if_else(node == "theta.na", "Transition (N-A)", name),
		name = if_else(node == "sig[1]", "Variance (L)", name),
		name = if_else(node == "sig[2]", "Variance (D)", name),
		name = if_else(node == "sig[3]", "Variance (N)", name),
		name = if_else(node == "sig[4]", "Variance (A)", name)
	)


sig %>%
	filter(siteID == "SERC", grepl("sig", node)) %>%
	ggplot() +
	aes(x = start.date) +
	geom_ribbon(
		aes(ymin = lower95, ymax = upper95, fill = species),
		alpha = 0.5
	) +
	scale_fill_manual(values = met.brewer("Egypt")) +
	facet_wrap(~name, scales = "free") +
	theme_pubr()
