######################################################
# Figure creation for tick forecasting at NEON sites #
# original script by J. Foster                       #
# Update by E.M. Beasley                             #
# Fall 2025                                          #
######################################################

# Load packages ------------------
library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)
library(utils)
library(ggpubr)
library(MetBrewer)
library(NatParksPalettes)

dir.out <- "./out/"
dir.analysis <- "./analysis/"
dir.plot <- "./figures/"

# Create function for saving plots------------
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

null.scores <- null.quants <- tibble()
for (i in seq_along(null.scores.files)) {
	null.s <- read_csv(file.path(dir.out.null, null.scores.files[i])) %>%
		suppressMessages()
	null.scores <- bind_rows(null.scores, null.s)

	null.q <- read_csv(file.path(dir.out.null, null.quants.files[i])) %>%
		suppressMessages()
	null.quants <- bind_rows(null.quants, null.q)
}

df.null <- null.quants %>%
	rename(lower95 = ymin, upper95 = ymax, variance = var, median = fx) %>%
	select(-n.days, -n.drags, -count.flag, -siteID) %>%
  filter(!(is.na(species)))

# Raw NEON data ----------------
source("./DataProcessing/functions.R")
neon.ix <- neon_tick_data("Ixodes scapularis") %>% suppressMessages()
neon.aa <- neon_tick_data("Amblyomma americanum") %>% suppressMessages()

neon.data <- bind_rows(neon.ix, neon.aa) %>%
  filter(time >= "2018-01-01") %>%
  select(-n.drags, -n.days, -count.flag) %>%
  pivot_longer(cols = c(Larva, Nymph, Adult),
               names_to = "lifeStage",
               values_to = "observed") %>%
  mutate(density = observed / totalSampledArea * 450)

site.info <- neonstore::neon_sites()

# Model output ------------------
analysis.files <- list.files(dir.analysis, recursive = T)

# time series figures-------------------------------------------------------------------
series.files <- analysis.files[str_detect(analysis.files, "allDays")]

for(i in 1:length(series.files)){
  # Read in time series from forecasting model
  df.mutate <- read.csv(paste(dir.analysis, series.files[i], sep = ""))
  
  # Get constants for filtering
  ls <- c("Larva", "Nymph", "Adult")
  sp <- unique(df.mutate$species)
  site.vec <- unique(df.mutate$siteID)
  mod <- unique(df.mutate$model)
  
  # Get associated scoring files because they have plot areas
  score.files <- analysis.files[str_detect(analysis.files, site.vec)]
  score.files <- score.files[-length(score.files)]
  
  # Read in score files
  scores <- tibble()
  for(j in 1:length(score.files)){
    file <- read_csv(file=paste(dir.analysis,score.files[j], sep = "")) %>%
      suppressMessages()
    scores <- bind_rows(scores, file)
    rm(file)
  }
  
  scores <- scores %>%
    select(time, siteID, totalSampledArea, plotID, species, model) %>%
    group_by(time, species, model, plotID) %>%
    distinct() %>%
    ungroup() %>%
    group_by(time, species, model) %>%
    summarise(sampledArea = sum(totalSampledArea))

  # Condense forecast across plots
  forecast.density <- df.mutate %>%
	  select(time, lifeStage, siteID, species, model, mean, lower95, upper95) %>%
    mutate(time=as.Date(time, format = "%Y-%m-%d")) %>%
    left_join(scores, by = c("time", "species","model")) %>%
    fill(sampledArea, .direction="down") %>%
    group_by(time, lifeStage, model, species, sampledArea) %>% 
    summarise(mean.forecast = mean(mean), forecast05 = mean(lower95),
              forecast95=mean(upper95)) 
    
  if(!(site.vec %in% c("GREN", "HNRY", "TEA"))){
    forecast.density <- forecast.density %>%
      mutate(mean.density = (mean.forecast/sampledArea)*450,
             density05 = (forecast05/sampledArea)*450,
             density95 = (forecast95/sampledArea)*450)
  }
  
  all_combos <- expand_grid(ls, sp, mod)

  fx.issue.date <- neon.data %>%
	  filter(siteID == site.vec) %>%
	  pull(time) %>%
	  unique()
  fx.issue.date <- as.Date(fx.issue.date, format = "%Y-%m-%d")

  # Filter null model data
  df.null.timeseries <- df.null %>%
    filter(site == site.vec, time >= min(fx.issue.date), time <= max(fx.issue.date),
           lifeStage %in% ls, species %in% sp) %>%
    rename(siteID = site) %>%
    select(median, lower95, upper95, variance, time, lifeStage, species) %>%
    group_by(time)

  # Filter raw data
  neon.timeseries <- neon.data %>%
    mutate(time = as.Date(time, format ="%Y-%m-%d")) %>%
    filter(siteID==site.vec, lifeStage %in% ls, species %in% sp, time>=min(fx.issue.date),
           time<=as.Date("2022-01-01", format="%Y-%m-%d")) %>%
    select(time, plotID, density, lifeStage, species) %>%
    group_by(time, lifeStage, species) %>%
    summarise(meandensity = mean(density))

  # Filter forecast data
  forecast.density <- forecast.density %>%
    filter(time >= min(fx.issue.date),time <= max(fx.issue.date+364))

  dist.cols <- c(
	  "Data" = "#dd5129",
	  "Forecast" = "#0f7ba2",
	  "Null" = "#43b284"
  )

  for(j in 1:nrow(all_combos)){
    # Further filtering
    forecast.smol <- forecast.density %>%
      filter(lifeStage == all_combos$ls[j], species==all_combos$sp[j], model == all_combos$mod[j])
    
    neon.smol <- neon.timeseries %>%
      filter(lifeStage == all_combos$ls[j], species==all_combos$sp[j])
    
    null.smol <- df.null.timeseries %>%
      filter(lifeStage == all_combos$ls[j], species==all_combos$sp[j])
    
    gg <- ggplot() +
	    geom_ribbon(data=null.smol, aes(x = time, ymin = lower95, ymax = upper95, fill = "Null")) +
	    geom_point(data = neon.smol, aes(x=time, y = meandensity, fill = "Observed Data"),
	               color = "#dd5129", size = 3) +
      geom_ribbon(data=forecast.smol, aes(x = time, ymin=density05, ymax=density95, fill = "Forecast"),
                  alpha = 0.5)+
      lims(x = c(fx.issue.date[1], as.Date("2022-01-01", format = "%Y-%m-%d"))) +
	    labs(x = "Date", y = "Ticks/450m^2", 
	         title = paste(site.vec, ", ", all_combos$sp[j], ", ", all_combos$ls[j], ", ", 
	                       all_combos$mod[j], sep = "")) +
      scale_fill_manual(values = c("#0f7ba2", "#43b284", "#dd5129"), name = "")+
	    theme_pubr() +
	    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
	          legend.position = "bottom")
    gg
    save_gg(
	    dest = paste0("/timeseries_singlemods/", site.vec, "_", all_combos$sp[j], "_", 
	                  all_combos$ls[j], "_", all_combos$mod[j], ".jpeg"),
	    gg = gg,
	    path = dir.plot
    )
    print(paste("Site = ", site.vec, ", Species = ", all_combos$sp[j], ", Life Stage = ", all_combos$ls[j], 
                ", Model = ", all_combos$mod[j], sep = ""))
  }
}

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
