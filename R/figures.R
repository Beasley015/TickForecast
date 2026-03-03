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
    summarise(sampledArea = sum(totalSampledArea)) %>%
    suppressMessages()

  # Condense forecast across plots
  forecast.density <- df.mutate %>%
	  select(time, lifeStage, siteID, species, model, mean, lower95, upper95) %>%
    mutate(time=as.Date(time, format = "%Y-%m-%d")) %>%
    left_join(scores, by = c("time", "species","model")) %>%
    fill(sampledArea, .direction="down") %>%
    group_by(time, lifeStage, model, species, sampledArea) %>% 
    summarise(mean.forecast = mean(mean), forecast05 = mean(lower95),
              forecast95=mean(upper95)) %>%
    suppressMessages()
    
  if(!(site.vec %in% c("GREN", "HNRY", "TEA"))){
    forecast.density <- forecast.density %>%
      mutate(mean.density = (mean.forecast/sampledArea)*450,
             density05 = (forecast05/sampledArea)*450,
             density95 = (forecast95/sampledArea)*450) %>%
      suppressMessages()
  } else{
    forecast.density <- forecast.density %>%
      rename("mean.density" = "mean.forecast", "density05"="forecast05",
             "density95"="forecast95")
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
    summarise(meandensity = mean(density)) %>%
    suppressMessages()

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
                  alpha = 0.3)+
      geom_line(dat=forecast.smol, aes(x=time, y=mean.density), color = "#0f7ba2")+
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
ix.plots <- c("TREE", "HARV", "GREN", "HNRY", "TEA")
aa.plots <- c("KONZ", "OSBS", "TALL", "UKFS")

null.crps <- null.scores %>%
	mutate(crps = score, doy = yday(time)) 

score.files <- analysis.files[str_detect(analysis.files, "analysis")]

df.mutate <- tibble()
for(i in 1:length(score.files)){
  score <- read_csv(file=file.path(dir.analysis, score.files[i])) %>%
    suppressMessages()
  
  score <- score %>%
    filter(year(time) >= 2018) %>%
    select(lifeStage, time, siteID, species, model, crps) %>%
    group_by(lifeStage, time, siteID, species, model) %>%
    summarise(crps = mean(crps)) %>%
    suppressMessages()
  
  df.mutate <- bind_rows(df.mutate, score)
  rm(score)
}

df.mutate <- df.mutate %>%
	mutate(doy = yday(time))

all.combos <- expand_grid(unique(df.mutate$lifeStage), unique(df.mutate$siteID),
                          unique(df.mutate$species))
colnames(all.combos) <- c("lifeStage", "siteID", "species")

# Score per day of year
for(i in 1:nrow(all.combos)){
  scores.smol <- df.mutate %>%
    filter(lifeStage==all.combos$lifeStage[i], siteID==all.combos$siteID[i],
           species==all.combos$species[i])

  null.smol <- null.crps %>%
    filter(lifeStage==all.combos$lifeStage[i], siteID==all.combos$siteID[i],
           species==all.combos$species[i])

  gg <- ggplot() +
    aes(x = doy, y = crps)+
    geom_smooth(data=scores.smol, se = FALSE, size = 1, method = "loess", alpha = 0.6, 
                aes(color = model)) +
    geom_smooth(data = null.smol, se = FALSE, size = 1, method = "loess", 
                color = "black", linetype = "dashed") +
    scale_color_manual(values = natparks.pals("DeathValley")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Day of Year", y = "CRPS",  color = "Model", linetype = "Null",
      title = paste(all.combos$siteID[i], ", ", all.combos$species[i], ", ",
                    all.combos$lifeStage[i], sep = "")) +
    theme_pubr() +
    theme(legend.position = "right")

  save_gg(dest = paste(all.combos$siteID[i], "_", str_replace_all(all.combos$species[i], " ", ""), 
                       "_", all.combos$lifeStage[i], sep = ""), 
          gg = gg, path = file.path(dir.plot, "scoresDOY"))
}

# Boxplots
scores.all <- bind_rows(df.mutate, null.crps)

aa.df <- scores.all %>%
  filter(siteID %in% aa.plots, lifeStage=="Nymph") %>%
  select(siteID, time, model, crps) %>%
  group_by(siteID, time, model) %>%
  summarise(crps = mean(crps, na.rm=T)) %>%
  pivot_wider(names_from = model, values_from = crps) %>%
  filter(is.na(Weather) == F) %>%
  rename("WeatherMice" = "Weather & Mice") %>%
  mutate(NullWeather = Null-Weather, NullWeatherMice = Null-WeatherMice) %>%
  select(-c(Null, Weather, WeatherMice)) %>%
  pivot_longer(NullWeather:NullWeatherMice, names_to = 'model', 
               values_to = 'crps.diff') %>%
  mutate(model = case_when(model=="NullWeather" ~ "Weather",
                           model=="NullWeatherMice" ~ "Weather & Mice"))

aa.fig <- ggplot(data=aa.df, aes(x = model, y = crps.diff))+
  geom_boxplot(fill='lightgray')+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  facet_wrap(~siteID)+
  labs(x = "Model", y = "Null CRPS - Model CRPS",
       title = "A. americanum")+
  theme_bw()+
  theme(panel.grid=element_blank())

aa.tab <- aa.figs %>%
  group_by(siteID, model) %>%
  summarise(mean.diff = mean(crps.diff))

ix.df <- scores.all %>%
  filter(siteID %in% ix.plots, lifeStage=="Nymph") %>%
  select(siteID, time, model, crps) %>%
  group_by(siteID, time, model) %>%
  summarise(crps = mean(crps, na.rm=T)) %>%
  pivot_wider(names_from = model, values_from = crps) %>%
  filter(is.na(Weather) == F) %>%
  rename("WeatherMice" = "Weather & Mice") %>%
  mutate(NullWeather = Null-Weather, NullWeatherMice = Null-WeatherMice) %>%
  select(-c(Null, Weather, WeatherMice)) %>%
  pivot_longer(NullWeather:NullWeatherMice, names_to = 'model', 
               values_to = 'crps.diff') %>%
  mutate(model = case_when(model=="NullWeather" ~ "Weather",
                           model=="NullWeatherMice" ~ "Weather & Mice"))

ix.fig <- ggplot(data=ix.df, aes(x = model, y = crps.diff))+
  geom_boxplot(fill='lightgray')+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  facet_wrap(~siteID)+
  labs(x = "Model", y = "Null CRPS - Model CRPS",
       title = "I. scapularis")+
  theme_bw()+
  theme(panel.grid=element_blank())

ix.tab <- ix.df %>%
  group_by(siteID, model) %>%
  summarise(mean.diff = mean(crps.diff))

both.df <-  scores.all %>%
  filter(siteID %in% both.plots, lifeStage=="Nymph") %>%
  select(siteID, time, model, crps, species) %>%
  group_by(siteID, time, model, species) %>%
  summarise(crps = mean(crps, na.rm=T)) %>%
  pivot_wider(names_from = model, values_from = crps) %>%
  filter(is.na(Weather) == F) %>%
  rename("WeatherMice" = "Weather & Mice") %>%
  mutate(NullWeather = Null-Weather, NullWeatherMice = Null-WeatherMice) %>%
  select(-c(Null, Weather, WeatherMice)) %>%
  pivot_longer(NullWeather:NullWeatherMice, names_to = 'model', 
               values_to = 'crps.diff') %>%
  mutate(model = case_when(model=="NullWeather" ~ "Weather",
                           model=="NullWeatherMice" ~ "Weather & Mice"))

ggplot(data=both.df, aes(x = model, y = crps.diff, fill = species))+
  geom_boxplot()+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  facet_wrap(~siteID)+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  labs(x = "Model", y = "Null CRPS - Model CRPS")+
  theme_bw()+
  theme(panel.grid=element_blank())

both.tab <- both.df %>%
  group_by(siteID, model, species) %>%
  summarise(mean.diff = mean(crps.diff))

# save_gg("aa_crps_diff.jpeg", aa.fig, dir.plot)
# save_gg("ix_crps_diff.jpeg", ix.fig, dir.plot)
# save_gg("both_crps_diff.jpeg", both.fig, dir.plot)

# Scores across model iterations --------------------------
score.files <- analysis.files[str_detect(analysis.files, "analysis")]

both.plots <- c("BLAN", "LENO", "SCBI", "SERC")
ix.plots <- c("TREE", "HARV", "GREN", "HNRY", "TEA")
aa.plots <- c("KONZ", "OSBS", "TALL", "UKFS")

df.mutate <- tibble()
for(i in 1:length(score.files)){
  score <- read_csv(file=file.path(dir.analysis, score.files[i])) %>%
    suppressMessages()
  
  score <- score %>%
    filter(year(time) >= 2018) %>%
    select(lifeStage, siteID, species, model, start.date, crps) %>%
    group_by(lifeStage, siteID, species, model, start.date) %>%
    summarise(crps = mean(crps)) %>%
    suppressMessages()
  
  df.mutate <- bind_rows(df.mutate, score)
  rm(score)
}

aa.sites <- df.mutate %>%
  filter(siteID %in% aa.plots)

aa.score.ts <- ggplot(data=aa.sites, aes(x = start.date, y = crps, color = model,
                          fill=model))+
  geom_smooth(method='lm', linewidth = 1.5)+
  facet_wrap(~siteID, nrow = 2, ncol = 2)+
  labs(x = "Forecast Start Date", y = "CRPS", color = "Model",
       fill = "Model")+
  scale_color_manual(values = natparks.pals("DeathValley"))+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  theme_bw()+
  theme(panel.grid = element_blank())

ix.sites <- df.mutate %>%
  filter(siteID %in% ix.plots)

ix.score.ts <- ggplot(data=ix.sites, aes(x = start.date, y = crps, color = model,
                          fill=model))+
  geom_smooth(method='lm', linewidth = 1.5)+
  facet_wrap(~siteID)+
  labs(x = "Forecast Start Date", y = "CRPS", color = "Model",
       fill = "Model")+
  scale_color_manual(values = natparks.pals("DeathValley"))+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  theme_bw()+
  theme(panel.grid = element_blank())

both.sites <- df.mutate %>%
  filter(siteID %in% both.plots)

both.score.ts <- ggplot(data=both.sites, aes(x = start.date, y = crps, color = model,
                          fill=model))+
  geom_smooth(method='lm', linewidth = 1.5)+
  facet_grid(rows = vars(siteID), cols = vars(species))+
  labs(x = "Forecast Start Date", y = "CRPS", color = "Model",
       fill = "Model")+
  scale_color_manual(values = natparks.pals("DeathValley"))+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  theme_bw()+
  theme(panel.grid = element_blank())

# save_gg("aa_score_ts.jpeg", aa.score.ts, dir.plot)
# save_gg("ix_score_ts.jpeg", ix.score.ts, dir.plot)
# save_gg("both_score_ts.jpeg", both.score.ts, dir.plot)
