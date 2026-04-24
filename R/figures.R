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
library(sf)

dir.out <- "./out/"
dir.analysis <- "./analysis/"
dir.plot <- "./figures/"

# Create function for saving plots------------
save_gg <- function(dest, gg, path, width=7, height=5) {
	if (!dir.exists(path)) {
		dir.create(path, showWarnings = FALSE, recursive = TRUE)
	}
	ggsave(
		filename = dest,
		plot = gg,
		device = "jpeg",
		path = path,
		width = width,
		height = height,
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
  filter(time >= "2018-01-01" & time <= "2022-01-01") %>%
  select(-n.drags, -n.days, -count.flag) %>%
  pivot_longer(cols = c(Larva, Nymph, Adult),
               names_to = "lifeStage",
               values_to = "observed") %>%
  mutate(density = observed / totalSampledArea * 450)

site.info <- neonstore::neon_sites()

# Model output ------------------
analysis.files <- list.files(dir.analysis, recursive = T)

# time series figures (single site) ----------------------------------------------
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
    filter(time >= as.Date("2018-01-01", format = "%Y-%m-%d"),
           is.na(plotID) == F) %>%
    group_by(time, species, model, plotID) %>%
    distinct() %>%
    ungroup() %>%
    group_by(time, species, model) %>%
    summarise(sampledArea = sum(totalSampledArea, na.rm = T)) %>%
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
  
  rm(df.mutate)
    
  if(!(site.vec %in% c("GREN", "HNRY", "TEA"))){
    forecast.density <- forecast.density %>%
      mutate(mean.forecast = (mean.forecast/sampledArea)*450,
             forecast05 = (forecast05/sampledArea)*450,
             forecast95 = (forecast95/sampledArea)*450) %>%
      suppressMessages()
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
      geom_ribbon(data=forecast.smol, aes(x = time, ymin=forecast05, ymax=forecast95, fill = "Forecast"),
                  alpha = 0.3)+
      geom_line(dat=forecast.smol, aes(x=time, y=mean.forecast), color = "#0f7ba2")+
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
  rm(list = c("scores", 'forecast.density', 'forecast.smol'))
  gc()
}

# Time series figures (hierarchical) ---------------------------
series.files <- analysis.files[str_detect(analysis.files, "allDays")]
hierarchical.files <- c(series.files[str_detect(series.files, "Ixodes_scapularis")],
                        series.files[str_detect(series.files, "Amblyomma_americanum")])

for(i in 1:length(hierarchical.files)){
  # Read in time series from forecasting model
  df.mutate <- read.csv(paste(dir.analysis, "/",hierarchical.files[i], sep = "")) %>%
    unite(col = model, model, hierarchy, sep = "_") 
  
  # Get constants for filtering
  ls <- c("Larva", "Nymph", "Adult")
  sp <- unique(df.mutate$species)
  site.vec <- unique(df.mutate$siteID)
  mod <- unique(df.mutate$model) 
  h <- unique(df.mutate$hierarchy)
  
  # Get associated scoring files because they have plot areas
  score.files <- analysis.files[str_detect(analysis.files, "hierarchical")]
  score.files <- score.files[str_detect(score.files, sp)]
  
  # Read in score files
  scores <- tibble()
  for(j in 1:length(score.files)){
    file <- read_csv(file=paste(dir.analysis,"/",score.files[j], sep = "")) %>%
      suppressMessages()
    scores <- bind_rows(scores, file)
    rm(file)
  }
  
  scores <- scores %>%
    select(time, siteID, totalSampledArea, plotID, species, model) %>%
    filter(time >= as.Date("2018-01-01", format = "%Y-%m-%d"),
           is.na(plotID) == F) %>%
    group_by(time, siteID, model, plotID) %>%
    distinct() %>%
    ungroup() %>%
    group_by(time, siteID, model) %>%
    summarise(sampledArea = sum(totalSampledArea, na.rm = T)) %>%
    suppressMessages()
  
  # Condense forecast across plots
  forecast.density <- df.mutate %>%
    select(time, lifeStage, siteID, species, model, mean, lower75, upper75) %>%
    mutate(time=as.Date(time, format = "%Y-%m-%d")) %>%
    mutate(model = case_when(model == "Weather_FullHierarchical" ~ 
                               "Weather_hierarchicalFull",
                             model == "Weather_HierarchicalIntercept" ~ 
                               "Weather_hierarchicalIntercept",
                             model == "WeatherMice_HierarchicalIntercept" ~ 
                               "WeatherMice_hierarchicalIntercept",
                             model == "WeatherMice_FullHierarchical" ~ 
                               "WeatherMice_hierarchicalFull",
                             TRUE ~ model)) %>%
    left_join(scores, by = c("time", "siteID","model")) %>%
    ungroup() %>%
    group_by(siteID) %>%
    mutate(sampledArea = case_when(siteID %in% c("GREN", "HNRY", "TEA")~450,
                                   TRUE ~ sampledArea)) %>%
    mutate(sampledArea = case_when(is.na(sampledArea)==T ~ 
                                     mean(sampledArea,na.rm=T),
                                   TRUE ~ sampledArea)) %>%
    group_by(time, lifeStage, model, siteID, sampledArea) %>% 
    mutate(mean = case_when(mean < 0 ~ 0, TRUE ~ mean),
           lower75 = case_when(lower75 < 0 ~ 0, TRUE ~ lower75),
           upper75 = case_when(upper75 < 0 ~ 0, TRUE ~ upper75)) %>%
    summarise(mean.forecast = mean(mean), forecast05 = mean(lower75),
              forecast95=mean(upper75)) %>%
    suppressMessages()
  
  rm(df.mutate)
  
  all_combos <- expand_grid(ls, sp, site.vec)
  
  fx.issue.date <- neon.data %>%
    filter(siteID %in% site.vec) %>%
    pull(time) %>%
    unique()
  fx.issue.date <- as.Date(fx.issue.date, format = "%Y-%m-%d")
  
  # Filter null model data
  df.null.timeseries <- df.null %>%
    mutate(species = str_replace(species, " ", "_")) %>%
    filter(site %in% site.vec, time >= min(fx.issue.date), time <= max(fx.issue.date),
           lifeStage %in% ls, species %in% sp) %>%
    rename(siteID = site) %>%
    select(median, lower95, upper95, variance, time, siteID, lifeStage, species) %>%
    group_by(time) %>%
    rename(mean.forecast = median, forecast05=lower95, forecast95=upper95)
  
  # Filter raw data
  neon.timeseries <- neon.data %>%
    mutate(time = as.Date(time, format ="%Y-%m-%d")) %>%
    mutate(species = str_replace(species, " ", "_")) %>%
    filter(siteID%in%site.vec, lifeStage %in% ls, species %in% sp, 
           time>=min(fx.issue.date),
           time<=as.Date("2022-01-01", format="%Y-%m-%d")) %>%
    select(time, plotID, siteID, density, lifeStage, species) %>%
    group_by(time, lifeStage, species, siteID) %>%
    summarise(meandensity = mean(density)) %>%
    suppressMessages()
  
  # Filter forecast data
  forecast.density <- forecast.density %>%
    filter(time >= min(fx.issue.date),time <= max(fx.issue.date+364))
  
  for(j in 1:nrow(all_combos)){
    # Further filtering
    forecast.smol <- forecast.density %>%
      filter(lifeStage == all_combos$ls[j], 
             siteID == all_combos$site.vec[j])

    neon.smol <- neon.timeseries %>%
      filter(lifeStage == all_combos$ls[j], siteID == all_combos$site.vec[j])
    
    null.smol <- df.null.timeseries %>%
      mutate(model = "Null") %>%
      filter(lifeStage == all_combos$ls[j], siteID == all_combos$site.vec[j])
    
    forecasts <- bind_rows(forecast.smol, null.smol)
    
    gg <- ggplot() +
      geom_ribbon(data=forecasts, aes(x = time, ymin = forecast05, ymax = forecast95, 
                                      fill = model), alpha = 0.3) +
      geom_line(data =forecasts, aes(x = time, y = mean.forecast, color = model))+
      geom_point(data = neon.smol, aes(x=time, y = meandensity),
                 color = "#dd5129", size = 3) +
      geom_ribbon(data=forecast.smol, aes(x = time, ymin=forecast05, ymax=forecast95, 
                                          fill = model),
                  alpha = 0.3)+
      lims(x = c(fx.issue.date[1], as.Date("2022-01-01", format = "%Y-%m-%d")))+
      labs(x = "Date", y = "Ticks/450m^2", 
           title = paste(all_combos$site.vec[j], ", ", all_combos$ls[j],
                         sep = "")) +
      scale_fill_manual(values = natparks.pals("DeathValley", 3), name = "Model Type")+
      scale_color_manual(values = natparks.pals('DeathValley', 3), name = '')+
      theme_bw() +
      theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
            panel.grid = element_blank())
    
    gg
    save_gg(
      dest = paste0("/timeseries_multisite/",all_combos$site.vec[j], "_", 
                    all_combos$ls[j], "_", sp, ".jpeg"),
      gg = gg,
      path = dir.plot
    )
    
    print(paste("Site = ", all_combos$site.vec[j], ", Species = ", sp, 
                ", Life Stage = ", all_combos$ls[j], sep = ""))
  }
  rm(list = c("scores", 'forecast.density', 'forecast.smol'))
  gc()
}


# score figures --------------------------------------------------------------------------
# Null model
null.crps <- null.scores %>%
	mutate(crps = score, doy = yday(time)) 

score.files <- analysis.files[!str_detect(analysis.files, "allDays")]

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
  filter(species == "Amblyomma americanum", lifeStage=="Nymph") %>%
  select(siteID, time, model, crps) %>%
  group_by(siteID, time, model) %>%
  summarise(crps = mean(crps, na.rm=T)) %>%
  pivot_wider(names_from = model, values_from = crps) %>%
  filter(is.na(Weather) == F) %>%
  rename("WeatherMice" = "WithWeatherAndMiceGlobal") %>%
  mutate(NullWeather = Null-Weather, NullWeatherMice = Null-WeatherMice,
         NullWeatherIntercept = Null-Weather_hierarchicalIntercept,
         NullWeatherFull = Null-Weather_hierarchicalFull,
         NullWeatherMiceIntercept = Null-WeatherMice_hierarchicalIntercept,
         NullWeatherMiceFull = Null-WeatherMice_hierarchicalFull) %>%
  select(-c(Null, Weather, WeatherMice, Weather_hierarchicalIntercept,
            WeatherMice_hierarchicalIntercept, Weather_hierarchicalFull,
            WeatherMice_hierarchicalFull)) %>%
  pivot_longer(NullWeather:NullWeatherMiceFull, names_to = 'model', 
               values_to = 'crps.diff') %>%
  mutate(model = case_when(model=="NullWeather" ~ "Weather",
                           model=="NullWeatherMice" ~ "Weather&Mice",
                           model=="NullWeatherIntercept"~"WeatherIntercept",
                           model=="NullWeatherFull"~"WeatherFull",
                           model=="NullWeatherMiceIntercept"~"WeatherMiceIntercept",
                           model=="NullWeatherMiceFull"~"WeatherMiceFull"))

aa.fig <- ggplot(data=aa.df, aes(x = model, y = crps.diff))+
  geom_boxplot(aes(fill=model))+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  lims(y = c(-20,NA))+
  facet_wrap(~siteID)+
  labs(x = "Model", y = "Null CRPS - Model CRPS",
       title = "A. americanum")+
  theme_bw()+
  theme(panel.grid=element_blank(), axis.text.x = element_blank())

aa.tab <- aa.df %>%
  group_by(siteID, model) %>%
  summarise(mean.diff = mean(crps.diff)) %>%
  pivot_wider(names_from = siteID, values_from = mean.diff)

ix.df <- scores.all %>%
  filter(species == "Ixodes scapularis", lifeStage=="Nymph") %>%
  select(siteID, time, model, crps) %>%
  group_by(siteID, time, model) %>%
  summarise(crps = mean(crps, na.rm=T)) %>%
  pivot_wider(names_from = model, values_from = crps) %>%
  filter(is.na(Weather) == F) %>%
  rename("WeatherMice" = "WithWeatherAndMiceGlobal") %>%
  mutate(NullWeather = Null-Weather, NullWeatherMice = Null-WeatherMice,
         NullWeatherIntercept = Null-Weather_hierarchicalIntercept,
         NullWeatherFull = Null-Weather_hierarchicalFull,
         NullWeatherMiceIntercept = Null-WeatherMice_hierarchicalIntercept,
         NullWeatherMiceFull = Null-WeatherMice_hierarchicalFull) %>%
  select(-c(Null, Weather, WeatherMice, Weather_hierarchicalIntercept,
            WeatherMice_hierarchicalIntercept, Weather_hierarchicalFull,
            WeatherMice_hierarchicalFull)) %>%
  pivot_longer(NullWeather:NullWeatherMiceFull, names_to = 'model', 
               values_to = 'crps.diff') %>%
  mutate(model = case_when(model=="NullWeather" ~ "Weather",
                           model=="NullWeatherMice" ~ "Weather&Mice",
                           model=="NullWeatherIntercept"~"WeatherIntercept",
                           model=="NullWeatherFull"~"WeatherFull",
                           model=="NullWeatherMiceIntercept"~"WeatherMiceIntercept",
                           model=="NullWeatherMiceFull"~"WeatherMiceFull"))

ix.fig <- ggplot(data=ix.df, aes(x = model, y = crps.diff))+
  geom_boxplot(aes(fill=model))+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  lims(y = c(-20,20))+
  facet_wrap(~siteID)+
  labs(x = "Model", y = "Null CRPS - Model CRPS",
       title = "I. scapularis")+
  theme_bw()+
  theme(panel.grid=element_blank(), axis.text.x = element_blank())

ix.tab <- ix.df %>%
  group_by(siteID, model) %>%
  summarise(mean.diff = mean(crps.diff, na.rm = T)) %>%
  pivot_wider(names_from = siteID, values_from = mean.diff)

# save_gg("aa_crps_diff.jpeg", aa.fig, dir.plot)
# save_gg("ix_crps_diff.jpeg", ix.fig, dir.plot)
# write_csv(aa.tab, "./figures/aa_crps_diff.csv")
# write_csv(ix.tab, "./figures/ix_crps_diff.csv")

# Scores across model iterations --------------------------
score.files <- analysis.files[!str_detect(analysis.files, "allDays")]

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
  filter(species == "Amblyomma americanum")

aa.score.ts <-ggplot(data=aa.sites, aes(x = start.date, y = crps, color = model,
                          fill=model))+
  geom_smooth(method='gam', linewidth = 1.5)+
  facet_wrap(~siteID)+
  labs(x = "Forecast Start Date", y = "CRPS", color = "Model",
       fill = "Model")+
  scale_color_manual(values = natparks.pals("DeathValley"))+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  theme_bw()+
  theme(panel.grid = element_blank())

ix.sites <- df.mutate %>%
  filter(species == "Ixodes scapularis")

ix.score.ts <-ggplot(data=ix.sites, aes(x = start.date, y = crps, color = model,
                          fill=model))+
  geom_smooth(method='gam', linewidth = 1.5)+
  facet_wrap(~siteID)+
  labs(x = "Forecast Start Date", y = "CRPS", color = "Model",
       fill = "Model")+
  scale_color_manual(values = natparks.pals("DeathValley"))+
  scale_fill_manual(values = natparks.pals("DeathValley"))+
  theme_bw()+
  theme(panel.grid = element_blank())

# save_gg("aa_score_ts.jpeg", aa.score.ts, dir.plot)
# save_gg("ix_score_ts.jpeg", ix.score.ts, dir.plot)

# Model coefficients ---------------------
# Get files
out.files <- list.files(dir.out, recursive = T)
out.files <- out.files[str_detect(out.files, "parameterSummary.csv")]

# Omit pre-2018 results
years <- year(as.Date(str_extract(out.files, pattern = "\\d+-\\d+-\\d+"),
                 format = "%Y-%m-%d"))
out.files <- out.files[which(years >= 2018)]

# Get coefficients
betas <- tibble()
for(i in seq_along(out.files)){
  wee.tab <- read_csv(file.path(dir.out, out.files[i])) %>%
    filter(str_detect(node, "beta")) %>%
    suppressMessages()
  
  betas <- bind_rows(betas, wee.tab)
}

betas <- betas %>%
  select(-(`start.date == start.date`)) %>%
  filter(model=='WithWeatherAndMiceGlobal') %>%
  group_by(siteID, species, node) %>%
  summarise(mean = mean(mean), upper95 = mean(upper95), lower95=mean(lower95)) %>%
  suppressMessages() %>%
  mutate(node = case_when(node == 'beta[1]' ~ 'MaxTempLarvalSurvival',
                          node == 'beta[2]' ~ 'MaxRHLarvalSurvival',
                          node == 'beta[3]' ~ 'MinRHLarvalSurvival',
                          node == 'beta[4]' ~ 'PrecipLarvalSurvival',
                          node == 'beta[5]' ~ 'MaxTempNymphSurvival',
                          node == 'beta[6]' ~ 'MaxRHNymphSurvival',
                          node == 'beta[7]' ~ 'MinRHNymphSurvival',
                          node == 'beta[8]' ~ 'PrecipNymphSurvival',
                          node == 'beta[9]' ~ 'MaxTempAdultSurvival',
                          node == 'beta[10]' ~ 'MaxRHAdultSurvival',
                          node == 'beta[11]' ~ 'MinRHAdultSurvival',
                          node == 'beta[12]' ~ 'PrecipAdultSurvival',
                          node == 'beta[13]' ~ 'MiceLarvaToNymph',
                          node == 'beta[14]' ~ 'MiceNymphToAdult'))

both.plots <- c("BLAN", "LENO", "SCBI", "SERC")
ix.plots <- c("TREE", "HARV", "GREN", "HNRY", "TEA")
aa.plots <- c("KONZ", "OSBS", "TALL", "UKFS")

ix.sub <- betas %>%
  filter(siteID %in% c(ix.plots, both.plots),
         species == "Ixodes scapularis") %>%
  mutate(sig = case_when(lower95<0 & upper95<0 ~ 'sig',
                         lower95>0 & upper95>0 ~ 'sig',
                         TRUE ~ NA),
         node = factor(node))

ix.survival <- ggplot(data = ix.sub%>%filter(str_detect(node, 'Survival')), aes(x = mean, y = siteID))+
  geom_point()+
  geom_point(aes(x = 4.2, shape = sig))+
  geom_errorbar(aes(xmin = lower95, xmax=upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(~node, dir = "v") +
  labs(x = "Coefficient Estimate")+
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = 'none', 
        axis.title.y = element_blank())

ix.transition <- ggplot(data = ix.sub%>%filter(str_detect(node, 'Mice')), aes(x = mean, y = siteID))+
  geom_point()+
  geom_point(aes(x = 2, shape = sig))+
  geom_errorbar(aes(xmin = lower95, xmax=upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(~node, dir = "v") +
  labs(x = "Coefficient Estimate")+
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = 'none', 
        axis.title.y = element_blank())

aa.sub <- betas %>%
  filter(siteID %in% c(aa.plots, both.plots),
         species == "Amblyomma americanum") %>%
  mutate(sig = case_when(lower95<0 & upper95<0 ~ 'sig',
                         lower95>0 & upper95>0 ~ 'sig',
                         TRUE ~ NA),
         node = factor(node))

aa.survival <- ggplot(data = aa.sub%>%filter(str_detect(node, 'Survival')), aes(x = mean, y = siteID))+
  geom_point()+
  geom_point(aes(x = 4, shape = sig))+
  geom_errorbar(aes(xmin = lower95, xmax=upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(~node, dir = "v") +
  labs(x = "Coefficient Estimate")+
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = 'none', 
        axis.title.y = element_blank())

aa.transition <- ggplot(data = aa.sub%>%filter(str_detect(node, 'Mice')), aes(x = mean, y = siteID))+
  geom_point()+
  geom_point(aes(x = 2, shape = sig))+
  geom_errorbar(aes(xmin = lower95, xmax=upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(~node, dir = "v") +
  labs(x = "Coefficient Estimate")+
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = 'none', 
        axis.title.y = element_blank())

# save_gg("aa_survival.jpeg", aa.survival, dir.plot, height = 8)
# save_gg("aa_transition.jpeg", aa.transition, dir.plot, height = 8)
# save_gg("ix_survival.jpeg", ix.survival, dir.plot, height = 8)
# save_gg("ix_transition.jpeg", ix.transition, dir.plot, height = 8)

# Maps?
site.coords <- read_csv("./Data/siteLatLon.csv") %>%
  suppressMessages()

betas.loc <- left_join(betas, site.coords, by = 'siteID') %>%
  filter(str_detect(node, 'Survival')) %>%
  filter(is.na(decimalLongitude)==F) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

state_map <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
state_map <- st_make_valid(st_transform(state_map, crs = 5070))

# Clip map at given latitude
state_map <- st_transform(state_map, crs = 4326)
state_map <- st_crop(state_map, st_bbox(betas.loc))

ix.map <- ggplot(data = betas.loc %>% filter(species == "Ixodes scapularis"))+
  geom_sf(data=state_map)+
  geom_sf(aes(fill = mean), shape = 21, size = 3.5)+
  # geom_sf_text(aes(label = siteID))+
  # scale_shape_manual(values = c(21,24))+
  labs(title = "Ixodes scapularis")+
  scale_fill_distiller(palette="RdBu", direction=1, name = "Mean Estimate")+
  facet_wrap(~node)+
  theme_bw()

aa.map <- ggplot(data = betas.loc %>% filter(species == "Amblyomma americanum"))+
  geom_sf(data=state_map)+
  geom_sf(aes(fill = mean), shape = 21, size = 3.5)+
  # geom_sf_text(aes(label = siteID))+
  # scale_shape_manual(values = c(21,24))+
  labs(title = "Amblyomma americanum")+
  scale_fill_distiller(palette="RdBu", direction=1, name = "Mean Estimate")+
  facet_wrap(~node)+
  theme_bw()

# save_gg("ix_map.jpeg", ix.map, dir.plot, width = 10, height = 8)
# save_gg("aa_map.jpeg", aa.map, dir.plot, width = 10, height = 8)
