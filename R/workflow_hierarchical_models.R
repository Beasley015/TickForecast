## Hierarchical workflow script
## Based on previous work by Foster et al.
## Updated by E.M. Beasley, fall 2025
## 1 - setup
## 3 - state data intake
## 4 - weather data intake
## 5 - create informative priors
## 6 - get initial conditions
## 7 - forecast step
## 7a - mice
## 7b - ticks
## 8 - analysis step
## 9 - save

# =========================================== #
#       1 - setup ------------
# =========================================== #

library(fitdistrplus)
library(tidyverse)
library(lubridate)
library(zoo)
library(nimble)
library(parallel)
library(abind)

options(dplyr.summarise.inform = FALSE)

update <- T

dir.top <- getwd()
dir.out <- file.path(dir.top, "out")

# Define models to run
models <- c("Weather_hierarchicalIntercept", "WeatherMice_hierarchicalIntercept")
species <- c("Ixodes scapularis", "Amblyomma americanum") 

# Create all possible combos
jobs <- expand_grid(
	model = models,
	species = species)

# Vectors of site names
iscap.sites <- c("BLAN","GREN","HARV","HNRY","LENO","SCBI",
                  "SERC","TEA","TREE")
ambly.sites <- c("BLAN","KONZ","LENO","OSBS","SCBI",
                 "SERC","TALL","UKFS")

job.num <- as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(job.num)) {
	job.num <- 2
}

species.job <- jobs$species[job.num] %>%
  str_replace(., " ", "_")
model.job <- jobs$model[job.num]

if(species.job=="Ixodes_scapularis"){
  sites <- iscap.sites
  } else{
    sites <- ambly.sites
}

# Assign variable that will later control whether mice submodel is run (?)
ua.cal <-
	if_else(
		model.job == "WeatherMice_hierarchical",
		"mice_ic_parameter_process",
		"ic_parameter_process"
	)

n.slots <- Sys.getenv("NSLOTS") %>% as.numeric() #Cluster var # of cores
# n.slots <- 2
production <- TRUE
n.iter <- 20000
# n.iter <- 100
# Nmc <- 2000
horizon <- 365

# =========================================== #
#       tick data intake ----------------
# =========================================== #
source("./DataProcessing/functions_hierarchical.R")

# Get tick data based on site
neon.data <- neon_tick_data(species.job) %>% suppressMessages()
# function now retrieves cary sites as well as NEON

# Filter tick data based on job requirements
neon.job <- neon.data %>%
  filter(siteID %in% sites) %>%
	filter(time >= "2016-01-01" & time < "2022-01-01") %>%
	arrange(time)

# Extract sampling dates and number of samples
drag.dates <- as.Date(neon.job$time, format="%Y-%m-%d") %>% unique()
start.date <- first(drag.dates)
n.drags <- length(drag.dates)

# =========================================== #
#       get initial conditions ----------
# =========================================== #

df.latent <- read_csv(file.path("./Data", "dormantNymphTimeSeries.csv"),
                      show_col_types = F)
month.get <- if_else(month(start.date) < 5, 4, month(start.date))
data.latent <- df.latent %>%
	mutate(model = gsub("DormantNymph", "", model)) %>%
  mutate(model = case_when(model == "Weather" ~ "Weather_hierarchicalIntercept",
                           model == "WithWeatherAndMiceGlobal" ~ 
                             "WeatherMice_hierarchicalIntercept",
                           TRUE ~ model)) %>%
	filter(
		model == model.job,
		type == "latent",
		statistic == "conf_50",
		ua == ua.cal,
		month(DATE) == month.get
	) %>%
  mutate(value = case_when(value == 0 ~ 1e-10,
                           TRUE ~ value)) %>%
	group_by(lifeStage) %>%
  summarise(shape=fitdist(value, distr='gamma')[[1]][1], 
            rate=fitdist(value, distr='gamma')[[1]][2]) %>%
  suppressWarnings() %>%
  pivot_wider(names_from = lifeStage, values_from = c(shape,rate))

IC <- tibble(
	shape = c(
		pull(data.latent, shape_larvae),
		pull(data.latent, shape_dormant),
		pull(data.latent, shape_nymphs),
		pull(data.latent, shape_adults)
	),
	rate = c(
		pull(data.latent, rate_larvae),
		pull(data.latent, rate_dormant),
		pull(data.latent, rate_nymphs),
		pull(data.latent, rate_adults)
	)
) %>%
	as.matrix()

IC <- array(IC, dim = c(nrow(IC), ncol(IC), length(sites)))

# =========================================== #
#       mouse data intake ---------------
# =========================================== #

source("./DataProcessing/capture_matrix_hierarchical.R")

smam_cary <- read_csv("./Data/cary_mouse_formatted.csv",
                   show_col_types=F)
smam_neon <- read_csv("./Data/allSmallMammals.csv",
                   show_col_types=F)
               
ch.ls <- capture_matrix(smam_neon, sites=sites)

ch <- ch.ls$ch %>%
  mutate_at(.vars = vars(-siteID), ~case_when(. %in% 1:3 ~ 1,
                                              TRUE ~ 0)) %>%
  mutate(ncaps = rowSums(.[2:ncol(.)])) %>%
  filter(ncaps > 0) %>%
  dplyr::select(-ncaps) %>%
  arrange(siteID)

# mna: NEON
ks <- known_states(ch)
mna <- ks %>%
  group_by(siteID) %>%
  summarise(across(.cols = everything(), sum)) %>%
  pivot_longer(cols = -siteID, names_to = "collectDate", values_to = "MNA") %>%
  mutate(collectDate = as.Date(collectDate, format = "%Y-%m-%d"))

# Add Cary mna
mna.full <- smam_cary %>%
  dplyr::select(-plotID) %>%
  filter(collectDate >= ymd("2013-01-01")) %>%
  full_join(mna, by = c("siteID", "collectDate", "MNA")) %>%
  pivot_wider(id_cols = siteID, names_from = collectDate, values_from = MNA,
              values_fn = sum) %>%
  mutate(pmap_df(., ~ na.locf(c(...)[-1]))) %>%
  mutate(across(-siteID, .fns = as.numeric)) %>%
  arrange(siteID)

mice.obs <- ymd(colnames(mna.full)[-1]) # unique sampling days: mice

# every day in mouse sequence
mice.seq <- seq.Date(mice.obs[1], mice.obs[length(mice.obs)], by = 1)

mna.all.days <- matrix(NA, ncol = length(mice.seq), nrow = nrow(mna.full))
mna.count <- 1
for (i in seq_along(mice.seq)) {
	if (mice.seq[i] %in% mice.obs) {
		mna.all.days[,i] <- pull(mna.full, colnames(mna.full)[mna.count+1])
		mna.count <- mna.count + 1
	} else {
		mna.all.days[,i] <- pull(mna.full, colnames(mna.full)[mna.count+1])
	}
}

# historical mna
mna.hist <- mna_jags("Green Control", return.mean = TRUE)

# center and scale
mna.scaled <- as.data.frame((mna.all.days-mna.hist$mean)/mna.hist$sd)
colnames(mna.scaled) <- mice.seq

mna.scaled <- mna.scaled %>%
  mutate(siteID = mna.full$siteID) %>%
  pivot_longer(cols=-siteID, names_to="Date", values_to = "mna_scaled")

# =========================================== #
#       daymet intake and correction -------------
# =========================================== #
source("./DataProcessing/daymet_downscale_hierarchical.R")

cgdd <- daymet_cumGDD(sites=sites) %>%
  ungroup() %>%
  filter(year(Date) < 2022) %>%
  dplyr::select(-year) %>%
  suppressMessages()

maxTemp <- daymet_temp(sites=sites, minimum = FALSE) %>%
  ungroup() %>%
  dplyr::select(Date, siteID, maxTempCorrect) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(year(Date) < 2022) %>%
  suppressMessages()

rh <- daymet_rh(sites) %>%
    dplyr::select(Date, maxRHCorrect, minRHCorrect, siteID) %>%
  filter(year(Date) < 2022) %>%
    suppressMessages()

precip <- daymet_precip(sites) %>%
    dplyr::select(Date, precipitation, siteID) %>%
  filter(year(Date) < 2022) %>%
    suppressMessages()
 
# Combine all met variables
join1 <- left_join(maxTemp, rh, by = c("Date", "siteID"))
join2 <- left_join(join1, precip, by = c("Date", "siteID"))
 
# Scale met data based on historical means
hist.means <- scale_met_forecast()
  
df.daymet <- join2 %>%
  mutate(
    maxTempScale = (maxTempCorrect - hist.means$means["MAX_TEMP"]) /
        hist.means$sds["MAX_TEMP"],
    maxRHScale = (maxRHCorrect - hist.means$means["MAX_RH"]) /
        hist.means$sds["MAX_RH"],
    minRHScale = (minRHCorrect - hist.means$means["MIN_RH"]) /
        hist.means$sds["MIN_RH"],
    precipScale = (precipitation - hist.means$means["TOT_PREC"]) /
        hist.means$sds["TOT_PREC"]
  ) %>%
  ungroup() %>%
  dplyr::select(Date, siteID, contains("Scale")) %>%
  filter(Date >= "2016-01-01" & Date < "2022-01-01")

# =========================================== #
#       get informative priors -------------------
# =========================================== #
df.params <- read_csv(file.path("./Data/dormantNymphParams.csv"),
                      show_col_types = F)

params.stats <- df.params %>%
  mutate(model = case_when(model == "Weather" ~ "Weather_hierarchicalIntercept",
                           model == "WithWeatherAndMiceGlobal" ~
                             "WeatherMice_hierarchicalIntercept",
                           TRUE ~ model)) %>%
	filter(model == model.job) %>%
	dplyr::select(parameter, value) %>%
	group_by(parameter) %>%
	summarise(mu = mean(value), 
	          tau = 1 / var(value))

get_prior <- function(name) {
	pr <- numeric(2)
	xx <- params.stats %>%
		filter(parameter == name)
	
	if(str_detect(name, "prec")==T){
	  pr[1] <- xx %>% pull(alpha)
	  pr[2] <- xx %>% pull(beta)
	} else{
	  pr[1] <- xx %>% pull(mu)
	  pr[2] <- xx %>% pull(tau)
	  pr
	}
}

# Get informative priors for mean model parameters
# and start with uninformative priors for precision
phi.l <- get_prior("phi.l.mu")
phi.n <- get_prior("phi.n.mu")
phi.a <- get_prior("phi.a.mu")

prec.l <- c(1,1)
prec.n <- c(1,1)
prec.a <- c(1,1)

theta.l2n <- get_prior("theta.ln")
theta.n2a <- get_prior("theta.na")

prec.l2n <- c(1,1)
prec.n2a <- c(1,1)

repro <- get_prior("repro.mu")
repro.mu <- repro[1] # goes to reproduction portion of transition matrix

# I don't like how covariates are annotated: all are just "beta"
# Will fix once model comparisons are complete
n.beta <- params.stats %>%
	filter(grepl("beta", parameter)) %>%
	nrow()

get_beta <- function(model.job) {
	if (model.job == "Static") {
		return(NA)
	} else {
		pr.beta <- matrix(NA, n.beta, 2)
		n.beta <- params.stats %>%
			filter(grepl("beta", parameter)) %>%
			nrow()

		pr.beta <- matrix(NA, n.beta, 2)
		for (i in seq_len(n.beta)) {
			if (model.job == "WithMNAMice") {
				node <- paste0("beta.m[", i, "]")
			} else {
				node <- paste0("beta[", i, "]")
			}
			pr.beta[i, ] <- get_prior(node)
		}
		return(pr.beta)
	}
}

pr.beta <- get_beta(model.job)

# function to approximate moment the inverse gamma
inv_gamma_mm <- function(x) {
	mu <- mean(x)
	v <- var(x)
	alpha <- (mu^2 / v) + 2
	beta <- mu * ((mu^2 / v) + 1)
	return(c("alpha" = alpha, "beta" = beta))
}

# get invgamma parameters
pr.sig <- df.params %>%
  mutate(model = case_when(model == "Weather" ~ "Weather_hierarchicalIntercept",
                           model == "WithWeatherAndMiceGlobal" ~
                             "WeatherMice_hierarchicalIntercept",
                           TRUE ~ model)) %>%
	filter(model == model.job, grepl("sig", parameter)) %>%
	dplyr::select(parameter, value) %>%
	group_by(parameter) %>%
	summarise(alpha = inv_gamma_mm(value)[1], beta = inv_gamma_mm(value)[2])


# iterate ===============================================================

# Make sure start is on the first time step
t = 1

# Define directories
dir.base <- file.path(
  dir.out,
  species.job,
  model.job
)

dir.save <- file.path(dir.base)

# Filter drag dates and # drags so 1 week is analyzed at a time
weekly <- logical(length = length(drag.dates))
weekly[1] <- T
for(n in 2:n.drags){
  weekly[n] <- ifelse(drag.dates[n]-drag.dates[n-1] < 7, F, T)
}

start.dates <- drag.dates[weekly]
start.drags <- length(start.dates)

# Pick up where you left off if an update
if(update == T){
  comp.dates <- list.dirs(path=dir.save)[-1]
  comp.dates <- str_extract(comp.dates, pattern = "\\d+-\\d+-\\d+")
  
  comp.dates <- comp.dates[which(comp.dates %in% start.dates)]
  
  t <- t+length(comp.dates)
}

for (t in t:start.drags) { 
	fx.start.date <- start.dates[t]
	message("---------------------------------------------------")
	mm <- paste(fx.start.date, " (", round(t / start.drags * 100, 2), "%)")
	message(mm)

  # flags for if statements
	miceAndWeather <- model.job == "WeatherMice_hierarchicalIntercept"
	use.daymet <- grepl("Weather", model.job)

	# initialize nimble lists
	constants <- data <- list()

	if (t == 1) {
		fx.sequence <- seq.Date(fx.start.date, by = 1, length.out = horizon)
		
		phi.l.obs <- matrix(nrow=length(sites), ncol = 2)
		phi.n.obs <- matrix(nrow=length(sites), ncol = 2)
		phi.a.obs <- matrix(nrow=length(sites), ncol = 2)
		theta.ln.obs <- matrix(nrow=length(sites), ncol = 2)
		theta.na.obs <- matrix(nrow=length(sites), ncol = 2)
		
		} else {
		  horizon <- ifelse(as.numeric(last(drag.dates) - fx.start.date) >= 365,
		                    365,
		                    as.numeric(last(drag.dates) - fx.start.date))

			# read last forecast parameters and state
			readDest <- file.path(
				dir.base,
				start.dates[t - 1]
			)

			last.params <- read_csv(file.path(readDest, "parameterSamples.csv")) %>%
				suppressMessages()
			
			# Get "observed" site-level values 
			obs.params <- last.params %>%
			  pivot_longer(cols=!(node), names_to="site", values_to="value") %>%
			  rename("parameter" = "node") %>%
			  group_by(parameter, site) %>%
			  summarise(mu = mean(value), tau = 1 / var(value))
			
			phi.a.obs <- filter(obs.params, parameter == "phi.a.mu") %>%
			  ungroup() %>%
			  select(mu, tau) %>%
			  as.matrix()
			
			phi.n.obs <- filter(obs.params, parameter == "phi.n.mu") %>%
			  ungroup() %>%
			  select(mu, tau) %>%
			  as.matrix()
			
			phi.l.obs <- filter(obs.params, parameter == "phi.l.mu") %>%
			  ungroup() %>%
			  select(mu, tau) %>%
			  as.matrix()
			
			theta.ln.obs <- filter(obs.params, parameter == "theta.ln") %>%
			  ungroup() %>%
			  select(mu, tau) %>%
			  as.matrix()
			
			theta.na.obs <- filter(obs.params, parameter == "theta.na") %>%
			  ungroup() %>%
			  select(mu, tau) %>%
			  as.matrix()

			# get parameter posterior summary
			params.stats <- last.params %>%
			  pivot_longer(cols=!(node), names_to="site", values_to="value") %>%
			  rename("parameter" = "node") %>%
				group_by(parameter) %>%
				summarise(mu = mean(value), tau = 1 / var(value))

			# Priors for interepts (priors are constant across sites)
			phi.l <- get_prior("phi.l.mu")
			phi.n <- get_prior("phi.n.mu")
			phi.a <- get_prior("phi.a.mu")
			
			theta.l2n <- get_prior("theta.ln")
			theta.n2a <- get_prior("theta.na")
			
			# Precision priors
			prec.params <- read_csv(file.path(readDest, "precSamples.csv")) %>%
			  group_by(node) %>%
			  summarise(mu = mean(value), v = var(value)) %>%
			  mutate(alpha = (mu^2 / v) + 2,
			         beta = mu * ((mu^2 / v) + 1)) %>%
			  suppressMessages() 
			
			prec.l <- c(prec.params$alpha[2], prec.params$beta[2])
			prec.n <- c(prec.params$alpha[3], prec.params$beta[3])
			prec.a <- c(prec.params$alpha[1], prec.params$beta[1])
			
			prec.l2n <- c(prec.params$alpha[4], prec.params$beta[4])
			prec.n2a <- c(prec.params$alpha[5], prec.params$beta[5])

			# Priors for betas
			betas <- read_csv(file.path(readDest, "beta.csv")) %>%
				rename("parameter" = "node") %>%
				group_by(parameter) %>%
				summarise(mu = mean(value), tau = 1/var(value)) %>%
				suppressMessages()

			pr.beta <- matrix(NA, n.beta, 2)
			for (i in seq_len(n.beta)) {
    		pr <- numeric(2)
				xx <- betas %>% filter(parameter == paste("beta", i, sep = ""))
				pr[1] <- xx %>% pull(mu)
				pr[2] <- xx %>% pull(tau)
				pr.beta[i, ] <- pr
			}

			# get invgamma parameters
			sig.last <- read_csv(file.path(readDest, "sigma.csv")) %>%
			  suppressMessages()
			
			pr.sig <- sig.last %>%
				group_by(parameter) %>%
				summarise(
					alpha = inv_gamma_mm(value)[1],
					beta = inv_gamma_mm(value)[2]
				)

			last.fx <- read_csv(file.path(readDest, "stateSamples.csv")) %>%
			  pivot_longer(cols=Larva:Adult, names_to = 'lifeStage', 
			               values_to='value') %>%
				suppressMessages()

			tick.stats <- last.fx %>%
				filter(time == fx.start.date) %>%
			  mutate(value = case_when(value < 0 ~ 0,
			                   TRUE ~ value)) %>%
				group_by(lifeStage, time, siteID) %>%
			  summarise(shape=fitdist(value, distr='gamma', lower = c(0,0))[[1]][1], 
			            rate=fitdist(value, distr='gamma', lower = c(0,0))[[1]][2]) %>%
			  suppressWarnings()

			IC <- array(NA, dim = c(4, 2, length(sites)))
			
			for(i in 1:length(sites)){
			  IC[1, 1, i] <- tick.stats %>% 
			    filter(lifeStage == "Larva" & siteID==sites[i]) %>% 
			    pull(shape)
			  
			  IC[1, 2, i] <- tick.stats %>% 
			    filter(lifeStage == "Larva" & siteID==sites[i]) %>% 
			    pull(rate)
			
			  IC[2, 1, i] <- tick.stats %>% 
			    filter(lifeStage == "Dormant"& siteID==sites[i]) %>% 
			    pull(shape)
			
			  IC[2, 2, i] <- tick.stats %>% 
			    filter(lifeStage == "Dormant" & siteID==sites[i]) %>% 
			    pull(rate)
			
			  IC[3, 1, i] <- tick.stats %>% 
			    filter(lifeStage == "Nymph" & siteID==sites[i]) %>% 
			    pull(shape)
			
			  IC[3, 2, i] <- tick.stats %>% 
			    filter(lifeStage == "Nymph" & siteID==sites[i]) %>% 
			    pull(rate)
			
			  IC[4, 1, i] <- tick.stats %>% 
			    filter(lifeStage == "Adult" & siteID==sites[i]) %>% 
			    pull(shape)
			
			  IC[4, 2, i] <- tick.stats %>% 
			    filter(lifeStage == "Adult" & siteID==sites[i]) %>% 
			    pull(rate)
			}

			fx.sequence <- seq.Date(fx.start.date, by = 1, length.out = horizon)
			n.days <- length(fx.sequence)
		}

	if (use.daymet) {
		# Filter dates that don't have corresponding drags
		daymet.sub <- df.daymet %>%
			filter(Date %in% fx.sequence)
			
		# Create day x site matrices for each variable
		data$maxtemp <- daymet.sub %>% 
			dplyr::select(Date, siteID, maxTempScale) %>%
			pivot_wider(names_from= siteID, values_from = maxTempScale,
		              values_fill=NA) %>%
			dplyr::select(-Date) %>%
			as.matrix()
			
		data$maxrh <- daymet.sub %>%
		  dplyr::select(Date, siteID, maxRHScale) %>%
			pivot_wider(names_from=siteID, values_from=maxRHScale,
			           values_fill=NA) %>%
			dplyr::select(-Date) %>%
			as.matrix()
			
		data$minrh <- daymet.sub %>% 
      dplyr::select(Date, siteID, minRHScale) %>%
	    pivot_wider(names_from=siteID, values_from=minRHScale,
	                values_fill=NA) %>%
		  dplyr::select(-Date) %>%
		  as.matrix()
			
		data$precip <- daymet.sub %>% 
		  dplyr::select(Date, siteID, precipScale) %>%
			pivot_wider(names_from=siteID, values_from=precipScale,
			            values_fill=NA) %>%
			dplyr::select(-Date) %>%
			as.matrix()
	}
    
	# Get observational data
	obs.dates <- as.Date(fx.start.date:(fx.start.date+6),
	                     format = "%Y-%m-%d")
	obs <- neon.job %>%
	  filter(time %in% obs.dates) %>%
	  mutate(index = match(time, obs.dates))
	
	# Get number of plots per site
	plots <- neon.job %>%
	  dplyr::select(siteID, plotID) %>%
	  filter(siteID %in% sites) %>%
	  distinct() 
	
	# ID and number of plots
	plot.names <- unique(plots$plotID)
	
  n.plots <- plots %>%
	  group_by(siteID) %>%
	  distinct() %>%
	  summarise(nplot=n()) %>%
	  pull(nplot)

	# Set up observation matrices
	y <- array(NA, dim = c(4, horizon, max(n.plots), length(sites)))
	area <- array(NA, dim = c(horizon, max(n.plots), length(sites)))
	
	for(site in 1:length(sites)){
	  # Filter observations from site
	  obs.site <- obs %>%
	    filter(siteID == sites[site])

	  if(nrow(obs.site) != 0){  
	    # Get all possible plots
	    plt.t <- plots %>%
	      filter(siteID == sites[site])
	    
		  for (p in 1:length(plt.t$plotID)) {
			  obs.plot <- obs.site %>% 
			    filter(plotID == plt.t$plotID[p])
			
			  if(nrow(obs.plot != 0)){
			    y[1, obs.plot$index, p, site] <- obs.plot %>% pull(Larva)
			    y[3, obs.plot$index, p, site] <- obs.plot %>% pull(Nymph)
			    y[4, obs.plot$index, p, site] <- obs.plot %>% pull(Adult)
			    area[obs.plot$index, p, site] <- obs.plot %>% pull(totalSampledArea)
			  }
		  }
	  }
	}

	# finalize data
	data$y <- y
	data$area <- area
	data$IC <- IC
	
	data$phi.l.obs <- phi.l.obs
	data$phi.n.obs <- phi.n.obs
	data$phi.a.obs <- phi.a.obs
	
	data$pr.phi.l <- phi.l
	data$pr.phi.n <- phi.n
	data$pr.phi.a <- phi.a
	data$prec.l <- prec.l
	data$prec.n <- prec.n
	data$prec.a <- prec.a
	
	data$theta.ln.obs <- theta.ln.obs
	data$theta.na.obs <- theta.na.obs
	
	data$pr.theta.l2n <- theta.l2n
	data$pr.theta.n2a <- theta.n2a
	data$prec.ln <- prec.l2n
	data$prec.na <- prec.n2a
	
	data$repro.mu <- repro.mu
	
	data$pr.beta <- pr.beta
	data$pr.sig <- pr.sig %>% dplyr::select(-parameter) %>% as.matrix()
	
	# Cumulative degree days
	data$cgdd <- cgdd %>%
	  filter(Date %in% ymd(fx.sequence)) %>%
	  pivot_wider(names_from=siteID, values_from=cumGDD) %>%
	  dplyr::select(-Date)
	
	# data$xind <- array(1, dim=c(4, horizon, length(sites)))

	if (miceAndWeather){
	  data$mice <- mna.scaled %>%
	    filter(Date %in% fx.sequence) %>%
	    pivot_wider(names_from=siteID, values_from=mna_scaled) %>%
	    dplyr::select(-Date) %>%
	    relocate(all_of(sites)) %>%
	    as.matrix() %>%
	    suppressMessages()

	  if (nrow(data$mice) < length(fx.sequence)) {
	    horizon <- min(length(data$cgdd), hrow(data$mice))
	    data$y <- y[, 1:horizon, ,]
	    fx.sequence <- fx.sequence[1:nrow(data$mice)]
	  }
	}

	if (year(fx.start.date) == max(year(neon.job$time))) {
	  if (model.job == "Weather_hierarchicalIntercept") {
	    # Make sure horizon does not go past empirical data
	    horizon <- nrow(data$cgdd)
	    data$y <- as.array(y[, 1:horizon, ,])
	    
	    # Fix error arising from single-site models, may remove
	    # if(is.na(dim(data$y)[3]==T)){
	    #   dim(data$y)[3] <- 1
	    # }
	    
	    } else {
	      horizon <- min(nrow(data$cgdd), nrow(data$mice))
				data$y <- y[, 1:horizon, ,]
	    }
	}

	# finalize constants
	constants$n.beta <- n.beta
	constants$nsite <- length(sites)
	constants$n.plots <- n.plots
	constants$horizon <- horizon
	constants$ns <- 4
	# constants$max.cgdd <- cgdd %>%
	#   group_by(siteID) %>%
	#   filter(Date >= min(fx.sequence)) %>%
	#   mutate(max.cgdd = cumGDD*1.2) %>%
	#   summarise(min = min(max.cgdd), max = max(max.cgdd)) %>%
	#   select(-siteID) %>%
	#   t()
	
	# Initialize area
	area.init <- area
	nai <- which(is.na(area))	
	area.init[nai] <- 160
	area.init[-nai] <- NA
		
	# Initial values to send to nimble
	inits <- function() {
	  list(
	    area = area.init,
	    phi.l.obs = matrix(c(rnorm(length(sites)), rgamma(n=length(sites),1)), ncol = 2),
	    phi.n.obs = matrix(c(rnorm(length(sites)), rgamma(n=length(sites),1)), ncol = 2),
	    phi.a.obs = matrix(c(rnorm(length(sites)), rgamma(n=length(sites),1)), ncol = 2),
			phi.l.mu = rep(rnorm(1, phi.l[1], 1 / sqrt(phi.l[2])), length(sites)),
			phi.n.mu = rep(rnorm(1, phi.n[1], 1 / sqrt(phi.n[2])), length(sites)),
			phi.a.mu = rep(rnorm(1, phi.a[1], 1 / sqrt(phi.a[2])), length(sites)),
			phi.l.prec = rgamma(1,1,1),
			phi.n.prec = rgamma(1,1,1),
			phi.a.prec = rgamma(1,1,1),
			theta.ln.obs = matrix(c(rnorm(length(sites)), rgamma(n=length(sites),1)), ncol = 2),
			theta.na.obs = matrix(c(rnorm(length(sites)), rgamma(n=length(sites),1)), ncol = 2),
			theta.ln = rep(rnorm(1, theta.l2n[1], 1 / sqrt(theta.l2n[2])), 
			               length(sites)),
			theta.na = rep(rnorm(1, theta.n2a[1], 1 / sqrt(theta.n2a[2])), 
			               length(sites)),
			theta.ln.prec = rgamma(1,1,1),
			theta.na.prec = rgamma(1,1,1),
			beta = rep(rnorm(n.beta, pr.beta[, 1], 1 / sqrt(pr.beta[, 2]))),
			sig = matrix(rinvgamma(4*length(sites), pr.sig$alpha, pr.sig$beta),
			             nrow=4, ncol=length(sites)),
			x = array(abs(rnorm(n=4*horizon*length(sites), mean=2)) / 160 * 450, 
			          dim=c(4, horizon, length(sites))),
			Ex = array(rpois(4 * horizon * length(sites), 2) / 160 * 450,
			           dim=c(4, horizon, length(sites))),
			y = array(rep(0, 4*horizon*max(n.plots)*length(sites)),
			          dim = dim(data$y)),
			tau.temp = rexp(length(sites)),
			tau.maxrh = rexp(length(sites)),
			tau.minrh = rexp(length(sites)),
			tau.precip = rexp(length(sites)),
			x1 = jitter(data$maxtemp),
			x2 = jitter(data$maxrh),
			x3 = jitter(data$minrh),
			x4 = jitter(data$precip),
			OMEGA = array(0, dim=c(4,4,length(sites))),
			A = array(0, dim=c(4, 4, horizon, length(sites)))
		)
	}
	
	params.to.save <-  c("beta", "phi.a.mu", "phi.l.mu", "phi.n.mu", "phi.l.prec",
	                     "phi.n.prec", "phi.a.prec", "sig",
	                     # "tau.maxrh", "tau.minrh", "tau.precip", "tau.temp", 
	                     "theta.ln", "theta.na", "theta.ln.prec", "theta.na.prec",
	                     "x" #, "x1", "x2", "x3", "x4"
	                     )     

	# start <- Sys.time()
	source("./R/nimble_forecast_hierarchical.R")
	source("./R/run_transfer_nimble_hierarchical.R")
	cl <- makeCluster(n.slots) 
	
	# Run the model	
	out.nchains <- run_transfer_nimble(
		cl = cl,
		model = model.code,
		data = data,
		constants = constants,
		inits = inits,
		n.iter = n.iter,
		parms = params.to.save,
		miceAndWeather = miceAndWeather,
		use.daymet = use.daymet
		) 
	
	stopCluster(cl)
	
	# end <- Sys.time()
	
	# end-start
  
	# Merge outputs of chains
	dat.hindcast <- list()
	for(i in seq_along(names(out.nchains[[1]]))){
	  if(length(dim(out.nchains[[1]][[i]]))==2){
	    dat.hindcast[[i]] <- do.call(rbind, lapply(out.nchains, `[[`, i))
	  } else{
	    dat.hindcast[[i]] <- do.call(abind, list(lapply(out.nchains, `[[`, i),
	                                 along = 1))
	  }
	}
	
	names(dat.hindcast) <- names(out.nchains[[1]])

	# Test MCMC convergence with Gelman-Rubin statistic
	if(year(fx.start.date)>=2018){
	  message("Checking convergence...")
	  nodes <- names(dat.hindcast)
	  nodes <- nodes[nodes %in% c("beta", "x", "x1", "x2", "x3", "x4", 
	                              nodes[str_detect(nodes, "phi")], 
	                              nodes[str_detect(nodes, "theta")])]
	  gelman.keep <-list()
	
	  for (ff in seq_along(nodes)) {
	    mcmc.check <- list()
		  col <- nodes[ff]
		
		  if(length(dim(out.nchains[[1]][[nodes[ff]]]))!=2){next}
				
		  for (c in seq_along(out.nchains)) {
		      mcmc.check[[c]] <- coda::mcmc(out.nchains[[c]][[col]])
		  }
		
		  gelman.keep[[ff]] <- try(coda::gelman.diag(mcmc.check, 
		                                           transform = TRUE)$psrf[,1])
		
		  if(all(is.na(gelman.keep[[ff]]=='character'))){next}
		  if(typeof(gelman.keep[[ff]])=='character'){next}

		  if (any(gelman.keep[[ff]] > 1.2)) {
		    message("WARNING: Convergence not reached!")
		    bad.nodes <- which(gelman.keep[[ff]] > 1.2)
		    bad.params <- tibble(node = nodes[[ff]],
		      psrf = as.numeric(gelman.keep[[ff]][bad.nodes])) %>%
		      arrange(psrf)
				  print(tail(bad.params))
		  
		    } else {
		      # message("Convergence = TRUE")
		    }
	  }
	}
  
	# Thin the chains
	dat.draws <- list()
	for(i in 1:length(dat.hindcast)){
	  if (nrow(dat.hindcast[[i]]) > 5000) {
	    draws <- round(seq.int(1, nrow(dat.hindcast[[i]]), length.out = 5000))
    } else {
		  draws <- seq_len(nrow(dat.hindcast[[i]]))
    }

	  if(length(dim(dat.hindcast[[i]]))==2){
	    dat.draws[[i]] <- dat.hindcast[[i]][draws, ]
	  } else if(length(dim(dat.hindcast[[i]]))==3){
	    dat.draws[[i]] <- dat.hindcast[[i]][draws,,]
	  } else{
	    dat.draws[[i]] <- dat.hindcast[[i]][draws,,,]
	  }
	}
	
	names(dat.draws) <- names(dat.hindcast)

	# Output processing and save
	fileDest <- file.path(dir.save, fx.start.date)
	
	message("Running analysis...")
	
	transfer_analysis(
	  fx.df = dat.draws,
		observations = neon.job,
		fx.dates = fx.sequence,
		model = model.job,
		spp = species.job,
		horizon = horizon,
		weather = use.daymet,
		out.dir = fileDest)
	
	# Clear environment
	rm(out.nchains, dat.hindcast, dat.draws)
}

