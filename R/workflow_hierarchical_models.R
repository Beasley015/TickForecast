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

library(tidyverse)
library(lubridate)
library(zoo)
library(nimble)
library(parallel)

options(dplyr.summarise.inform = FALSE)

update <- FALSE

dir.top <- getwd()
dir.out <- file.path(dir.top, "out")
if (update) {
	dir.out <- paste0(dir.out, "Update")
}

# Define models to run
models <- c("Weather_hierarchical", "WeatherMice_hierarchical")
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
	job.num <- 1
}

species.job <- jobs$species[job.num] %>%
  str_replace(., " ", "_")
model.job <- jobs$model[job.num]

if(species.job=="Ixodes scapularis"){
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

# n.slots <- Sys.getenv("NSLOTS") %>% as.numeric() #Cluster var # of cores
n.slots <- 2
production <- TRUE
n.iter <- 1000 #50000
Nmc <- 2000
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
  mutate(model = case_when(model == "Weather" ~ "Weather_hierarchical",
                           model == "WeatherAndMiceGlobal" ~ 
                             "WeatherMice_hierarchical",
                           TRUE ~ model)) %>%
	filter(
		model == model.job,
		type == "latent",
		statistic == "conf_50",
		ua == ua.cal,
		month(DATE) == month.get
	) %>%
	group_by(lifeStage) %>%
	summarise(mu = mean(value), prec = 1 / var(value)) %>%
	pivot_wider(names_from = lifeStage, values_from = c(mu, prec))

IC <- tibble(
	mu = c(
		pull(data.latent, mu_larvae),
		pull(data.latent, mu_dormant),
		pull(data.latent, mu_nymphs),
		pull(data.latent, mu_adults)
	),
	prec = c(
		pull(data.latent, prec_larvae),
		pull(data.latent, prec_dormant),
		pull(data.latent, prec_nymphs),
		pull(data.latent, prec_adults)
	)
) %>%
	as.matrix()

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
  mutate(ncaps = rowSums(.[2:156])) %>%
  filter(ncaps > 0) %>%
  select(-ncaps)

# mna: NEON
ks <- known_states(ch)
mna <- ks %>%
  group_by(siteID) %>%
  summarise(across(.cols = everything(), sum)) %>%
  pivot_longer(cols = -siteID, names_to = "collectDate", values_to = "MNA") %>%
  mutate(collectDate = as.Date(collectDate, format = "%Y-%m-%d"))

# Add Cary mna
mna.full <- smam_cary %>%
  select(-plotID) %>%
  filter(collectDate >= ymd("2013-01-01")) %>%
  full_join(mna, by = c("siteID", "collectDate", "MNA")) %>%
  pivot_wider(id_cols = siteID, names_from = collectDate, values_from = MNA,
              values_fn = sum) %>%
  mutate(pmap_df(., ~ na.locf(c(...)[-1]))) %>%
  mutate(across(-siteID, .fns = as.numeric))

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
mna.scaled <- as.data.frame(mna.all.days-mna.hist$mean/mna.hist$sd)
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
  select(-year) %>%
  suppressMessages()

maxTemp <- daymet_temp(sites=sites, minimum = FALSE) %>%
  ungroup() %>%
  select(Date, siteID, maxTempCorrect) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  suppressMessages()

rh <- daymet_rh(sites) %>%
    select(Date, maxRHCorrect, minRHCorrect, siteID) %>%
    suppressMessages()

precip <- daymet_precip(sites) %>%
    select(Date, precipitation, siteID) %>%
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
  select(Date, siteID, contains("Scale")) %>%
  filter(Date >= "2016-01-01" & Date < "2022-01-01")

# =========================================== #
#       get informative priors -------------------
# =========================================== #
df.params <- read_csv(file.path("./Data/dormantNymphParams.csv"),
                      show_col_types = F)

params.stats <- df.params %>%
  mutate(model = case_when(model == "Weather" ~ "Weather_hierarchical",
                           model == "WithWeatherAndMiceGlobal" ~
                             "WeatherMice_hierarchical",
                           TRUE ~ model)) %>%
	filter(model == model.job) %>%
	select(parameter, value) %>%
	group_by(parameter) %>%
	summarise(mu = mean(value), tau = 1 / var(value))

get_prior <- function(name) {
	pr <- numeric(2)
	xx <- params.stats %>%
		filter(parameter == name)
	pr[1] <- xx %>% pull(mu)
	pr[2] <- xx %>% pull(tau)
	pr
}

# Get informative priors for model parameters
# All site priors drawn from dist with the below parameters
phi.l <- get_prior("phi.l.mu")
phi.n <- get_prior("phi.n.mu")
phi.a <- get_prior("phi.a.mu")
theta.l2n <- get_prior("theta.ln")
theta.n2a <- get_prior("theta.na")
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
  mutate(model = case_when(model == "Weather" ~ "Weather_hierarchical",
                           model == "WithWeatherAndMiceGlobal" ~
                             "WeatherMice_hierarchical",
                           TRUE ~ model)) %>%
	filter(model == model.job, grepl("sig", parameter)) %>%
	select(parameter, value) %>%
	group_by(parameter) %>%
	summarise(alpha = inv_gamma_mm(value)[1], beta = inv_gamma_mm(value)[2])


# iterate ======================================================================================

# Make sure start is on the first time step
t = 1

for (t in seq_len(n.drags)) {
	fx.start.date <- drag.dates[t]
	message("---------------------------------------------------")
	mm <- paste(fx.start.date, " (", round(t / n.drags * 100, 2), "%)")
	message(mm)

  # flags for if statements
	miceAndWeather <- model.job == "WeatherMice_hierarchical"
	use.daymet <- grepl("Weather", model.job)

	dir.base <- file.path(
		dir.out,
		species.job,
		model.job
	)
	
	dir.save <- file.path(dir.base)

	# initialize nimble lists
	constants <- data <- list()

	if (t == 1) {
		fx.sequence <- seq.Date(fx.start.date, by = 1, length.out = horizon)
		} else { # EDIT AFTER 1 SUCCESSFUL TIME STEP ---------------------------
			# read last forecast parameters and state
			readDest <- file.path(
				dir.base,
				drag.dates[t - 1]
			)

			last.params <- read_csv(file.path(readDest, "parameterSamples.csv")) %>%
				suppressMessages()

			# get parameter posterior summary
			params.stats <- last.params %>%
				rename(parameter = node) %>%
				select(parameter, value) %>%
				group_by(parameter) %>%
				summarise(mu = mean(value), tau = 1 / var(value))

			if (update) {
				phi.l <- get_prior("phi.l.mu")
				phi.n <- get_prior("phi.n.mu")
				phi.a <- get_prior("phi.a.mu")
				theta.l2n <- get_prior("theta.ln")
				theta.n2a <- get_prior("theta.na")
				# repro <- get_prior("repro.mu")

				if (model.job != "Static") {
					pr.beta <- matrix(NA, n.beta, 2)
					for (i in seq_len(n.beta)) {
						pr.beta[i, ] <- get_prior(paste0("beta[", i, "]"))
					}
				}

				# get invgamma parameters
				pr.sig <- last.params %>%
					rename(parameter = node) %>%
					filter(grepl("sig", parameter)) %>%
					select(parameter, value) %>%
					group_by(parameter) %>%
					summarise(
						alpha = inv_gamma_mm(value)[1],
						beta = inv_gamma_mm(value)[2]
					)
			}

			last.fx <- read_csv(file.path(readDest, "stateSamples.csv")) %>%
				suppressMessages()

			tick.stats <- last.fx %>%
				filter(time == fx.start.date) %>%
				group_by(lifeStage, time) %>%
			  summarise(mu = mean(value), tau = 1 / var(value))

			IC <- matrix(NA, 4, 2)
			IC[1, 1] <- tick.stats %>% filter(lifeStage == "Larva") %>% pull(mu)
			IC[1, 2] <- tick.stats %>% filter(lifeStage == "Larva") %>% pull(tau)
			IC[2, 1] <- tick.stats %>% filter(lifeStage == "Dormant") %>% pull(mu)
			IC[2, 2] <- tick.stats %>% filter(lifeStage == "Dormant") %>% pull(tau)
			IC[3, 1] <- tick.stats %>% filter(lifeStage == "Nymph") %>% pull(mu)
			IC[3, 2] <- tick.stats %>% filter(lifeStage == "Nymph") %>% pull(tau)
			IC[4, 1] <- tick.stats %>% filter(lifeStage == "Adult") %>% pull(mu)
			IC[4, 2] <- tick.stats %>% filter(lifeStage == "Adult") %>% pull(tau)

			fx.sequence <- seq.Date(fx.start.date, by = 1, length.out = horizon)
			n.days <- length(fx.sequence)
		}

	if (use.daymet) {
		# Filter dates that don't have corresponding drags
		daymet.sub <- df.daymet %>%
			filter(Date %in% fx.sequence)
			
		# Create day x site matrices for each variable
		data$maxtemp <- daymet.sub %>% 
			select(Date, siteID, maxTempScale) %>%
			pivot_wider(names_from= siteID, values_from = maxTempScale,
		              values_fill=NA) %>%
			select(-Date) %>%
			as.matrix()
			
		data$maxrh <- daymet.sub %>%
		  select(Date, siteID, maxRHScale) %>%
			pivot_wider(names_from=siteID, values_from=maxRHScale,
			           values_fill=NA) %>%
			select(-Date) %>%
			as.matrix()
			
		data$minrh <- daymet.sub %>% 
      select(Date, siteID, minRHScale) %>%
	    pivot_wider(names_from=siteID, values_from=minRHScale,
	                values_fill=NA) %>%
		  select(-Date) %>%
		  as.matrix()
			
		data$precip <- daymet.sub %>% 
		  select(Date, siteID, precipScale) %>%
			pivot_wider(names_from=siteID, values_from=precipScale,
			            values_fill=NA) %>%
			select(-Date) %>%
			as.matrix()
	}
    
	# Get observational data
	obs <- neon.job %>%
	  filter(time == fx.start.date)
	
	# Get number of plots per site
	plots <- neon.job %>%
	  select(siteID, plotID) %>%
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
			    y[1, 1, p, site] <- obs.plot %>% pull(Larva)
			    y[3, 1, p, site] <- obs.plot %>% pull(Nymph)
			    y[4, 1, p, site] <- obs.plot %>% pull(Adult)
			    area[1, p, site] <- obs.plot %>% pull(totalSampledArea)
			  }
		  }
	  }
	}

	# finalize data
	data$y <- y
	data$area <- area
	data$IC <- IC
	data$pr.phi.l <- phi.l
	data$pr.phi.n <- phi.n
	data$pr.phi.a <- phi.a
	data$pr.theta.l2n <- theta.l2n
	data$pr.theta.n2a <- theta.n2a
	data$repro.mu <- repro.mu
	data$pr.beta <- pr.beta
	data$pr.sig <- pr.sig %>% select(-parameter) %>% as.matrix()
	
	# Cumulative degree days
	data$cgdd <- cgdd %>%
	  filter(Date %in% ymd(fx.sequence)) %>%
	  pivot_wider(names_from=siteID, values_from=cumGDD) %>%
	  select(-Date)
	
	data$max.cgdd <- cgdd %>%
	  filter(Date %in% ymd(fx.sequence)) %>%
	  group_by(siteID) %>%
	  summarise(max.cgdd = max(cumGDD)*1.2) %>%
	  pull(max.cgdd)
	
	data$xind <- array(1, dim=c(4, horizon, length(sites)))

	if (miceAndWeather){
	  data$mice <- mna.scaled %>%
	    filter(Date %in% fx.sequence) %>%
	    pivot_wider(names_from=siteID, values_from=mna_scaled)

	  if (nrow(data$mice) < length(fx.sequence)) {
	    horizon <- min(length(data$cgdd), hrow(data$mice))
	    data$y <- y[, 1:horizon, ,]
	  }
	}

	if (year(fx.start.date) == max(year(neon.job$time))) {
	  if (model.job == "Weather_hierarchical") {
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

	# Initialize area
	area.init <- area
	nai <- which(is.na(area))	
	area.init[nai] <- 160
	area.init[-nai] <- NA
		
	# Initial values to send to nimble
	inits <- function() {
	  list(
	    area = area.init,
			phi.l.mu = rep(rnorm(1, phi.l[1], 1 / sqrt(phi.l[2])), length(sites)),
			phi.n.mu = rep(rnorm(1, phi.n[1], 1 / sqrt(phi.n[2])), length(sites)),
			phi.a.mu = rep(rnorm(1, phi.a[1], 1 / sqrt(phi.a[2])), length(sites)),
			theta.ln = rep(rnorm(1, theta.l2n[1], 1 / sqrt(theta.l2n[2])), 
			               length(sites)),
			theta.na = rep(rnorm(1, theta.n2a[1], 1 / sqrt(theta.n2a[2])), 
			               length(sites)),
			beta = rnorm(n.beta, pr.beta[, 1], 1 / sqrt(pr.beta[, 2])),
			sig = rinvgamma(4, pr.sig$alpha, pr.sig$beta),
			x = array(rpois(4 * horizon * length(sites), 2) / 160 * 450, 
			          dim=c(4, horizon, length(sites))),
			Ex = array(rpois(4 * horizon * length(sites), 2) / 160 * 450,
			           dim=c(4, horizon, length(sites))),
			y = array(rpois(4 * horizon * n.plots * length(sites), 5), 
			          dim = dim(data$y)),
			tau.temp = rexp(length(sites)),
			tau.maxrh = rexp(length(sites)),
			tau.minrh = rexp(length(sites)),
			tau.precip = rexp(length(sites)),
			tau.cgdd = rexp(length(sites)),
			x1 = jitter(data$maxtemp),
			x2 = jitter(data$maxrh),
			x3 = jitter(data$minrh),
			x4 = jitter(data$precip),
			gdd = jitter(as.matrix(data$cgdd)),
			OMEGA = matrix(0, nrow = 4, ncol = 4),
			A = array(0, dim=c(4, 4, horizon, length(sites)))
		)
	}

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
		miceAndWeather = miceAndWeather,
		use.daymet = use.daymet
		) 
	
	stopCluster(cl)
  
	# Merge outputs of chains
	dat.hindcast <- list()
	for(i in 1:length(out.nchains[[1]])){
	  
	}

	# RESUME HERE -------------------
	# Test MCMC convergence with Gelman-Rubin statistic
	message("Checking convergence...")
	nodes <- names(out.nchains[[1]])
	gelman.keep <- numeric(length(nodes))
	
	for (ff in seq_along(nodes)) {
	  mcmc.check <- list()
		col <- nodes[ff]
				
		for (c in seq_along(out.nchains)) {
		  mcmc.check[[c]] <- coda::mcmc(out.nchains[[c]][, col])
		  }
		
		gelman.keep[ff] <- try(coda::gelman.diag(mcmc.check, 
		                                         transform = TRUE)$psrf[1])

		if (any(gelman.keep > 1.2)) {
		  # message("WARNING: Convergence not reached!")
		  bad.nodes <- which(gelman.keep > 1.2)
		  bad.params <- tibble(node = nodes[bad.nodes],
		    psrf = as.numeric(gelman.keep[bad.nodes])) %>%
		    arrange(psrf)
				# print(tail(bad.params))
		  
		  } else {
		    # message("Convergence = TRUE")
		  }
	}
  
	# Thin the chains
	if (nrow(dat.hindcast) > 5000) {
	  draws <- round(seq.int(1, nrow(dat.hindcast), length.out = 5000))
  } else {
		  draws <- seq_len(nrow(dat.hindcast))
	}

	dat.draws <- dat.hindcast[draws, ]

	# Output processing and save
	fileDest <- file.path(dir.save, fx.start.date)
	
	message("Running analysis...")
	
	transfer_analysis(
	  fx.df = dat.draws,
		observations = neon.job,
		fx.dates = fx.sequence,
		model = model.job,
		spp = species.job,
		out.dir = fileDest)
}
