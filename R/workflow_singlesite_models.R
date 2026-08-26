## Workflow for plot-level single-site models
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

library(mclm)
library(fitdistrplus)
library(tidyverse)
library(lubridate)
library(nimble)
library(parallel)

options(dplyr.summarise.inform = FALSE)

dir.top <- getwd()
dir.out <- file.path(dir.top, "out")

# Define models to run
models <- c("PlotLevel")

iscap.sites <- str_remove(read_txt("./Data/ix_sites_single.txt"), "\\*")
ambly.sites <- str_remove(read_txt("./Data/am_sites_single.txt"), "\\*")

# Create all possible combos
iscap.jobs <- data.frame(site = iscap.sites, species = "Ixodes_scapularis")
ambly.jobs <- data.frame(site = ambly.sites, species = "Amblyomma_americanum")

jobs <- bind_rows(iscap.jobs, ambly.jobs) %>%
  mutate(model = models)

job.num <- as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(job.num)) {
	job.num <- 1
}

site.job <- jobs$site[job.num]
species.job <- jobs$species[job.num]
model.job <- jobs$model[job.num]

n.slots <- Sys.getenv("NSLOTS") %>% as.numeric() #Cluster var # of cores
if(is.na(n.slots)){
  n.slots <- 2
}

n.iter <- 50000
Nmc <- 2000
horizon <- 365

# =========================================== #
#       tick data intake ----------------
# =========================================== #
source("./DataProcessing/functions.R")

# Get tick data based on site
neon.data <- neon_tick_data(species.job) %>% suppressMessages()
# function now retrieves cary sites as well as NEON

# Filter tick data based on job requirements
neon.job <- neon.data %>%
	filter(siteID == site.job, #grepl("Forest", nlcd), 
	       time >= "2016-01-01" & time < "2022-01-01") %>%
	arrange(time)

# Extract sampling dates and number of samples
drag.dates <- as.Date(neon.job$time, format="%Y-%m-%d") %>% unique()
start.date <- first(drag.dates)
n.drags <- length(drag.dates)

# =========================================== #
#       get initial conditions ----------
# =========================================== #

df.latent <- read_csv(file.path("./Data", "dormantNymphTimeSeries.csv"))
month.get <- if_else(month(start.date) < 5, 4, month(start.date))
data.latent <- df.latent %>%
	mutate(model = gsub("DormantNymph", "", model)) %>%
	filter(
		model == "WithWeatherAndMiceGlobal",
		type == "latent",
		statistic == "conf_50",
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

source("./DataProcessing/capture_matrix.R")

if(site.job %in% c("HNRY", "TEA", "GREN")){
  smam <- read_csv("./Data/cary_mouse_formatted.csv",
                   show_col_types=F) %>%
    filter(siteID == site.job) %>%
    rename(MNA = n_trapped)
  
  mice.obs <- ymd(smam$collectDate)
  mna <- smam$MNA
  names(mna) <- smam$collectDate
  
} else{
  smam <- read_csv("./Data/allSmallMammals.csv",
                   show_col_types=F)
               
  ch.ls <- capture_matrix(site.job, smam)
  ch <- ch.ls$ch
  alive <- ch %in% 1:3
  ch[alive] <- 1
  ch[!alive] <- 0
  ncaps <- rowSums(ch)
  ch <- ch[ncaps > 0, ]
  source("./DataProcessing/known_states.R")
  ks <- known_states(ch)
  mna <- colSums(ks)
  mice.obs <- ymd(colnames(ch)) # unique sampling days: mice
}

# every day in mouse sequence
mice.seq <- seq.Date(mice.obs[1], mice.obs[length(mice.obs)], by = 1)

mna.all.days <- rep(NA, length(mice.seq))
mna.count <- 1
for (i in seq_along(mice.seq)) {
	if (mice.seq[i] %in% mice.obs) {
		mna.all.days[i] <- mna[mna.count]
		mna.count <- mna.count + 1
	} else {
		mna.all.days[i] <- mna[mna.count]
	}
}

# historical mna
mna.hist <- mna_jags("Green Control", return.mean = TRUE)

# center and scale
mna.scaled <- tibble(
	mna.scaled = (mna.all.days - mna.hist$mean) / mna.hist$sd,
	Date = mice.seq
)

# =========================================== #
#       daymet intake and correction -------------
# =========================================== #
source("./DataProcessing/daymet_downscale_singlesite.R")

cgdd <- daymet_cumGDD(site.job) %>% suppressMessages()
maxTemp <- daymet_temp(site.job, minimum = FALSE) %>%
    select(Date, maxTempCorrect) %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    suppressMessages()
rh <- daymet_rh(site.job) %>%
    select(Date, maxRHCorrect, minRHCorrect) %>%
    suppressMessages()
precip <- daymet_precip(site.job) %>%
    select(Date, precipitation) %>%
    suppressMessages()
  
hist.means <- scale_met_forecast()
  
join1 <- left_join(maxTemp, rh, by = "Date")
join2 <- left_join(join1, precip, by = "Date")
  
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
  select(Date, contains("Scale")) %>%
  filter(Date >= "2016-01-01" & Date < "2022-01-01")

# =========================================== #
#       get informative priors -------------------
# =========================================== #
df.params <- read_csv(file.path("./Data/dormantNymphParams.csv"),
                      show_col_types = F)

params.stats <- df.params %>%
	filter(model == "WithWeatherAndMiceGlobal") %>%
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

phi.l <- get_prior("phi.l.mu")
phi.n <- get_prior("phi.n.mu")
phi.a <- get_prior("phi.a.mu")
theta.l2n <- get_prior("theta.ln")
theta.n2a <- get_prior("theta.na")
repro <- get_prior("repro.mu")
repro.mu <- repro[1]

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
	filter(model == "WithWeatherAndMiceGlobal", grepl("sig", parameter)) %>%
	select(parameter, value) %>%
	group_by(parameter) %>%
	summarise(alpha = inv_gamma_mm(value)[1], beta = inv_gamma_mm(value)[2])

# iterate ==================================================================

t = 2

for (t in seq_len(n.drags)) {
	fx.start.date <- drag.dates[t]
	message("---------------------------------------------------")
	mm <- paste(fx.start.date, " (", round(t / n.drags * 100, 2), "%)")
	message(mm)

	dir.base <- file.path(
			dir.out,
			site.job,
			model.job,
			gsub(" ", "", species.job)
	)
	dir.save <- file.path(dir.base)

	# initialize nimble lists
	constants <- data <- list()

	if (t == 1) {
		fx.sequence <- seq.Date(fx.start.date, by = 1, length.out = horizon)
		n.days <- horizon
		y <- matrix(NA, 4, horizon)
		
		# uninformative plot-level priors (zero inflation/phenology)
		pr.gam0 <- cbind(rep(0,3), rep(1,3))
		pr.gam1 <- cbind(rep(1,3), rep(1,3))
		pr.gam2 <- cbind(rep(0,3), rep(1,3))
		
	} else {
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

		phi.l <- get_prior("phi.l.mu")
		phi.n <- get_prior("phi.n.mu")
		phi.a <- get_prior("phi.a.mu")
		theta.l2n <- get_prior("theta.ln")
		theta.n2a <- get_prior("theta.na")

		pr.beta <- matrix(NA, n.beta, 2)
		for (i in seq_len(n.beta)) {
				pr.beta[i, ] <- get_prior(paste0("beta[", i, "]"))
		}
		
		# plot-level priors
		pr.gam0 <- matrix(NA, 3, 2)
		for(i in 1:3){
		  pr.gam0[i,] <- get_prior(paste0("gam0[", i, "]"))
		}
		
		# gam1 is weird because it must be negative
		gam1.vals <- last.params %>%
		  filter(str_detect(node, "gam1"))
		
		pr.gam1 <- matrix(NA, 3, 2)
		for(i in 1:3){
		  pr.gam1[i,] <- filter(gam1.vals, node == paste0("gam1[", i, "]")) %>%
		    mutate(value = ifelse(value > 0, value, -value)) %>%
		    summarise(alpha = fitdist(value, distr = 'gamma')$estimate[1],
		              beta = fitdist(value, distr = 'gamma')$estimate[2]) %>%
		    as.numeric() %>%
		    suppressMessages()
		}
		
		pr.gam2 <- matrix(NA, 3, 2)
		for(i in 1:3){
		  pr.gam2[i,] <- get_prior(paste0("gam2[", i, "]"))
		}

		# get invgamma parameters
		pr.sig <- last.params %>%
				rename(parameter = node) %>%
				filter(grepl("sig", parameter)) %>%
				select(parameter, value) %>%
				group_by(parameter) %>%
				summarise(alpha = inv_gamma_mm(value)[1],
				          beta = inv_gamma_mm(value)[2])

		last.fx <- read_csv(file.path(readDest, "stateSamples.csv")) %>%
		  suppressMessages()

		tick.stats <- last.fx %>%
			filter(time == fx.start.date) %>%
				group_by(lifeStage, time) %>%
			  summarise(mu = mean(value), tau = 1 / var(value))

		IC <- matrix(NA, 4, 2)
			
		if(nrow(tick.stats)!=0){
			  IC[1, 1] <- tick.stats %>% filter(lifeStage == "Larva") %>% pull(mu)
			  IC[1, 2] <- tick.stats %>% filter(lifeStage == "Larva") %>% pull(tau)
			  IC[2, 1] <- tick.stats %>% filter(lifeStage == "Dormant") %>% pull(mu)
			  IC[2, 2] <- tick.stats %>% filter(lifeStage == "Dormant") %>% pull(tau)
			  IC[3, 1] <- tick.stats %>% filter(lifeStage == "Nymph") %>% pull(mu)
			  IC[3, 2] <- tick.stats %>% filter(lifeStage == "Nymph") %>% pull(tau)
			  IC[4, 1] <- tick.stats %>% filter(lifeStage == "Adult") %>% pull(mu)
			  IC[4, 2] <- tick.stats %>% filter(lifeStage == "Adult") %>% pull(tau)
			}

		fx.sequence <- seq.Date(fx.start.date, by = 1, length.out = horizon)
    n.days <- length(fx.sequence)
	}
		
	if(horizon == 0){
		break
	}

	daymet.sub <- df.daymet %>%
			filter(Date %in% fx.sequence)
			
	data$maxtemp <- daymet.sub %>% pull(maxTempScale) %>% as.vector()
	data$maxrh <- daymet.sub %>% pull(maxRHScale) %>% as.vector()
	data$minrh <- daymet.sub %>% pull(minRHScale) %>% as.vector()
	data$precip <- daymet.sub %>% pull(precipScale) %>% as.vector()

	obs <- neon.job %>%
		filter(time == fx.start.date)

	plots <- unique(obs$plotID)
	n.plots <- length(plots)

	y <- array(NA, dim = c(4, horizon, n.plots))
	area <- matrix(NA, horizon, n.plots)
	for (p in 1:n.plots) {
	  obs.plot <- obs %>% filter(plotID == plots[p])
	  if(nrow(obs.plot) > 1){
	    obs.plot <- obs.plot[1,]
	  }
		y[1, 1, p] <- obs.plot %>% pull(Larva)
		y[3, 1, p] <- obs.plot %>% pull(Nymph)
		y[4, 1, p] <- obs.plot %>% pull(Adult)
		area[1, p] <- obs.plot %>% pull(totalSampledArea)
	}

	# Finalize data
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
	data$pr.gam0 <- pr.gam0
	data$pr.gam1 <- pr.gam1
	data$pr.gam2 <- pr.gam2
	data$pr.sig <- pr.sig %>% select(-parameter) %>% as.matrix()
	
	data$gdd <- cgdd %>%
	  ungroup() %>%
		filter(Date %in% ymd(fx.sequence),
		       plotID %in% plots) %>%
	  select(-year) %>%
	  pivot_wider(names_from = plotID, values_from = cumGDD,
	              values_fn = mean) %>%
	  select(-Date)
	
	data$max.cgdd <- max(data$gdd) * 1.2
	data$xind <- matrix(1, 4, horizon)

	data$mice <- mna.scaled %>%
	  filter(Date %in% fx.sequence) %>%
		pull(mna.scaled)

	if (length(data$mice) < length(fx.sequence)) {
	  horizon <- min(nrow(data$gdd), length(data$mice))
	  data$y <- y[, 1:horizon, ]
		
	  if(is.na(dim(data$y)[3]==T)){
			 dim(data$y)[3] <- 1
		}
	}

	if (year(fx.start.date) == max(year(neon.job$time))){
		horizon <- nrow(daymet.sub)
		data$y <- as.array(y[, 1:horizon, ])
				
		if(is.na(dim(data$y)[3]==T)){
		  dim(data$y)[3] <- 1
		  }
    
		} else {
		  horizon <- min(nrow(data$cgdd), length(data$mice))
			data$y <- y[, 1:horizon, ]
				
			if(is.na(dim(data$y)[3]==T)){
				dim(data$y)[3] <- 1
			}
		}
		
	if(horizon == 0){
		break
	}

	# finalize constants
	constants$n.beta <- n.beta
	constants$n.plots <- n.plots
	constants$horizon <- horizon
	constants$ns <- 4

	area.init <- area
	nai <- which(is.na(area))
	area.init[nai] <- 160
	area.init[-nai] <- NA

	# Define inits
	inits <- function() {
	  list(
	    area = area.init,
			phi.l.mu = rnorm(1, phi.l[1], 1 / sqrt(phi.l[2])),
			phi.n.mu = rnorm(1, phi.n[1], 1 / sqrt(phi.n[2])),
			phi.a.mu = rnorm(1, phi.a[1], 1 / sqrt(phi.a[2])),
			theta.ln = rnorm(1, theta.l2n[1], 1 / sqrt(theta.l2n[2])),
			theta.na = rnorm(1, theta.n2a[1], 1 / sqrt(theta.n2a[2])),
			beta = rnorm(n.beta, pr.beta[, 1], 1 / sqrt(pr.beta[, 2])),
			sig = rinvgamma(4, pr.sig$alpha, pr.sig$beta),
			x = matrix(rpois(4 * horizon, 2) / 160 * 450, 4, horizon),
			Ex = matrix(rpois(4 * horizon, 2) / 160 * 450, 4, horizon),
			y = array(rpois(4 * horizon * n.plots, 5), dim = dim(data$y)),
			tau.temp = rexp(1),
			tau.maxrh = rexp(1),
			tau.minrh = rexp(1),
			tau.precip = rexp(1),
			x1 = jitter(data$maxtemp),
			x2 = jitter(data$maxrh),
			x3 = jitter(data$minrh),
			x4 = jitter(data$precip))
	}
	
	# Parameters to save (uses less memory)
	params.to.save <- c("beta", "phi.a.mu", "phi.l.mu", "phi.n.mu", "sig",
	            # "tau.maxrh", "tau.minrh", "tau.precip", "tau.temp", 
	            "theta.ln", "theta.na", 
	            "gam0", "gam1", "gam2", "pz", 
	            "dx", "dlamb",
	            "x" #, "x1", "x2", "x3", "x4"
	)  

	source("./R/nimble_forecast_singlesite.R")
	source("./R/run_transfer_nimble_singlesite.R")
	cl <- makeCluster(n.slots) 
		
	out.nchains <- run_transfer_nimble(
		cl = cl,
		model = model.code,
		data = data,
		constants = constants,
		inits = inits,
		n.iter = n.iter,
		parms = params.to.save)
		
	stopCluster(cl)

	dat.hindcast <- do.call(rbind, out.nchains)

	message("Checking convergence...")
	nodes <- colnames(out.nchains[[1]])
	nodes <- nodes[!str_detect(nodes, c("dlamb|dx|pz|x"))]
			
	gelman.keep <- numeric(length(nodes))
	for (ff in seq_along(nodes)) {
	  mcmc.check <- list()
	  col <- nodes[ff]
	  
	  for (c in seq_along(out.nchains)) {
	    mcmc.check[[c]] <- coda::mcmc(out.nchains[[c]][, col])
	    }
	  
	  gelman.keep[ff] <- try(coda::gelman.diag(
	    mcmc.check,
	    transform = TRUE)$psrf[1])

	  if (any(gelman.keep > 1.2)) {
	    # message("WARNING: Convergence not reached!")
	    bad.nodes <- which(gelman.keep > 1.2)
	    bad.params <- tibble(
				node = nodes[bad.nodes],
				psrf = as.numeric(gelman.keep[bad.nodes])
			) %>%
					arrange(psrf)
	    
	    print(tail(bad.params))
	  } 
	  }

	if (nrow(dat.hindcast) > 5000) {
	  draws <- round(seq.int(1, nrow(dat.hindcast), length.out = 5000))
	} else {
		draws <- seq_len(nrow(dat.hindcast))
	}

	dat.draws <- dat.hindcast[draws, ]

	fileDest <- file.path(dir.save, fx.start.date)
	message("Running analysis...")
		
	transfer_analysis(
	  fx.df = dat.draws,
	  observations = neon.job,
	  fx.dates = fx.sequence,
	  model = model.job,
		spp = species.job,
		plots = plots, 
		out.dir = fileDest
	)
	
	rm(dat.draws)
	rm(dat.hindcast)
	rm(out.nchains)
}
