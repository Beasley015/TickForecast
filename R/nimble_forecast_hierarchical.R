library(nimble)
source("./DataProcessing/functions_hierarchical.R")

model.code <- nimbleCode({
  # Priors for beta
  for (j in 1:n.beta) {
    # Betas are the linear covariates for mice/weather linear models
    # This will vary by site after hierarchical intercepts work
		beta[j] ~ dnorm(pr.beta[j, 1], tau = pr.beta[j, 2])
    }

  # Set up Cholensky decomp- does not vary by site
  for (i in 1:ns) { # ns = number of life stages
			sig[i] ~ dinvgamma(pr.sig[i, 1], pr.sig[i, 2]) 
    }

  ### precision priors with process error
	OMEGA[1, 1] <- sig[1]
	OMEGA[2, 2] <- sig[2]
	OMEGA[3, 3] <- sig[3]
	OMEGA[4, 4] <- sig[4]

	# Cholesky decomposition
	Ochol[1:4, 1:4] <- chol(OMEGA[1:4, 1:4])
	  
	# Omega and the Cholensky decomp are eventually used
	# To estimate questing ticks
	
	### priors
	phi.l.mu[1:nsite] ~ dnorm_vec(pr.phi.l[1], tau = pr.phi.l[2])
	phi.n.mu[1:nsite] ~ dnorm_vec(pr.phi.n[1], tau = pr.phi.n[2])
	phi.a.mu[1:nsite] ~ dnorm_vec(pr.phi.a[1], tau = pr.phi.a[2])
	theta.ln[1:nsite] ~ dnorm_vec(pr.theta.l2n[1], tau = pr.theta.l2n[2])
	theta.na[1:nsite] ~ dnorm_vec(pr.theta.n2a[1], tau = pr.theta.n2a[2])
	# Params can also be drawn from prior distributions
	# But this is fine for now: an informative prior
	# Based on previous data/model iterations

	for(site in 1:nsite){
	  # Weather precision priors
	  tau.temp[site] ~ dexp(1)
	  tau.maxrh[site] ~ dexp(1)
	  tau.minrh[site] ~ dexp(1)
	  tau.precip[site] ~ dexp(1)
	  tau.cgdd[site] ~ dexp(1)
	  
	  ### first latent process
	  for (i in 1:4) {
	    x[i, 1, site] ~ dgamma(shape=IC[i, 1, site], rate = IC[i, 2, site])
	  }
	}
	
	### define parameters
	for (t in 1:horizon) {
	  # loop over every day in time series
	  for(site in 1:nsite){
	    cgdd[t, site] ~ dnorm(gdd[t, site], tau = tau.cgdd[site])
	    gdd[t, site] ~ dunif(min = 0, max = max.cgdd[2,site])

	    theta.n2a[t, site] <- if_else_nimble(
	      (gdd[t, site] <= 1000) | (gdd[t, site] >= 2500),
	      n2a[t, site],
	      0)
	    
		  lambda[t, site] <- if_else_nimble(
		    (gdd[t, site] >= 1400) & (gdd[t, site] <= 2500),
		    repro.mu,
		    0)
		  
		  l2n.quest[t, site] <- if_else_nimble((gdd[t, site] >= 400) &
		                                         (gdd[t, site] <= 2500), 1, 0)
	  }
	  
	  # Mice inputs for life stage transitions
	  if (miceAndWeather) {
	    logit(l2n[t, 1:nsite]) <- theta.ln[1:nsite] + beta[13] * mice[t, 1:nsite]
	    logit(n2a[t, 1:nsite]) <- theta.na[1:nsite] + beta[14] * mice[t, 1:nsite]
	  } else {
	    logit(l2n[t, 1:nsite]) <- theta.ln[1:nsite]
	    logit(n2a[t, 1:nsite]) <- theta.na[1:nsite]
	  }
	  
	  # Weather inputs for survival
	  if(use.daymet){
	    for(site in 1:nsite){
	    # Weather inputs for survival models
	      maxtemp[t, site] ~ dnorm(x1[t, site], tau = tau.temp[site])
	      maxrh[t, site] ~ dnorm(x2[t, site], tau = tau.maxrh[site])
	      minrh[t, site] ~ dnorm(x3[t, site], tau = tau.minrh[site])
	      precip[t, site] ~ dnorm(x4[t, site], tau = tau.precip[site])
	    }
	    
	    x1[t, 1:nsite] ~ dnorm_vec(0, 1)
	    x2[t, 1:nsite] ~ dnorm_vec(0, 1)
	    x3[t, 1:nsite] ~ dnorm_vec(0, 1)
	    x4[t, 1:nsite] ~ dnorm_vec(0, 1)
	    
	    logit(phi.l[t, 1:nsite]) <- phi.l.mu[1:nsite] +
	      beta[1] * x1[t, 1:nsite] +
	    	beta[2] * x2[t, 1:nsite] +
	    	beta[3] * x3[t, 1:nsite] +
	    	beta[4] * x4[t, 1:nsite]

			logit(phi.n[t, 1:nsite]) <- phi.n.mu[1:nsite] +
			  beta[5] * x1[t, 1:nsite] +
			  beta[6] * x2[t, 1:nsite] +
				beta[7] * x3[t, 1:nsite] +
			  beta[8] * x4[t, 1:nsite]

			logit(phi.a[t, 1:nsite]) <- phi.a.mu[1:nsite] +
				beta[9] * x1[t, 1:nsite] +
				beta[10] * x2[t, 1:nsite] +
				beta[11] * x3[t, 1:nsite] +
				beta[12] * x4[t, 1:nsite]
			} else {
			  logit(phi.l[t, 1:nsite]) <- phi.l.mu[1:nsite]
			  logit(phi.n[t, 1:nsite]) <- phi.n.mu[1:nsite]
			  logit(phi.a[t, 1:nsite]) <- phi.a.mu[1:nsite]
			  }

		  # Transition matrix
		  A[1, 1, t, 1:nsite] <- phi.l[t, 1:nsite] * (1 - l2n[t, 1:nsite])
		  A[1, 4, t, 1:nsite] <- lambda[t, 1:nsite]
		  A[2, 1, t, 1:nsite] <- phi.l[t, 1:nsite] * l2n[t, 1:nsite]
		  A[2, 2, t, 1:nsite] <- 1 - l2n.quest[t, 1:nsite]
		  A[3, 2, t, 1:nsite] <- l2n.quest[t, 1:nsite]
		  A[3, 3, t, 1:nsite] <- phi.n[t, 1:nsite] * (1 - theta.n2a[t, 1:nsite])
		  A[4, 3, t, 1:nsite] <- phi.n[t, 1:nsite] * theta.n2a[t, 1:nsite]
		  A[4, 4, t, 1:nsite] <- phi.a[t, 1:nsite]

		  for(site in 1:nsite){
		  ### Data Model ###
		  # This does not associate plot samples with environmental covs
		  # But it does treat individual plots as replicates of the site
			  dx[1, t, 1:n.plots[site], site] <- x[1, t, site] / 450 * area[t, 1:n.plots[site], site]
			  dx[2, t, 1:n.plots[site], site] <- x[3, t, site] / 450 * area[t, 1:n.plots[site], site]
			  dx[3, t, 1:n.plots[site], site] <- x[4, t, site] / 450 * area[t, 1:n.plots[site], site]
			  y[1, t, 1:n.plots[site], site] ~ dpois(dx[1, t, 1:n.plots[site], site])
			  y[3, t, 1:n.plots[site], site] ~ dpois(dx[2, t, 1:n.plots[site], site])
			  y[4, t, 1:n.plots[site], site] ~ dpois(dx[3, t, 1:n.plots[site], site])

			  # dx is a transition variable so it only has 3 rows instead of 4
		  }
	  }

	  for (t in 2:horizon) {
		  # expected number questing
		  Ex[1:4, t, 1:nsite] <- A[1:4, 1:4, t-1, 1:nsite] %*% x[1:4, t-1, 1:nsite]

		  x[1:ns, t, 1:nsite] ~
			  dmnorm(mean = Ex[1:ns, t, 1:nsite],
			         cholesky = Ochol[1:ns, 1:ns], prec_param = 0)
		}
  }
)
