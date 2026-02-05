library(nimble)
source("./DataProcessing/functions.R")

model.code <- nimbleCode({
  for(site in 1:nsite){
	  ### priors
	  phi.l.mu[site] ~ dnorm(pr.phi.l[1], tau = pr.phi.l[2])
	  phi.n.mu[site] ~ dnorm(pr.phi.n[1], tau = pr.phi.n[2])
	  phi.a.mu[site] ~ dnorm(pr.phi.a[1], tau = pr.phi.a[2])
	  theta.ln[site] ~ dnorm(pr.theta.l2n[1], tau = pr.theta.l2n[2])
	  theta.na[site] ~ dnorm(pr.theta.n2a[1], tau = pr.theta.n2a[2])
	  # Params can also be drawn from prior distributions
	  # But this is fine for now: an informative prior
	  # Based on previous data/model iterations

    for (j in 1:n.beta) {
      # Betas are the linear covariates for mice/weather linear models
      # This will be hierarchical after hierarchical intercepts work
		  beta[j] ~ dnorm(pr.beta[j, 1], tau = pr.beta[j, 2])
	  }

		tau.temp[site] ~ dexp(1)
		tau.maxrh[site] ~ dexp(1)
		tau.minrh[site] ~ dexp(1)
		tau.precip[site] ~ dexp(1)
		tau.cgdd[site] ~ dexp(1)
  
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

	  ### first latent process
		for (i in 1:4) {
			x[i, 1, site] ~ T(dnorm(IC[i, 1], tau = IC[i, 2]), 0, Inf)
		}

	  ### define parameters
	  for (t in 1:horizon) {
		  # loop over every day in time series
	    
	    # Mice inputs for life stage transitions
      if (miceAndWeather) {
			  logit(l2n[t, site]) <- theta.ln[site] + beta[13] * mice[t, site]
			  logit(n2a[t, site]) <- theta.na[site] + beta[14] * mice[t, site]
		  } else {
			  logit(l2n[t, site]) <- theta.ln[site]
			  logit(n2a[t, site]) <- theta.na[site]
		  }
	    
		  cgdd[t, site] ~ dnorm(gdd[t, site], tau = tau.cgdd[site])
		  gdd[t, site] ~ dunif(0, max.cgdd[site])

		  theta.n2a[t, site] <- if_else_nimble(
			  (gdd[t, site] <= 1000) | (gdd[t, site] >= 2500),
			  n2a[t, site],
			  0
		  )
		  
		  # lambda is part of transition matrix... I think reproduction?
		  lambda[t, site] <- if_else_nimble(
			  (gdd[t, site] >= 1400) & (gdd[t, site] <= 2500),
			  repro.mu,
			  0
		  )
		  l2n.quest[t, site] <- if_else_nimble((gdd[t, site] >= 400) & 
		                                   (gdd[t, site] <= 2500), 1, 0)

		  if (use.daymet) {
		    # Weather inputs for survival models
				maxtemp[t, site] ~ dnorm(x1[t, site], tau = tau.temp[site])
				maxrh[t, site] ~ dnorm(x2[t, site], tau = tau.maxrh[site])
				minrh[t, site] ~ dnorm(x3[t, site], tau = tau.minrh[site])
				precip[t, site] ~ dnorm(x4[t, site], tau = tau.precip[site])

				x1[t, site] ~ dnorm(0, 1)
				x2[t, site] ~ dnorm(0, 1)
				x3[t, site] ~ dnorm(0, 1)
				x4[t, site] ~ dnorm(0, 1)

			  logit(phi.l[t, site]) <- phi.l.mu[site] +
				  beta[1] * x1[t, site] +
				  beta[2] * x2[t, site] +
				  beta[3] * x3[t, site] +
				  beta[4] * x4[t, site]

			  logit(phi.n[t, site]) <- phi.n.mu[site] +
				  beta[5] * x1[t, site] +
				  beta[6] * x2[t, site] +
				  beta[7] * x3[t, site] +
			  	beta[8] * x4[t, site]

			  logit(phi.a[t, site]) <- phi.a.mu[site] +
				  beta[9] * x1[t, site] +
				  beta[10] * x2[t, site] +
				  beta[11] * x3[t, site] +
				  beta[12] * x4[t, site]
		  } else {
			  logit(phi.l[t, site]) <- phi.l.mu[site]
			  logit(phi.n[t, site]) <- phi.n.mu[site]
			  logit(phi.a[t, site]) <- phi.a.mu[site]
		  }

		  # Transition matrix
		  A[1, 1, t, site] <- phi.l[t, site] * (1 - l2n[t, site])
		  A[1, 4, t, site] <- lambda[t, site]
		  A[2, 1, t, site] <- phi.l[t, site] * l2n[t, site]
		  A[2, 2, t, site] <- 1 - l2n.quest[t, site]
		  A[3, 2, t, site] <- l2n.quest[t, site]
		  A[3, 3, t, site] <- phi.n[t, site] * (1 - theta.n2a[t, site])
		  A[4, 3, t, site] <- phi.n[t, site] * theta.n2a[t, site]
		  A[4, 4, t, site] <- phi.a[t, site]

		  ### Data Model ###
		  # This does not associate plot samples with environmental covs
		  # But it does treat individual plots as replicates of the site
		  for (p in 1:n.plots[site]) {
			  dx[1, t, p, site] <- x[1, t, site] / 450 * area[t, p, site]
			  dx[2, t, p, site] <- x[3, t, site] / 450 * area[t, p, site]
			  dx[3, t, p, site] <- x[4, t, site] / 450 * area[t, p, site]
			  y[1, t, p, site] ~ dpois(dx[1, t, p, site]) 
			  y[3, t, p, site] ~ dpois(dx[2, t, p, site])
			  y[4, t, p, site] ~ dpois(dx[3, t, p, site])
			  
			  # dx is a transition variable so it only has 3 rows instead of 4
		  }
	  }

	  for (t in 2:horizon) {
		  # expected number questing
		  Ex[1:4, t, site] <- A[1:4, 1:4, t-1, site] %*% x[1:4, t-1, site]

		  x[1:ns, t, site] ~
			  dmnorm(mean = Ex[1:ns, t, site], 
			         cholesky = Ochol[1:ns, 1:ns], prec_param = 0)

		  for (c in 1:ns) {
			  xind[c, t, site] ~ dconstraint(x[c, t, site] >= 0)
		  }
		}
  }
}
)
