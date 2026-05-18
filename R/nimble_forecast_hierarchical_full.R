library(nimble)
source("./DataProcessing/functions_hierarchical.R")

model.code <- nimbleCode({
  # Hyperpriors for intercepts 
  phi.l.mean ~ dnorm(pr.phi.l[1],sd=1)
  phi.l.prec ~ dgamma(prec.l[1],prec.l[2]) 
  
  phi.n.mean ~ dnorm(pr.phi.n[1],sd=1)
  phi.n.prec ~ dgamma(prec.n[1],prec.n[2])
  
  phi.a.mean ~ dnorm(pr.phi.a[1],sd=1)
  phi.a.prec ~ dgamma(prec.a[1],prec.a[2])
  
  theta.ln.mean ~ dnorm(pr.theta.l2n[1],sd=1)
  theta.ln.prec ~ dgamma(prec.ln[1],prec.ln[2])
  
  theta.na.mean ~ dnorm(pr.theta.n2a[1],sd=1)
  theta.na.prec ~ dgamma(prec.na[1],prec.na[2])
  
  # Hyperpriors for betas
  for(j in 1:n.beta){
    beta.mean[j] ~ dnorm(pr.beta[j,1], sd=1)
    beta.prec[j] ~ dgamma(prec.b[j,1], prec.b[j,2])
  }

  for(site in 1:nsite){
	  ### priors
    phi.l.mu[site] ~ dnorm(phi.l.mean, tau = phi.l.prec)
    phi.n.mu[site] ~ dnorm(phi.n.mean, tau = phi.n.prec)
    phi.a.mu[site] ~ dnorm(phi.a.mean, tau = phi.a.prec)
    theta.ln[site] ~ dnorm(theta.ln.mean, tau = theta.ln.prec)
    theta.na[site] ~ dnorm(theta.na.mean, tau = theta.na.prec)
	  # Params can also be drawn from prior distributions
	  # But this is fine for now: an informative prior
	  # Based on previous data/model iterations
	  
	  for (j in 1:n.beta) {
	    # Betas are the linear covariates for mice/weather linear models
	    beta[j, site] ~ dnorm(beta.mean[j], tau = beta.prec[j])
	  }

		tau.temp[site] ~ dexp(1)
		tau.maxrh[site] ~ dexp(1)
		tau.minrh[site] ~ dexp(1)
		tau.precip[site] ~ dexp(1)

	  ### first latent process
		for (i in 1:4) {
		  x[i, 1, site] ~ dgamma(shape=IC[i, 1, site], rate = IC[i, 2, site])
		}
		
		# Set up nodes that don't vary by site
		for (i in 1:ns) { # ns = number of life stages
		  sig[i,site] ~ dinvgamma(pr.sig[i, 1], pr.sig[i, 2]) 
		}
		
		### precision priors with process error
		OMEGA[1, 1, site] <- sig[1, site]
		OMEGA[2, 2, site] <- sig[2, site]
		OMEGA[3, 3, site] <- sig[3, site]
		OMEGA[4, 4, site] <- sig[4, site]
		
		# Cholesky decomposition
		Ochol[1:4, 1:4, site] <- chol(OMEGA[1:4, 1:4, site])
		
		# Omega and the Cholensky decomp are eventually used
		# To estimate questing ticks

	  ### define parameters
	  for (t in 1:horizon) {
		  # loop over every day in time series
	    
	    # Mice inputs for life stage transitions
      if (miceAndWeather) {
			  logit(l2n[t, site]) <- theta.ln[site] + beta[13, site] * mice[t, site]
			  logit(n2a[t, site]) <- theta.na[site] + beta[14, site] * mice[t, site]
		  } else {
			  logit(l2n[t, site]) <- theta.ln[site]
			  logit(n2a[t, site]) <- theta.na[site]
		  }

		  theta.n2a[t, site] <- if_else_nimble(
			  (cgdd[t, site] <= 1000) | (cgdd[t, site] >= 2500),
			  n2a[t, site],
			  0
		  )
		  
		  # lambda is part of transition matrix... I think reproduction?
		  lambda[t, site] <- if_else_nimble(
			  (cgdd[t, site] >= 1400) & (cgdd[t, site] <= 2500),
			  repro.mu,
			  0
		  )
		  l2n.quest[t, site] <- if_else_nimble((cgdd[t, site] >= 400) & 
		                                   (cgdd[t, site] <= 2500), 1, 0)

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
				  beta[1, site] * x1[t, site] +
				  beta[2, site] * x2[t, site] +
				  beta[3, site] * x3[t, site] +
				  beta[4, site] * x4[t, site]

			  logit(phi.n[t, site]) <- phi.n.mu[site] +
				  beta[5, site] * x1[t, site] +
				  beta[6, site] * x2[t, site] +
				  beta[7, site] * x3[t, site] +
			  	beta[8, site] * x4[t, site]

			  logit(phi.a[t, site]) <- phi.a.mu[site] +
				  beta[9, site] * x1[t, site] +
				  beta[10, site] * x2[t, site] +
				  beta[11, site] * x3[t, site] +
				  beta[12, site] * x4[t, site]
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
		    dx[1,t,p,site] <- x[1, t, site] / 450 * area[t, p, site]
		    dx[2,t,p,site] <- x[3, t, site] / 450 * area[t, p, site]
		    dx[3,t,p,site] <- x[4, t, site] / 450 * area[t, p, site]
		    
			  y[1, t, p, site] ~ dpois(dx[1,t,p,site])
			  y[3, t, p, site] ~ dpois(dx[2,t,p,site])
			  y[4, t, p, site] ~ dpois(dx[3,t,p,site])
			  
			  # dx is an intermediate object and doesn't include dormant nymphs
		  }
	  }

	  for (t in 2:horizon) {
	    # Figure out how to do this as a wishart with moment-matching
	    # from a multivariate normal -------------------
		  # expected number questing
		  Ex[1:4, t, site] <- A[1:4, 1:4, t-1, site] %*% x[1:4, t-1, site]

		  x[1:ns, t, site] ~
			  dmnorm(mean = Ex[1:ns, t, site], 
			         cholesky = Ochol[1:ns, 1:ns, site], prec_param = 0)
		}
  }
}
)
