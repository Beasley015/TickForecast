library(nimble)
source("./DataProcessing/functions_hierarchical.R")

model.code <- nimbleCode({
  # # Hyperpriors for intercepts 
  # phi.l.mean ~ dnorm(pr.phi.l[1], tau=pr.phi.l[2])
  # phi.n.mean ~ dnorm(pr.phi.n[1], tau=pr.phi.n[2])
  # phi.a.mean ~ dnorm(pr.phi.a[1], tau=pr.phi.a[2])
  # 
  # theta.l2n.mean ~ dnorm(pr.theta.l2n[1], tau=pr.theta.l2n[2])
  # theta.n2a.mean ~ dnorm(pr.theta.n2a[1], tau=pr.theta.n2a[2])
  # 
  # # Hyperpriors for betas
  # for(j in 1:n.beta){
  #   beta.mean[j] ~ dnorm(pr.beta[j,1], tau=pr.beta[j,2])
  # }
  # 
  # # Shrinkage parameters
  # l.shrink ~ dgamma(phil.shrink[1], rate=phil.shrink[2])
  # n.shrink ~ dgamma(phin.shrink[1], rate=phin.shrink[2])
  # a.shrink ~ dgamma(phia.shrink[1], rate=phia.shrink[2])
  # 
  # ln.shrink ~ dgamma(l2n.shrink[1], rate=l2n.shrink[2])
  # na.shrink ~ dgamma(n2a.shrink[1], rate=n2a.shrink[2])
  # 
  # for(j in 1:n.beta){
  #   beta.shrink[j] ~ dgamma(pr.beta.shrink[j,1], rate=pr.beta.shrink[j,2])
  # }
  # 
  # # Taus for site-level parameters
  # phi.l.tau ~ dgamma(pr.phil.tau[1], rate=pr.phil.tau[2])
  # phi.n.tau ~ dgamma(pr.phin.tau[1], rate=pr.phin.tau[2])
  # phi.a.tau ~ dgamma(pr.phia.tau[1], rate=pr.phia.tau[2])
  # 
  # ln.tau ~ dgamma(pr.ln.tau[1], rate=pr.ln.tau[2])
  # na.tau ~ dgamma(pr.na.tau[1], rate=pr.na.tau[2])
  # 
  # for(j in 1:n.beta){
  #   beta.tau[j] ~ dgamma(pr.beta.tau[j,1], rate=pr.beta.tau[j,2])
  # }

  for(site in 1:nsite){
	  ### priors
#     dev.phi.l[site] ~ dnorm(dev.l.pr[site,1], tau=dev.l.pr[site,2])
#     dev.phi.a[site] ~ dnorm(dev.a.pr[site,1], tau=dev.a.pr[site,2])
#     dev.phi.n[site] ~ dnorm(dev.n.pr[site,1], tau=dev.n.pr[site,2])
#     
#     phi.l.mu[site] ~ dnorm(phi.l.mean + (l.shrink*dev.phi.l[site]), 
#                            tau=phi.l.tau)
#     phi.n.mu[site] ~ dnorm(phi.n.mean + (n.shrink*dev.phi.n[site]), 
#                            tau=phi.n.tau)
#     phi.a.mu[site] ~ dnorm(phi.a.mean + (a.shrink*dev.phi.a[site]), 
#                            tau=phi.a.tau)
#     
#     dev.ln[site] ~ dnorm(dev.ln.pr[site,1], tau=dev.ln.pr[site,2])
#     dev.na[site] ~ dnorm(dev.na.pr[site,1], tau=dev.na.pr[site,2])
#     
#     theta.ln[site] ~ dnorm(theta.l2n.mean + (ln.shrink*dev.ln[site]),
#                            tau=ln.tau)
# 	  theta.na[site] ~ dnorm(theta.n2a.mean + (na.shrink*dev.na[site]),
# 	                          tau=na.tau)
# 	  
# 	  for(j in 1:n.beta){
# 	    dev.beta[j,site] ~ dnorm(dev.beta.mu[j,site], tau=dev.beta.tau[j,site])
# 	    beta[j,site] ~ dnorm(beta.mean[j] + (beta.shrink[j]*dev.beta[j,site]),
# 	                         tau=beta.tau[j])
	  # }
    
    phi.l.mu[site] ~ dnorm(pr.phi.l[site,1], tau = pr.phi.l[site,2])
    phi.n.mu[site] ~ dnorm(pr.phi.n[site,1], tau = pr.phi.n[site,2])
    phi.a.mu[site] ~ dnorm(pr.phi.a[site,1], tau = pr.phi.a[site,2])
    
    theta.ln[site] ~ dnorm(pr.ln[site,1], tau = pr.ln[site,2])
    theta.na[site] ~ dnorm(pr.na[site,1], tau = pr.na[site,2])
    
    for(j in 1:n.beta){
      beta[j,site] ~ dnorm(pr.beta.mu[j,site], tau = pr.beta.tau[j,site])
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
		  
		  # lambda is reproduction in transition matrix
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
	    # Figure out alternative distribution here
		  # expected number questing
		  Ex[1:4, t, site] <- A[1:4, 1:4, t-1, site] %*% x[1:4, t-1, site]

		  x[1:ns, t, site] ~
			  dmnorm(mean = Ex[1:ns, t, site], 
			         cholesky = Ochol[1:ns, 1:ns, site], prec_param = 0)
		}
  }
}
)
