library(nimble)
source("./DataProcessing/functions.R")

model.code <- nimbleCode({
	### priors (intercepts)
	phi.l.mu ~ dnorm(pr.phi.l[1], tau = pr.phi.l[2])
	phi.n.mu ~ dnorm(pr.phi.n[1], tau = pr.phi.n[2])
	phi.a.mu ~ dnorm(pr.phi.a[1], tau = pr.phi.a[2])
	theta.ln ~ dnorm(pr.theta.l2n[1], tau = pr.theta.l2n[2])
	theta.na ~ dnorm(pr.theta.n2a[1], tau = pr.theta.n2a[2])

	# priors for model coefficients
	for (j in 1:n.beta) {
		beta[j] ~ dnorm(pr.beta[j, 1], tau = pr.beta[j, 2])
	}
	
	# plot-level priors
	for(j in 1:3){
	  gam0[j] ~ dnorm(pr.gam0[j,1], tau=pr.gam0[j,2])
	  -gam1[j] ~ dgamma(pr.gam1[j,1], pr.gam1[j,2])
	  gam2[j] ~ dnorm(pr.gam2[j,1], tau=pr.gam2[j,2])
	}

	tau.temp ~ dexp(1)
	tau.maxrh ~ dexp(1)
	tau.minrh ~ dexp(1)
	tau.precip ~ dexp(1)

	for (i in 1:ns) {
		sig[i] ~ dinvgamma(pr.sig[i, 1], pr.sig[i, 2])
	}

	### precision priors with process error
	OMEGA[1, 1] <- sig[1]
	OMEGA[2, 2] <- sig[2]
	OMEGA[3, 3] <- sig[3]
	OMEGA[4, 4] <- sig[4]

	OMEGA[1, 2] <- 0
	OMEGA[1, 3] <- 0
	OMEGA[1, 4] <- 0
	OMEGA[2, 1] <- 0
	OMEGA[2, 3] <- 0
	OMEGA[2, 4] <- 0
	OMEGA[3, 1] <- 0
	OMEGA[3, 2] <- 0
	OMEGA[3, 4] <- 0
	OMEGA[4, 1] <- 0
	OMEGA[4, 2] <- 0
	OMEGA[4, 3] <- 0

	# Cholesky decomposition
	Ochol[1:ns, 1:ns] <- chol(OMEGA[1:ns, 1:ns])

	### first latent process
	for (i in 1:4) {
		x[i, 1] ~ T(dnorm(IC[i, 1], tau = IC[i, 2]), 0, Inf)
	}

  ### define parameters
	for (t in 1:horizon) {
		# loop over every day in time series

		logit(l2n[t]) <- theta.ln + beta[13] * mice[t]
		logit(n2a[t]) <- theta.na + beta[14] * mice[t]

		if(n.plots == 1){
		  mean.gdd <- gdd[t,1]
		  mean.pz3 <- pz[3,t,1]
		  mean.pz2 <- pz[2,t,1]
		  mean.pz1 <- pz[1,t,1]
		} else{
		  mean.gdd <- mean(gdd[t,1:n.plots])
		  mean.pz3 <- mean(pz[3,t,1:n.plots])
		  mean.pz2 <- mean(pz[2,t,1:n.plots])
		  mean.pz1 <- mean(pz[1,t,1:n.plots])
		}
		
		lambda.bin[t] ~ dbern(mean.pz3)
		lambda[t] <- if_else_nimble(lambda.bin[t]==0, repro.mu, 0)
		# look @ repro.mu: put a prior so it can vary?
		
		# Relate p's to site-level transition probs:
		n2a.bin[t] ~ dbern(mean.pz2)
		n2a.quest[t] <- if_else_nimble(n2a.bin[t]==0, n2a[t], 0)
		
		# Turn prob of zero from phenology into binary site-level yes/no
		l2n.bin[t] ~ dbern(mean.pz1)
		l2n.quest[t] <- if_else_nimble(l2n.bin[t]==0, 1, 0)

		maxtemp[t] ~ dnorm(x1[t], tau = tau.temp)
		maxrh[t] ~ dnorm(x2[t], tau = tau.maxrh)
		minrh[t] ~ dnorm(x3[t], tau = tau.minrh)
		precip[t] ~ dnorm(x4[t], tau = tau.precip)

		x1[t] ~ dnorm(0, 1)
		x2[t] ~ dnorm(0, 1)
		x3[t] ~ dnorm(0, 1)
		x4[t] ~ dnorm(0, 1)

		logit(phi.l[t]) <- phi.l.mu +
			beta[1] * x1[t] +
			beta[2] * x2[t] +
			beta[3] * x3[t] +
			beta[4] * x4[t]

		logit(phi.n[t]) <- phi.n.mu +
			beta[5] * x1[t] +
			beta[6] * x2[t] +
			beta[7] * x3[t] +
			beta[8] * x4[t]

		logit(phi.a[t]) <- phi.a.mu +
			beta[9] * x1[t] +
			beta[10] * x2[t] +
			beta[11] * x3[t] +
			beta[12] * x4[t]

		A[1, 1, t] <- phi.l[t] * (1 - l2n[t])
		A[1, 2, t] <- 0
		A[1, 3, t] <- 0
		A[1, 4, t] <- lambda[t]
		A[2, 1, t] <- phi.l[t] * l2n[t]
		A[2, 2, t] <- 1 - l2n.quest[t]
		A[2, 3, t] <- 0
		A[2, 4, t] <- 0
		A[3, 1, t] <- 0
		A[3, 2, t] <- l2n.quest[t]
		A[3, 3, t] <- phi.n[t] * (1 - n2a.quest[t])
		A[3, 4, t] <- 0
		A[4, 1, t] <- 0
		A[4, 2, t] <- 0
		A[4, 3, t] <- phi.n[t] * n2a.quest[t]
		A[4, 4, t] <- phi.a[t]

		### Data Model ###
		for (p in 1:n.plots) {
		  # Convert expected questing ticks to density
			dx[1, t, p] <- x[1, t] / 450 * area[t, p]
			dx[2, t, p] <- x[3, t] / 450 * area[t, p]
			dx[3, t, p] <- x[4, t] / 450 * area[t, p]
			
			# Prob available for sampling (p)
			logit(pz[1,t,p]) <- gam0[1] + gam1[1] * gdd[t,p]^2 + 
			  gam2[1] * gdd[t,p]
			logit(pz[2,t,p]) <- gam0[2] + gam1[2] * gdd[t,p]^2 + 
			  gam2[2] * gdd[t,p]
			logit(pz[3,t,p]) <- gam0[3] + gam1[3] * gdd[t,p]^2 + 
			  gam2[3] * gdd[t,p]
			
			# Tick density given sampling availability (dlamb)
			log(dlamb[1,t,p]) <- dx[1,t,p] # add habitat covs
			log(dlamb[2,t,p]) <- dx[2,t,p]
			log(dlamb[3,t,p]) <- dx[3,t,p]
			
			# Observed ticks follow zero-inflated Poisson
			y[1,t,p] ~ dZIP(dlamb=dlamb[1,t,p], zeroProb = 1-pz[1,t,p])
			y[3,t,p] ~ dZIP(dlamb=dlamb[2,t,p], zeroProb = 1-pz[2,t,p])
			y[4,t,p] ~ dZIP(dlamb=dlamb[3,t,p], zeroProb = 1-pz[3,t,p])
		}
	}

	for (t in 2:horizon) {
		# process error
		# expected number questing
		Ex[1:ns, t] <- A[1:ns, 1:ns, t - 1] %*% x[1:ns, t - 1]

		x[1:ns, t] ~
			dmnorm(mean = Ex[1:ns, t], cholesky = Ochol[1:ns, 1:ns], prec_param = 0)

		for (c in 1:ns) {
			xind[c, t] ~ dconstraint(x[c, t] >= 0)
		}

	}
})
