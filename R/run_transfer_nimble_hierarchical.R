run_transfer_nimble <- function(
	cl,
	model,
	data,
	constants,
	inits,
	n.iter,
	parms,
	miceAndWeather,
	use.daymet
) {
	library(parallel)
	library(nimble)
	library(coda)

	source("./DataProcessing/functions_hierarchical.R")

	n.cores <- length(cl) # number of cores used

	# Need to add nimble function here?
	export.vec <- c(
		"model",
		"constants",
		"data",
		"n.iter",
		"parms",
		"if_else_nimble",
		"miceAndWeather",
		"use.daymet"
	)

	clusterExport(cl, export.vec, envir = environment())

	# export inits to clusters 
	for (j in seq_along(cl)) {
		set.seed(j)
		init <- inits()
		clusterExport(cl[j], "init", envir = environment())
	}

	message("Running mcmc...")
	out <- clusterEvalQ(cl, {
		# sample on each cluster
		library(nimble)
		library(coda)
	  library(nimbleHMC)

		nimbleOptions('MCMCusePredictiveDependenciesInCalculations' = TRUE,
		              unsupportedDerivativeHandling='warn',
		              enableDerivs = TRUE)
		
		model <- nimbleModel(
			model,
			constants = constants,
			data = data,
			inits = init,
			buildDerivs=T
		)  
		cModel <- compileNimble(model)
		
		# # Test for infinite values
		# test <- names(cModel)[str_detect(names(cModel), "logProb")]
		# test <- test[str_detect(test, "\\.")==F]
		# for(i in 1:length(test)){
		#   prb <- any(is.infinite(cModel[[test[i]]]))
		#   if(prb==T){print(paste(test[i], "is infinite", sep = " "))}
		# }

		mcmcConf <- configureMCMC(cModel, onlyRW = TRUE, monitors = parms) 
		
		mcmcConf$addSampler(target = mcmcConf$monitors, type = 'NUTS')

		Rmcmc <- buildMCMC(mcmcConf)
		Cmcmc <- compileNimble(Rmcmc)
		Cmcmc$run(niter = n.iter, nburnin = n.iter / 2)
		return(as.list(Cmcmc$mvSamples))
	})

	out.mcmc <- as.mcmc(out)
	
	return(out.mcmc)
}
