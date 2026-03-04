run_transfer_nimble <- function(
	cl,
	model,
	data,
	constants,
	inits,
	n.iter,
	miceAndWeather,
	use.daymet
) {
	library(parallel)
	library(nimble)
	library(coda)

	source("./DataProcessing/functions_hierarchical.R")

	n.cores <- length(cl) # number of cores used

	export.vec <- c(
		"model",
		"constants",
		"data",
		"n.iter",
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

		nimbleOptions(#'MCMCjointlySamplePredictiveBranches' = FALSE,
		              'MCMCusePredictiveDependenciesInCalculations' = TRUE,
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
		mcmcConf <- configureMCMC(cModel, onlyRW = TRUE)
		
		mcmcConf$addSampler(target = mcmcConf$monitors, type = 'NUTS')

		Rmcmc <- buildMCMC(mcmcConf)
		Cmcmc <- compileNimble(Rmcmc)
		Cmcmc$run(niter = n.iter, nburnin = n.iter / 2)
		return(as.list(Cmcmc$mvSamples))
	})

	out.mcmc <- as.mcmc(out)
	
	return(out.mcmc)
}
