run_transfer_nimble <- function(
	cl,
	model,
	dat,
	constant,
	inits,
	n.iter,
	parms
) {
	library(nimble)
	library(coda)

	source("./DataProcessing/functions_hierarchical.R")

	nimbleOptions('MCMCjointlySamplePredictiveBranches' = FALSE)
	
	init <- inits()

	model <- nimbleModel(
	  model,
		constants = constant,
		data = dat,
		inits = init)
		
	cModel <- compileNimble(model)
	
	# Test for infinite values
	# test <- names(cModel)[str_detect(names(cModel), "logProb")]
	# test <- test[str_detect(test, "\\.")==F]
	# for(i in 1:length(test)){
	#   prb <- any(is.infinite(cModel[[test[i]]]))
	#   if(prb==T){print(paste(test[i], "is infinite", sep = " "))}
	# }

	mcmcConf <- configureMCMC(cModel, onlyRW = TRUE, monitors = parms)
	Rmcmc <- buildMCMC(mcmcConf)
	Cmcmc <- compileNimble(Rmcmc)
	Cmcmc$run(niter = n.iter, nburnin = n.iter / 2)
	return(as.mcmc(as.matrix(Cmcmc$mvSamples)))
}
