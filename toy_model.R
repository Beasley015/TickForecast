#############################################
# Toy model to test sequential updating idea
# Part of NEON/Cary tick forecasting project
# E.M. Beasley
# Summer 2026
#############################################

# Packages and global variables ------------
library(R2jags)

set.seed(15)
time.steps <- 20
var.seq <- seq(from = -2, to = 2, length.out = time.steps+1)

# Generate environmental variables ---------------
var1 <- var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)
var2 <-  -(var.seq)^2 + var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)

# Coefficients -----------------
coef1 <- c(-2, 0, 2)
coef2 <- sample(c(-2, 0, 2), size = 3, replace = F)

# Generate latent time series -----------------
# lambda values
lambda1 <- logical(length = time.steps)
lambda2 <- logical(length = time.steps)
lambda3 <- logical(length = time.steps)

lambda1[1] <- exp(0+coef1[1]*var1[1]+coef2[1]+var2[1])
lambda2[1] <- exp(0+coef1[2]*var1[1]+coef2[2]+var2[1])
lambda3[1] <- exp(0+coef1[3]*var1[1]+coef2[3]+var2[1])

for(i in 2:time.steps){
  lambda1[i] <- exp(0+coef1[1]*var1[i]+coef2[1]+var2[i])
  lambda2[i] <- exp(0+coef1[2]*var1[i]+coef2[2]+var2[i])
  lambda3[i] <- exp(0+coef1[3]*var1[i]+coef2[3]+var2[i])
}

# Time series
ts1 <- logical()
ts2 <- logical()
ts3 <- logical()

for(i in 1:time.steps){
  ts1[i] <- rpois(1, lambda1[i])
  ts2[i] <- rpois(1, lambda2[i])
  ts3[i] <- rpois(1, lambda3[i])
}

# Create sampling history ---------------------
# Sampling dates
site1.days <- sort(sample(1:time.steps, 10, replace = F))
site2.days <- sort(sample(1:time.steps, 10, replace = F))
site3.days <- sort(sample(1:time.steps, 10, replace = F))

sampling.history <- cbind(site1.days, site2.days, site3.days)

# Samples
samples <- data.frame(site1=rep(NA, time.steps),
                      site2=rep(NA, time.steps),
                      site3=rep(NA, time.steps))

for(i in 1:nrow(sampling.history)){
  row = as.matrix(sampling.history[i,])
  
  samples$site1[row[1]] <- rbinom(n = 1, size = ts1[row[1]], prob = 0.8)
  samples$site2[row[2]] <- rbinom(n = 1, size = ts2[row[2]], prob = 0.8)
  samples$site3[row[3]] <- rbinom(n = 1, size = ts3[row[3]], prob = 0.8)
}

# Model script --------------------
toy.model <- function(){
  # Global priors
  mu.int ~ dnorm(int.mu.pr[1], int.mu.pr[2])
  tau.int ~ dgamma(int.tau.pr[1], int.tau.pr[2])
  
  mu.b1 ~ dnorm(b1.mu.pr[1], b1.mu.pr[2])
  tau.b1 ~ dgamma(b1.tau.pr[1], b1.tau.pr[2])
  
  mu.b2 ~ dnorm(b2.mu.pr[1], b2.mu.pr[2])
  tau.b2 ~ dgamma(b2.tau.pr[1], b2.tau.pr[2])
  
  for(site in sampled.sites){
    # site-level priors
    int[site] ~ dnorm(mu.int, tau.int)
    beta1[site] ~ dnorm(mu.b1, tau.b1)
    beta2[site] ~ dnorm(mu.b2, tau.b2)
    
    for(t in 1:steps){
      # Latent state
      log(lambda[t,site]) <- int[site] + beta1[site]*coef1[t] + 
        beta2[site]*coef2[t]
      
      # Sampling error
      y[t,site] ~ dpois(lambda[t,site])
    }
    
    for(t in 2:(steps+1)){
      # Forecast
      ex.lambda[t,site] ~ dpois(lambda[t-1, site])
    }
  }
}

# Model workflow -----------------------
model.outs <- list()
for(i in 1:time.steps){
  if(i == 1){
    int.mu.pr <- c(0,1)
    int.tau.pr <- c(1,1)
    
    b1.mu.pr <- c(0,1)
    b1.tau.pr <- c(1,1)
    
    b2.mu.pr <- c(0,1)
    b2.tau.pr <- c(1,1)
    
    site <- which(is.na(samples[i,])==F)
    
    steps <- 5
    
    data <- list(int.mu.pr=int.mu.pr, int.tau.pr=int.tau.pr, b1.mu.pr=b1.mu.pr,
                 b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, b2.tau.pr=b2.tau.pr,
                 coef1=var1, coef2=var2, steps=steps, y=samples[i:(i+5),], 
                 sampled.sites=site)
    params <- c("int", "beta1", "beta2", "lambda", "mu.int", "tau.int",
                "mu.b1", "tau.b1", "mu.b2", "tau.b2")
    inits <- function(){
      list(
        int = rep(0,3),
        beta1 = rep(0,3),
        beta2 = rep(0,3),
        lambda = matrix(1, nrow = steps, ncol = 3)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = toy.model,
                n.chains = 3, n.iter=2000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",site, sep = "")
    colnames(outs$beta2) <- paste("site",site, sep = "")
    colnames(outs$int) <- paste("site",site, sep = "")
    dimnames(outs$lambda)[[3]] <- paste("site",site, sep = "")
    model.outs[[i]] <- outs 
    
    priors <- list(int.mu.pr = c(mean(outs$mu.int), 1/var(outs$mu.int)),
                   int.tau.pr = c(mean(outs$tau.int), 1/var(outs$tau.int)),
                   b1.mu.pr = c(mean(outs$mu.b1), 1/var(outs$mu.b1)),
                   b1.tau.pr = c(mean(outs$tau.b1), 1/var(outs$tau.b1)),
                   b2.mu.pr = c(mean(outs$mu.b2), 1/var(outs$mu.b2)),
                   b2.tau.pr = c(mean(outs$tau.b2), 1/var(outs$tau.b2)))
    
  } else{
    int.mu.pr <- priors$int.mu.pr
    int.tau.pr <- priors$int.tau.pr
    
    b1.mu.pr <- priors$b1.mu.pr
    b1.tau.pr <- priors$b1.tau.pr
    
    b2.mu.pr <- priors$b2.mu.pr
    b2.tau.pr <- priors$b2.tau.pr
    
    site <- which(is.na(samples[i,])==F)
    if(length(site) == 0){next}
    
    steps <- ifelse(time.steps+1- i < 5, time.steps+1 - i, 5)
    
    if(steps == 5){
      subs <- samples[i:(i+5),]
    } else{
      subs <- samples[i:(i+steps),]
    }
    
    data <- list(int.mu.pr=int.mu.pr, int.tau.pr=int.tau.pr, b1.mu.pr=b1.mu.pr,
                 b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, b2.tau.pr=b2.tau.pr,
                 coef1=var1, coef2=var2, steps=steps, y=subs, 
                 sampled.sites=site)
    params <- c("int", "beta1", "beta2", "lambda", "mu.int", "tau.int",
                "mu.b1", "tau.b1", "mu.b2", "tau.b2")
    inits <- function(){
      list(
        int = rep(0,3),
        beta1 = rep(0,3),
        beta2 = rep(0,3),
        lambda = matrix(1, nrow = steps, ncol = 3)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = toy.model,
                n.chains = 3, n.iter=2000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",site, sep = "")
    colnames(outs$beta2) <- paste("site",site, sep = "")
    colnames(outs$int) <- paste("site",site, sep = "")
    dimnames(outs$lambda)[[3]] <- paste("site",site, sep = "")
    model.outs[[i]] <- outs 
    
    # Getting infinite values here; need to automate a check and fix
    priors <- list(int.mu.pr = c(mean(outs$mu.int), 1/var(outs$mu.int)),
                   int.tau.pr = c(mean(outs$tau.int), 1/var(outs$tau.int)),
                   b1.mu.pr = c(mean(outs$mu.b1), 1/var(outs$mu.b1)),
                   b1.tau.pr = c(mean(outs$tau.b1), 1/var(outs$tau.b1)),
                   b2.mu.pr = c(mean(outs$mu.b2), 1/var(outs$mu.b2)),
                   b2.tau.pr = c(mean(outs$tau.b2), 1/var(outs$tau.b2)))
  }
}
