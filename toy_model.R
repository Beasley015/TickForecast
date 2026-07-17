#############################################
# Toy model to test sequential updating idea
# Part of NEON/Cary tick forecasting project
# E.M. Beasley
# Summer 2026
#############################################

# Packages and global variables ------------
library(boot)
library(R2jags)
library(abind)

set.seed(10)
time.steps <- 20
var.seq <- seq(from = -2, to = 2, length.out = time.steps+1)

# Generate environmental variables ---------------
var1 <- -var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)
var2 <-  (var.seq)^2 + var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)

# Coefficients -----------------
stage1.beta <- c(-2, 0, 2)
stage2.beta <- sample(c(-2, 0, 2), size = 3, replace = F)
transition.beta <- sample(c(-2,0,2), size = 3, replace = F)

# Survival/transition probs ---------------
get.probs <- function(param, variable){
  prob <- matrix(NA, nrow = time.steps+1, ncol = 3)
  for(i in 1:3){
    prob[,i] <- inv.logit(param[i]*variable)
  }
  return(prob)
}

stage1 <- get.probs(stage1.beta, var1)
stage2 <- get.probs(stage2.beta, var1)
transition <- get.probs(transition.beta, var2)

repro <- rpois(n=1, lambda = 10)

# Format into stage-structured arrays ---------------
# dims: [2,2,time.steps+1, sites (3)]
A <- array(0, dim = c(2,2,time.steps+1, 3))

A[1,1,,] <- stage1*(1-transition)
A[1,2,,] <- repro
A[2,2,,] <- stage2

# Generate latent time series -----------------
# Starting population
start.pop <- c(50,50)

# Create time series array
ts <- array(NA, dim = c(2, time.steps, 3))
ts[,1,] <- start.pop

# Fill in values from transition matrix
for(t in 2:(time.steps)){
  for(site in 1:3){
    ts[,t,site] <- round(A[,,t-1,site] %*% ts[,t-1,site]) 
  }
}

# Create sampling history ---------------------
# Sampling dates
site1.days <- sort(sample(1:time.steps, 14, replace = F))
site2.days <- sort(sample(1:time.steps, 14, replace = F))
site3.days <- sort(sample(1:time.steps, 14, replace = F))

sampling.history <- cbind(site1.days, site2.days, site3.days)

# Samples
samples <- array(NA, dim = dim(ts))

for(i in 1:nrow(sampling.history)){
  row = as.matrix(sampling.history[i,])
  
  samples[,row[1],1] <- rbinom(n = 2, size = ts[,row[1],1], prob = 0.7)
  samples[,row[2],2] <- rbinom(n = 2, size = ts[,row[2],2], prob = 0.7)
  samples[,row[3],3] <- rbinom(n = 2, size = ts[,row[3],3], prob = 0.7)
}

# Model script --------------------
toy.model <- function(){
  # Global priors
  mu.int1 ~ dnorm(int.mu1[1], int.mu1[2])
  tau.int1 ~ dgamma(int.tau1[1], int.tau1[2])
  
  mu.int2 ~ dnorm(int.mu2[1], int.mu2[2])
  tau.int2 ~ dgamma(int.tau2[1], int.tau2[2])
  
  mu.int3 ~ dnorm(int.mu3[1], int.mu3[2])
  tau.int3 ~ dgamma(int.tau3[1], int.tau3[2])
  
  mu.b1 ~ dnorm(b1.mu.pr[1], b1.mu.pr[2])
  tau.b1 ~ dgamma(b1.tau.pr[1], b1.tau.pr[2])
  
  mu.b2 ~ dnorm(b2.mu.pr[1], b2.mu.pr[2])
  tau.b2 ~ dgamma(b2.tau.pr[1], b2.tau.pr[2])
  
  mu.b3 ~ dnorm(b3.mu.pr[1], b3.mu.pr[2])
  tau.b3 ~ dgamma(b3.tau.pr[1], b3.tau.pr[2])
  
  sample.prob ~ dbeta(1,1)
  lambda ~ dgamma(1,1)
  
  # Starting value for x
  for(stage in 1:2){
    for(site in 1:3){
      x[stage,1,site] ~ dpois(pr.x[stage,site])
    }
  }
  
  for(site in sampled.sites){
    # site-level priors
    int1[site] ~ dnorm(mu.int1, tau.int1)
    int2[site] ~ dnorm(mu.int2, tau.int2)
    int3[site] ~ dnorm(mu.int3, tau.int3)
    
    beta1[site] ~ dnorm(mu.b1, tau.b1)
    beta2[site] ~ dnorm(mu.b2, tau.b2)
    beta3[site] ~ dnorm(mu.b3, tau.b3)
    
    for(t in 1:steps){
      # Components of transition matrix
      logit(survival1[t,site]) <- int1[site] + beta1[site]*coef1[t]
      logit(survival2[t,site]) <- int2[site] + beta2[site]*coef1[t]
      logit(transition[t,site]) <- int3[site] + beta3[site]*coef2[t]
      
      repro[t,site] ~ dpois(lambda)
      
      # Transition matrix
      A[1,1,t,site] <- survival1[t,site]*(1-transition[t,site])
      A[1,2,t,site] <- repro[t,site]
      A[2,1,t,site] <- 0
      A[2,2,t,site] <- survival2[t,site]
      
      # Sampling error
      for(stage in 1:2){
        y[stage,t,site] ~ dbin(sample.prob, x[stage,t,site])
      }
    }
    
    for(t in 2:(steps+1)){
      # Forecast
      ex[1:2,t,site] <- A[1:2,1:2,t-1,site] %*% x[1:2,t-1,site]
      
      for(stage in 1:2){
        x[stage,t,site] ~ dpois(ex[stage,t,site])
      }
    }
  }
}

# Model workflow -----------------------
model.outs <- list()
for(i in 1:time.steps){
  if(i == 1){
    int.mu1 <- c(0,1)
    int.tau1 <- c(1,1)
    
    int.mu2 <- c(0,1)
    int.tau2 <- c(1,1)
    
    int.mu3 <- c(0,1)
    int.tau3 <- c(1,1)
    
    b1.mu.pr <- c(0,1)
    b1.tau.pr <- c(1,1)
    
    b2.mu.pr <- c(0,1)
    b2.tau.pr <- c(1,1)
    
    b3.mu.pr <- c(0,1)
    b3.tau.pr <- c(1,1)
    
    pr.x <- matrix(5, nrow = 2, ncol = 3)
    
    site <- which(colSums(is.na(samples[,i,]))==0)
    
    steps <- 2
    
    obs <- array(NA, dim = c(2, time.steps, 3))
    for(j in 1:length(site)){
      obs[,i,site[j]] <- samples[,i,site[j]]
    }
    
    data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
                 int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
                 b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
                 b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
                 coef1=var1, coef2=var2, steps=steps, y=obs, pr.x = pr.x,
                 sampled.sites=site)
    params <- c("int1", "int2", "int3", "beta1", "beta2", "beta3", "lambda", 
                "mu.int1", "tau.int1", "mu.int2", "tau.int2", "mu.int3",
                "tau.int3", "mu.b1", "tau.b1", "mu.b2", "tau.b2", "mu.b3",
                "tau.b3", "x", "ex", "sample.prob", "pr.x")
    
    inits <- function(){
      list(
        int1 = rep(0,3),
        int2 = rep(0,3),
        int3 = rep(0,3),
        beta1 = rep(0,3),
        beta2 = rep(0,3),
        beta3 = rep(0,3),
        lambda = rgamma(1,1,1),
        x = array(rpois(steps*3*2, mean(obs, na.rm = T)), dim = c(2,steps,3)),
        sample.prob = dunif(1,0,1),
        A = array(0, dim(A))
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = toy.model,
                n.chains = 3, n.iter=2000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",site, sep = "")
    colnames(outs$beta2) <- paste("site",site, sep = "")
    colnames(outs$beta3) <- paste("site", site, sep = "")
    colnames(outs$int1) <- paste("site",site, sep = "")
    colnames(outs$int2) <- paste("site",site, sep = "")
    colnames(outs$int3) <- paste("site",site, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:3, sep = "")
    model.outs[[i]] <- outs
    
    priors <- data.frame(int.mu1 = c(mean(outs$mu.int1), 1/var(outs$mu.int1)),
                   int.tau1 = c(mean(outs$tau.int1), 1/var(outs$tau.int1)),
                   int.mu2 = c(mean(outs$mu.int2), 1/var(outs$mu.int2)),
                   int.tau2 = c(mean(outs$tau.int2), 1/var(outs$tau.int2)),
                   int.mu3 = c(mean(outs$mu.int3), 1/var(outs$mu.int3)),
                   int.tau3 = c(mean(outs$tau.int3), 1/var(outs$tau.int3)),
                   b1.mu.pr = c(mean(outs$mu.b1), 1/var(outs$mu.b1)),
                   b1.tau.pr = c(mean(outs$tau.b1), 1/var(outs$tau.b1)),
                   b2.mu.pr = c(mean(outs$mu.b2), 1/var(outs$mu.b2)),
                   b2.tau.pr = c(mean(outs$tau.b2), 1/var(outs$tau.b2)),
                   b3.mu.pr = c(mean(outs$mu.b3), 1/var(outs$mu.b3)),
                   b3.tau.pr = c(mean(outs$tau.b3), 1/var(outs$tau.b3)))
    
    pr.x <- apply(outs$x, c(2,4), median)
    
  } else{
    int.mu1 <- priors$int.mu1
    int.tau1 <- priors$int.tau1
    
    int.mu2 <- priors$int.mu2
    int.tau2 <- priors$int.tau2
    
    int.mu3 <- priors$int.mu3
    int.tau3 <- priors$int.tau3
    
    b1.mu.pr <- priors$b1.mu.pr
    b1.tau.pr <- priors$b1.tau.pr
    
    b2.mu.pr <- priors$b2.mu.pr
    b2.tau.pr <- priors$b2.tau.pr
    
    b3.mu.pr <- priors$b3.mu.pr
    b3.tau.pr <- priors$b3.tau.pr
    
    pr.x <- pr.x
    
    site <- which(colSums(is.na(samples[,i,]))==0)
    if(length(site) == 0){next}
    
    steps <- ifelse(time.steps+1- i < 2, time.steps+1 - i, 2)
    
    obs <- array(NA, dim = c(2, time.steps-steps, 3))
    for(j in 1:length(site)){
      obs[,1,site[j]] <- samples[,i,site[j]]
    }
    
    data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
                 int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
                 b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
                 b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
                 coef1=var1, coef2=var2, steps=steps, y=obs, pr.x = pr.x,
                 sampled.sites=site)
    params <- c("int1", "int2", "int3", "beta1", "beta2", "beta3", "lambda", 
                "mu.int1", "tau.int1", "mu.int2", "tau.int2", "mu.int3",
                "tau.int3", "mu.b1", "tau.b1", "mu.b2", "tau.b2", "mu.b3",
                "tau.b3", "x", "ex", "sample.prob")
    
    inits <- function(){
      list(
        int1 = rep(0,3),
        int2 = rep(0,3),
        int3 = rep(0,3),
        beta1 = rep(0,3),
        beta2 = rep(0,3),
        beta3 = rep(0,3),
        lambda = rgamma(1,1,1),
        x = array(rpois(steps*3*2, mean(obs, na.rm = T)), dim = c(2,steps,3)),
        sample.prob = dunif(1,0,1),
        A = array(0, dim(A))
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = toy.model,
                n.chains = 3, n.iter=2000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",site, sep = "")
    colnames(outs$beta2) <- paste("site",site, sep = "")
    colnames(outs$beta3) <- paste("site", site, sep = "")
    colnames(outs$int1) <- paste("site",site, sep = "")
    colnames(outs$int2) <- paste("site",site, sep = "")
    colnames(outs$int3) <- paste("site",site, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:3, sep = "")
    model.outs[[i]] <- outs
    
    priors <- data.frame(int.mu1 = c(mean(outs$mu.int1), 1/var(outs$mu.int1)),
                         int.tau1 = c(mean(outs$tau.int1), 1/var(outs$tau.int1)),
                         int.mu2 = c(mean(outs$mu.int2), 1/var(outs$mu.int2)),
                         int.tau2 = c(mean(outs$tau.int2), 1/var(outs$tau.int2)),
                         int.mu3 = c(mean(outs$mu.int3), 1/var(outs$mu.int3)),
                         int.tau3 = c(mean(outs$tau.int3), 1/var(outs$tau.int3)),
                         b1.mu.pr = c(mean(outs$mu.b1), 1/var(outs$mu.b1)),
                         b1.tau.pr = c(mean(outs$tau.b1), 1/var(outs$tau.b1)),
                         b2.mu.pr = c(mean(outs$mu.b2), 1/var(outs$mu.b2)),
                         b2.tau.pr = c(mean(outs$tau.b2), 1/var(outs$tau.b2)),
                         b3.mu.pr = c(mean(outs$mu.b3), 1/var(outs$mu.b3)),
                         b3.tau.pr = c(mean(outs$tau.b3), 1/var(outs$tau.b3)))
    
    pr.x <- apply(outs$x, c(2,4), median)
  }
}
