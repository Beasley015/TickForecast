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
library(tidyverse)
library(patchwork)
library(fitdistrplus)
library(viridis)

set.seed(10)
time.steps <- 20
var.seq <- seq(from = -2, to = 2, length.out = time.steps+1)
sites <- 10

# Generate environmental variables ---------------
var1 <- var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)
var2 <-  (var.seq)^2 + var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)

# Coefficients -----------------
stage1.beta <- sample(c(-2,0,2), size = sites, replace = T)
stage2.beta <- sample(c(-2, 0, 2), size = sites, replace = T)
transition.beta <- sample(c(-2,0,2), size = sites, replace = T)

# Survival/transition probs ---------------
get.probs <- function(param, variable, intercept){
  prob <- matrix(NA, nrow = time.steps+1, ncol = sites)
  for(i in 1:sites){
    prob[,i] <- inv.logit(intercept + param[i]*variable)
  }
  return(prob)
}

stage1 <- get.probs(stage1.beta, var2, 0)
stage2 <- get.probs(stage2.beta, var2, 0)
transition <- get.probs(transition.beta, var1, 0)

repro <- rpois(n=1, lambda = 2)

# Format into stage-structured arrays ---------------
# dims: [2,2,time.steps+1, sites (3)]
A <- array(0, dim = c(2,2,time.steps+1, sites))

A[1,1,,] <- stage1*(1-transition)
A[1,2,,] <- repro
A[2,1,,] <- stage1*transition
A[2,2,,] <- stage2

# Generate latent time series -----------------
# Starting population
start.pop <- c(50,50)

# Create time series array
ts <- array(NA, dim = c(2, time.steps, sites))
ts[,1,] <- start.pop

# Fill in values from transition matrix
for(t in 2:(time.steps)){
  for(site in 1:sites){
    ts[,t,site] <- round(A[,,t-1,site] %*% ts[,t-1,site]) 
  }
}

# Create sampling history ---------------------
# Sampling dates
# site1.days <- sort(sample(1:time.steps, 14, replace = F))
# site2.days <- sort(sample(1:time.steps, 14, replace = F))
# site3.days <- sort(sample(1:time.steps, 14, replace = F))
# 
# sampling.history <- cbind(site1.days, site2.days, site3.days)
# 
# # Samples
# samples <- array(NA, dim = dim(ts))
# 
# for(i in 1:nrow(sampling.history)){
#   row = as.matrix(sampling.history[i,])
#   
#   samples[,row[1],1] <- rbinom(n = 2, size = ts[,row[1],1], prob = 0.7)
#   samples[,row[2],2] <- rbinom(n = 2, size = ts[,row[2],2], prob = 0.7)
#   samples[,row[3],3] <- rbinom(n = 2, size = ts[,row[3],3], prob = 0.7)
# }

samples <- array(NA, dim = c(dim(ts),4))

for(i in 1:4){
  samples[,,,i] <- array(rbinom(n = ts, size = ts, prob = 0.7), dim = dim(ts))
}

# Model script --------------------
# base model 
model.base <- function(){
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
  
  sample.prob ~ dbeta(5,5)
  lambda ~ dgamma(1,1)
  
  # Starting value for x
  for(stage in 1:2){
    for(site in 1:sites){
      x[stage,1,site] ~ dpois(pr.x[stage,site])
    }
  }
  
  for(site in 1:sites){
    # site-level priors
    int1[site] ~ dnorm(mu.int1, tau.int1)
    int2[site] ~ dnorm(mu.int2, tau.int2)
    int3[site] ~ dnorm(mu.int3, tau.int3)
    
    beta1[site] ~ dnorm(mu.b1, tau.b1)
    beta2[site] ~ dnorm(mu.b2, tau.b2)
    beta3[site] ~ dnorm(mu.b3, tau.b3)
    
    for(t in 1:steps){
      # Components of transition matrix
      logit(survival1[t,site]) <- int1[site] + beta1[site]*coef2[t]
      logit(survival2[t,site]) <- int2[site] + beta2[site]*coef2[t]
      logit(transition[t,site]) <- int3[site] + beta3[site]*coef1[t]
      
      repro[t,site] ~ dpois(lambda)
      
      # Transition matrix
      A[1,1,t,site] <- survival1[t,site]*(1-transition[t,site])
      A[1,2,t,site] <- repro[t,site]
      A[2,1,t,site] <- survival1[t,site]*transition[t,site]
      A[2,2,t,site] <- survival2[t,site]
      
      # Sampling error
      for(stage in 1:2){
        for(sample in 1:4){
          y[stage,t,site,sample] ~ dbin(sample.prob, x[stage,t,site])
        }
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

# PP-RB part 1: single-site models
pprb.phase1 <- function(){
  # Starting value for x
  for(stage in 1:2){
    for(site in 1:sites){
      x[stage,1,site] ~ dpois(pr.x[stage,site])
    }
  }
  
  for(site in 1:sites){
    # site-level priors
    int1[site] ~ dnorm(mu.int1[site], tau.int1[site])
    int2[site] ~ dnorm(mu.int2[site], tau.int2[site])
    int3[site] ~ dnorm(mu.int3[site], tau.int3[site])
    
    beta1[site] ~ dnorm(mu.b1[site], tau.b1[site])
    beta2[site] ~ dnorm(mu.b2[site], tau.b2[site])
    beta3[site] ~ dnorm(mu.b3[site], tau.b3[site])
    
    sample.prob[site] ~ dbeta(5,5)
    lambda[site] ~ dgamma(1,1)
    
    for(t in 1:steps){
      # Components of transition matrix
      logit(survival1[t,site]) <- int1[site] + beta1[site]*coef2[t]
      logit(survival2[t,site]) <- int2[site] + beta2[site]*coef2[t]
      logit(transition[t,site]) <- int3[site] + beta3[site]*coef1[t]
      
      repro[t,site] ~ dpois(lambda[site])
      
      # Transition matrix
      A[1,1,t,site] <- survival1[t,site]*(1-transition[t,site])
      A[1,2,t,site] <- repro[t,site]
      A[2,1,t,site] <- survival1[t,site]*transition[t,site]
      A[2,2,t,site] <- survival2[t,site]
      
      # Sampling error
      for(stage in 1:2){
        for(sample in 1:4){
          y[stage,t,site,sample] ~ dbin(sample.prob[site], x[stage,t,site])
        }
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

# PP_RB part 2: across-site parameters using recursive Bayes
pprb.phase2 <- function(){
  # proposed variance
  for(b in 1:n.beta){
    q.start[b] ~ dgamma(q.pr[1,b], q.pr[2,b])
    r.start[b] ~ dgamma(r.pr[1,b], r.pr[2,b])
  
    q[b] <- sites/(2+q.start[b])
    r[b] <- 1/sum((b1-mu.b1)^2 + 1/r.start[b])
  }
  
  s2b1.temp ~ dgamma(q[1], r[1])
  b1.tau <- 1/s2b1.temp
  
  s2b2.temp ~ dgamma(q[2], r[2])
  b2.tau <- 1/s2b2.temp
  
  s2b3.temp ~ dgamma(q[3], r[3])
  b3.tau <- 1/s2b3.temp
    
  # proposed mu
  tmp.sd.b1 <- 1/((sites/s2b1.temp)+(1/b1.var.pr))
  tmp.mn.b1 <- tmp.sd.b1*((sum(b1)/s2b1.temp) + (b1.mu.pr/b1.var.pr))
  b1.mu ~ dnorm(tmp.mn.b1, 1/(tmp.sd.b1^2))
  
  tmp.sd.b2 <- 1/((sites/s2b2.temp)+(1/b2.var.pr))
  tmp.mn.b2 <- tmp.sd.b2*((sum(b2)/s2b2.temp) + (b2.mu.pr/b2.var.pr))
  b2.mu ~ dnorm(tmp.mn.b2, 1/(tmp.sd.b2^2))
  
  tmp.sd.b3 <- 1/((sites/s2b3.temp)+(1/b3.var.pr))
  tmp.mn.b3 <- tmp.sd.b3*((sum(b3)/s2b3.temp) + (b3.mu.pr/b3.var.pr))
  b3.mu ~ dnorm(tmp.mn.b3, 1/(tmp.sd.b3^2))
  
  for(site in 1:sites){
    b1[site] ~ dnorm(mu.b1[site], tau.b1[site])
    b2[site] ~ dnorm(mu.b2[site], tau.b2[site])
    b3[site] ~ dnorm(mu.b3[site], tau.b3[site])
  }
}

# Base model workflow ------------------
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

pr.x <- matrix(apply(ts, c(1,3), max), nrow = 2, ncol = sites)

data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
             int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
             b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
             b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
             coef1=var1, coef2=var2, steps=time.steps, y=samples, pr.x = pr.x,
             sites = sites)

params <- c("int1", "int2", "int3", "beta1", "beta2", "beta3", "lambda", 
            "mu.int1", "tau.int1", "mu.int2", "tau.int2", "mu.int3",
            "tau.int3", "mu.b1", "tau.b1", "mu.b2", "tau.b2", "mu.b3",
            "tau.b3", "x", "ex", "sample.prob", "pr.x")

inits <- function(){
  list(
    int1 = rep(0,sites),
    int2 = rep(0,sites),
    int3 = rep(0,sites),
    beta1 = rep(0,sites),
    beta2 = rep(0,sites),
    beta3 = rep(0,sites),
    lambda = rgamma(1,5,0.5),
    sample.prob = runif(1,0.5,1),
    x = abind(ceiling(apply(samples, c(1:3), max)*1.2),
              matrix(0, nrow = dim(samples)[1], ncol = sites),
              along = 2)
  )
}

mod <- jags(data=data, parameters.to.save = params, model.file = model.base,
            inits = inits, n.chains = 3, n.iter=10000)

# Base model: figures ------------------
# Time series
x <- mod$BUGSoutput$sims.list$x

x <- apply(x, 2:4, mean)

x.df <- as.data.frame(apply(x, 1, rbind)) %>%
  rename('stage1' = 'V1', 'stage2' = 'V2') %>%
  mutate(time = rep(1:(time.steps+1), sites),
         site = rep(paste("site", 1:sites, sep=""), each = time.steps+1)) %>%
  pivot_longer(stage1:stage2, names_to='life_stage', values_to='est_count')

ts.df <- as.data.frame(apply(ts, 1, rbind)) %>%
  rename('stage1' = 'V1', 'stage2' = 'V2') %>%
  mutate(time = rep(1:(time.steps), sites),
         site = rep(paste0("site", 1:sites), each = time.steps)) %>%
  pivot_longer(stage1:stage2, names_to='life_stage', values_to='count')

full.time.series <- full_join(x.df, ts.df, 
                              by = c('time', 'site', 'life_stage')) %>%
  pivot_longer(est_count:count, names_to = 'sample', values_to = 'count')

stage1.ts <- ggplot(data = filter(full.time.series, life_stage == 'stage1'), 
       aes(x = time, y = count, color = site, linetype = sample))+
  geom_line()+
  labs(x = "Time", y = "Count", title = "Stage 1")+
  scale_color_viridis_d(end = 0.8)+
  theme_bw()+
  theme(panel.grid = element_blank())

stage2.ts <- ggplot(data = filter(full.time.series, life_stage == 'stage2'), 
       aes(x = time, y = count, color = site, linetype = sample))+
  geom_line()+
  labs(x = "Time", y = "Count", title = "Stage 2")+
  scale_color_viridis_d(end = 0.8)+
  theme_bw()+
  theme(panel.grid = element_blank())

(stage1.ts | stage2.ts) +
  plot_layout(guides = 'collect')

# lambda and sample prob
prob.est <- mean(mod$BUGSoutput$sims.list$sample.prob)
lambda.est <- mean(mod$BUGSoutput$sims.list$lambda)

base.params <- data.frame(param = c('sample_prob', 'lambda'), 
                          estimate = c(prob.est, lambda.est))

write.table(base.params, "./ToyModel/base_params.csv")

# betas
beta1 <- as.data.frame(mod$BUGSoutput$sims.list$beta1) %>%
  rename('site1'='V1', 'site2'='V2', 'site3'='V3') %>%
  pivot_longer(cols = everything(), names_to = 'site', values_to = 'estimate') %>%
  group_by(site) %>%
  summarise(mean = mean(estimate), lower95 = quantile(estimate, 0.025),
            upper95 = quantile(estimate, 0.975)) %>%
  
  mutate(param = 'beta1')

beta2 <- as.data.frame(mod$BUGSoutput$sims.list$beta2) %>%
  rename('site1'='V1', 'site2'='V2', 'site3'='V3') %>%
  pivot_longer(cols = everything(), names_to = 'site', values_to = 'estimate') %>%
  group_by(site) %>%
  summarise(mean = mean(estimate), lower95 = quantile(estimate, 0.025),
            upper95 = quantile(estimate, 0.975)) %>%
  
  mutate(param = 'beta2')
 
beta3 <- as.data.frame(mod$BUGSoutput$sims.list$beta3) %>%
  rename('site1'='V1', 'site2'='V2', 'site3'='V3') %>%
  pivot_longer(cols = everything(), names_to = 'site', values_to = 'estimate') %>%
  group_by(site) %>%
  summarise(mean = mean(estimate), lower95 = quantile(estimate, 0.025),
            upper95 = quantile(estimate, 0.975)) %>%
  
  mutate(param = 'beta3') 

betas <- bind_rows(beta1, beta2, beta3)

tru.betas <- as.data.frame(rbind(stage1.beta, stage2.beta, transition.beta)) %>%
  rename_with(~paste0("site", 1:sites)) %>%
  mutate(param = c('beta1', 'beta2', 'beta3')) %>%
  pivot_longer(-param, names_to = 'site', values_to = 'val')

ggplot(betas, aes(x = mean, y = site))+
  geom_point(size = 1.5)+
  geom_errorbar(aes(xmin = lower95, xmax=upper95), linewidth = 1)+
  geom_point(data=tru.betas, aes(x = val, y = site), color = 'firebrick',
             size = 1.5)+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(~param) +
  labs(x = "Estimate", y = "Site")+
  theme_bw(base_size = 18)+
  theme(panel.grid = element_blank())

# iterative workflow -----------------------
iter.outs <- list()
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
    
    pr.x <- matrix(apply(ts, c(1,3), max), nrow = 2, ncol = sites)
    
    steps <- 2
    
    obs <- samples[,1:steps,,]
    
    data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
                 int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
                 b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
                 b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
                 coef1=var1, coef2=var2, steps=steps, y=obs, pr.x = pr.x,
                 sites=sites)
    params <- c("int1", "int2", "int3", "beta1", "beta2", "beta3", "lambda", 
                "mu.int1", "tau.int1", "mu.int2", "tau.int2", "mu.int3",
                "tau.int3", "mu.b1", "tau.b1", "mu.b2", "tau.b2", "mu.b3",
                "tau.b3", "x", "ex", "sample.prob", "pr.x")
    
    inits <- function(){
      list(
        int1 = rep(0,sites),
        int2 = rep(0,sites),
        int3 = rep(0,sites),
        beta1 = rep(0,sites),
        beta2 = rep(0,sites),
        beta3 = rep(0,sites),
        lambda = rgamma(1,5,0.5),
        sample.prob = runif(1,0.5,1),
        x = abind(ceiling(apply(samples[,1:steps,,], c(1:3), max)*1.2),
                  matrix(0, nrow = dim(samples)[1], ncol = sites),
                  along = 2)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = model.base,
                inits = inits, n.chains = 3, n.iter=10000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",1:sites, sep = "")
    colnames(outs$beta2) <- paste("site",1:sites, sep = "")
    colnames(outs$beta3) <- paste("site", 1:sites, sep = "")
    colnames(outs$int1) <- paste("site",1:sites, sep = "")
    colnames(outs$int2) <- paste("site",1:sites, sep = "")
    colnames(outs$int3) <- paste("site",1:sites, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:sites, sep = "")
    iter.outs[[i]] <- outs
    
    priors <- data.frame(int.mu1 = c(mean(outs$mu.int1), 1/var(outs$mu.int1)),
                   int.tau1 = fitdist(outs$tau.int1, 'gamma')$estimate,
                   int.mu2 = c(mean(outs$mu.int2), 1/var(outs$mu.int2)),
                   int.tau2 = fitdist(outs$tau.int2, 'gamma')$estimate,
                   int.mu3 = c(mean(outs$mu.int3), 1/var(outs$mu.int3)),
                   int.tau3 = fitdist(outs$tau.int3, 'gamma')$estimate,
                   b1.mu.pr = c(mean(outs$mu.b1), 1/var(outs$mu.b1)),
                   b1.tau.pr = fitdist(outs$tau.b1, 'gamma')$estimate,
                   b2.mu.pr = c(mean(outs$mu.b2), 1/var(outs$mu.b2)),
                   b2.tau.pr = fitdist(outs$tau.b2, 'gamma')$estimate,
                   b3.mu.pr = c(mean(outs$mu.b3), 1/var(outs$mu.b3)),
                   b3.tau.pr = fitdist(outs$tau.b3, 'gamma')$estimate)
    
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
    
    steps <- 2
    
    obs <- samples[,i:(i+1),,]
    
    data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
                 int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
                 b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
                 b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
                 coef1=var1[i:(i+1)], coef2=var2[i:(i+1)], steps=steps, y=obs, pr.x = pr.x,
                 sites=sites)
    params <- c("int1", "int2", "int3", "beta1", "beta2", "beta3", "lambda", 
                "mu.int1", "tau.int1", "mu.int2", "tau.int2", "mu.int3",
                "tau.int3", "mu.b1", "tau.b1", "mu.b2", "tau.b2", "mu.b3",
                "tau.b3", "x", "ex", "sample.prob")
    
    inits <- function(){
      list(
        int1 = rep(0,sites),
        int2 = rep(0,sites),
        int3 = rep(0,sites),
        beta1 = rep(0,sites),
        beta2 = rep(0,sites),
        beta3 = rep(0,sites),
        lambda = rgamma(1,5,0.5),
        sample.prob = runif(1,0.5,1),
        x = abind(ceiling(apply(samples[,i:(i+1),,], c(1:3), max)*1.2),
                  matrix(0, nrow = dim(samples)[1], ncol = sites),
                  along = 2)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = model.base,
                inits=inits, n.chains = 3, n.iter=10000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",1:sites, sep = "")
    colnames(outs$beta2) <- paste("site",1:sites, sep = "")
    colnames(outs$beta3) <- paste("site", 1:sites, sep = "")
    colnames(outs$int1) <- paste("site",1:sites, sep = "")
    colnames(outs$int2) <- paste("site",1:sites, sep = "")
    colnames(outs$int3) <- paste("site",1:sites, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:sites, sep = "")
    iter.outs[[i]] <- outs
    
    priors <- data.frame(int.mu1 = c(mean(outs$mu.int1), 1/var(outs$mu.int1)),
                         int.tau1 = fitdist(outs$tau.int1, 'gamma')$estimate,
                         int.mu2 = c(mean(outs$mu.int2), 1/var(outs$mu.int2)),
                         int.tau2 = fitdist(outs$tau.int2, 'gamma')$estimate,
                         int.mu3 = c(mean(outs$mu.int3), 1/var(outs$mu.int3)),
                         int.tau3 = fitdist(outs$tau.int3, 'gamma')$estimate,
                         b1.mu.pr = c(mean(outs$mu.b1), 1/var(outs$mu.b1)),
                         b1.tau.pr = fitdist(outs$tau.b1, 'gamma')$estimate,
                         b2.mu.pr = c(mean(outs$mu.b2), 1/var(outs$mu.b2)),
                         b2.tau.pr = fitdist(outs$tau.b2, 'gamma')$estimate,
                         b3.mu.pr = c(mean(outs$mu.b3), 1/var(outs$mu.b3)),
                         b3.tau.pr = fitdist(outs$tau.b3, 'gamma')$estimate)
    
    pr.x <- apply(outs$x, c(2,4), median)
  }
}

# Iterative figures -----------------------
# Tau over time
b2.tau <- list()
for(i in 1:length(iter.outs)){
  b2.tau[[i]] <- as.data.frame(iter.outs[[i]]$tau.b2) %>%
    mutate(time = i, param = 'tau.b2')
}

b2tau.df <- do.call(bind_rows, b2.tau)

ggplot(data = b2tau.df, aes(x = factor(time), y = V1))+
  geom_boxplot(fill = 'lightgray')+
  labs(x = "Iter", y = "B2 tau")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank())

# Betas
iter.betas <- tibble()
for(i in 1:length(iter.outs)){
  b1.raw <- as.data.frame(iter.outs[[i]]$beta1) %>%
    mutate(time = i, param = 'beta1') %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')

  b2.raw <- as.data.frame(iter.outs[[i]]$beta2) %>%
    mutate(time = i, param = 'beta2') %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')
 
  b3.raw <- as.data.frame(iter.outs[[i]]$beta3) %>%
    mutate(time = i, param = 'beta3') %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')

  betas <- bind_rows(b1.raw, b2.raw, b3.raw) %>%
    group_by(time, site, param) %>%
    summarise(mean = mean(estimate), median=median(estimate), 
              lower95 = quantile(estimate, 0.025),
              upper95 = quantile(estimate, 0.975),
              var=var(estimate,na.rm = T)) %>%
    suppressMessages()
  
  iter.betas <- bind_rows(iter.betas, betas)
}

all.betas <- iter.betas %>%
  ungroup() %>%
  group_by(site, param) %>%
  summarise(mean = median(mean), lower95=median(lower95), upper95=median(upper95),
            var = median(var)) %>%
  suppressMessages()

ggplot(data = all.betas, aes(x = mean, y = site))+
  geom_point()+
  geom_errorbar(aes(xmin = lower95, xmax = upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_point(data = tru.betas, aes(x = val, y = site), color = 'firebrick')+
  facet_wrap(~param)+
  labs(x = "Estimate")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank(), axis.title.y = element_blank())

ggplot(data = iter.betas, aes(x = time, y = mean, color = site))+
  geom_line()+
  # geom_hline(data = tru.betas, aes(yintercept = val, color = site))+
  facet_wrap(~param)+
  scale_color_viridis_d(end = 0.8)+
  labs(x = "Iter", y = "Estimate")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank())

last.betas <- iter.betas %>%
  filter(time > 5) %>%
  ungroup() %>%
  group_by(site, param) %>%
  summarise(mean = median(mean), lower95=median(lower95), upper95=median(upper95),
            var = median(var)) %>%
  suppressMessages()

ggplot(data = last.betas, aes(x = mean, y = site))+
  geom_point()+
  geom_errorbar(aes(xmin = lower95, xmax = upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_point(data = tru.betas, aes(x = val, y = site), color = 'firebrick')+
  facet_wrap(~param)+
  labs(x = "Estimate")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank(), axis.title.y = element_blank())

# Iterative model workflow: recursive Bayes -------------
rb.outs <- list()
rb.outs.comm <- list()
for(i in 1:time.steps){
  if(i == 1){
    # Part I: Site-level models
    mu.int1 <- rep(0,sites)
    tau.int1 <- rep(1, sites)
    
    mu.int2 <- rep(0,sites)
    tau.int2 <- rep(1, sites)
    
    mu.int3 <- rep(0,sites)
    tau.int3 <- rep(1, sites)
    
    mu.b1 <- rep(0,sites)
    tau.b1 <- rep(1,sites)
    
    mu.b2 <- rep(0,sites)
    tau.b2 <- rep(1,sites)
    
    mu.b3 <- rep(0,sites)
    tau.b3 <- rep(1,sites)
    
    pr.x <- matrix(apply(ts, c(1,3), max), nrow = 2, ncol = sites)
    
    steps <- 2
    
    obs <- samples[,1:steps,,]
    
    data <- list(mu.int1=mu.int1, tau.int1=tau.int1, mu.int2=mu.int2, 
                 tau.int2=tau.int2, mu.int3=mu.int3, tau.int3=tau.int3,
                 mu.b1=mu.b1, tau.b1=tau.b1, mu.b2=mu.b2, tau.b2=tau.b2, 
                 mu.b3=mu.b3, tau.b3=tau.b3, coef1=var1, coef2=var2, 
                 steps=steps, y=obs, pr.x = pr.x, sites=sites)
    params <- c("int1", "int2", "int3", "beta1", "beta2", "beta3", "lambda",
                "x", "ex", "sample.prob", "pr.x")
    
    inits <- function(){
      list(
        int1 = rep(0,sites),
        int2 = rep(0,sites),
        int3 = rep(0,sites),
        beta1 = rep(0,sites),
        beta2 = rep(0,sites),
        beta3 = rep(0,sites),
        lambda = rgamma(sites,5,0.5),
        sample.prob = runif(sites,0.5,1),
        x = abind(ceiling(apply(samples[,1:steps,,], c(1:3), max)*1.2),
                  matrix(0, nrow = dim(samples)[1], ncol = sites),
                  along = 2)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = pprb.phase1,
                inits = inits, n.chains = 3, n.iter=10000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",1:sites, sep = "")
    colnames(outs$beta2) <- paste("site",1:sites, sep = "")
    colnames(outs$beta3) <- paste("site", 1:sites, sep = "")
    colnames(outs$int1) <- paste("site",1:sites, sep = "")
    colnames(outs$int2) <- paste("site",1:sites, sep = "")
    colnames(outs$int3) <- paste("site",1:sites, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:sites, sep = "")
    rb.outs[[i]] <- outs
    
    priors <- data.frame(mu.int1 = colMeans(outs$int1),
                         tau.int1 = apply(outs$int1, 2, function(x) 1/var(x)),
                         mu.int2 = colMeans(outs$int2),
                         tau.int2 = apply(outs$int2, 2, function(x) 1/var(x)),
                         mu.int3 = colMeans(outs$int3),
                         tau.int3 = apply(outs$int3, 2, function(x) 1/var(x)),
                         mu.b1 = colMeans(outs$beta1),
                         tau.b1 = apply(outs$beta1, 2, function(x) 1/var(x)),
                         mu.b2 = colMeans(outs$beta2),
                         tau.b2 = apply(outs$beta2, 2, function(x) 1/var(x)),
                         mu.b3 = colMeans(outs$beta3),
                         tau.b3 = apply(outs$beta3, 2, function(x) 1/var(x)))
    
    pr.x <- apply(outs$x, c(2,4), median)
    
    # Part II: Recursive Bayesian updating of site-level coefs
    q.pr <- matrix(1, nrow=2, ncol=3)
    r.pr <- matrix(1, nrow=2, ncol=3)

    b1.mu.pr <- 0
    b1.var.pr <- 1
    
    b2.mu.pr <- 0
    b2.var.pr <- 1
    
    b3.mu.pr <- 0
    b3.var.pr <- 1

    data.rb <- list(q.pr=q.pr, r.pr=r.pr, b1.mu.pr=b1.mu.pr, b1.var.pr=b1.var.pr,
                    mu.b1 = priors$mu.b1, tau.b1 = priors$tau.b1, 
                    b2.mu.pr=b2.mu.pr, b2.var.pr=b2.var.pr, mu.b2=priors$mu.b2,
                    tau.b2=priors$tau.b2, b3.mu.pr=b3.mu.pr, b3.var.pr=b3.var.pr,
                    mu.b3=priors$mu.b3, tau.b3=priors$tau.b3, sites=sites,
                    n.beta = 3)
    params.rb <- c("q", "r", "b1.mu", "b1.tau", "b1", "b2.mu", "b2.tau", "b2",
                   "b3.mu", "b3.tau", "b3")

    mod.rb <- jags(data=data.rb, parameters.to.save = params.rb,
                   model.file = pprb.phase2, n.chains = 3, n.iter=5000,
                   DIC = F)

    outs.update <- mod.rb$BUGSoutput$sims.list
    rb.outs.comm[[i]] <- outs.update

    priors$mu.b1 <- colMeans(outs.update$b1)
    priors$tau.b1 <- apply(outs.update$b1, 2, function(x) 1/var(x))
    
    priors$mu.b2 <- colMeans(outs.update$b2)
    priors$tau.b2 <- apply(outs.update$b2, 2, function(x) 1/var(x))
    
    priors$mu.b3 <- colMeans(outs.update$b3)
    priors$tau.b3 <- apply(outs.update$b3, 2, function(x) 1/var(x))

    q.pr <- apply(outs.update$q, 2, 
                  function(x) fitdist(x, distr='gamma')$estimate)
    r.pr <- apply(outs.update$r, 2, 
                  function(x) fitdist(x, distr='gamma')$estimate)

    b1.mu.pr <- mean(outs.update$b1.mu)
    b1.var.pr <- 1/(mean(outs.update$b1.tau))
    
    b2.mu.pr <- mean(outs.update$b2.mu)
    b2.var.pr <- 1/(mean(outs.update$b2.tau))
    
    b3.mu.pr <- mean(outs.update$b3.mu)
    b3.var.pr <- 1/(mean(outs.update$b3.tau))

  } else{
    # Part I: Site-level models
    mu.int1 <- priors$mu.int1
    tau.int1 <- priors$tau.int1
    
    mu.int2 <- priors$mu.int2
    tau.int2 <- priors$tau.int2
    
    mu.int3 <- priors$mu.int3
    tau.int3 <- priors$tau.int3
    
    mu.b1 <- priors$mu.b1
    tau.b1 <- priors$tau.b1
    
    mu.b2 <- priors$mu.b2
    tau.b2 <- priors$tau.b2
    
    mu.b3 <- priors$mu.b3
    tau.b3 <- priors$tau.b3
    
    pr.x <- pr.x
    
    steps <- 2
    
    obs <- samples[,i:(i+1),,]
    
    data <- list(mu.int1=mu.int1, tau.int1=tau.int1, mu.int2=mu.int2, 
                 tau.int2=tau.int2, mu.int3=mu.int3, tau.int3=tau.int3,
                 mu.b1=mu.b1, tau.b1=tau.b1, mu.b2=mu.b2, tau.b2=tau.b2, 
                 mu.b3=mu.b3, tau.b3=tau.b3, coef1=var1[i:(i+1)], coef2=var2[i:(i+1)], 
                 steps=steps, y=obs, pr.x = pr.x, sites=sites)
    params <- c("int1", "int2", "int3", "beta1", "beta2", "beta3", "lambda",
                "x", "ex", "sample.prob", "pr.x")
    
    inits <- function(){
      list(
        int1 = rep(0,sites),
        int2 = rep(0,sites),
        int3 = rep(0,sites),
        beta1 = rep(0,sites),
        beta2 = rep(0,sites),
        beta3 = rep(0,sites),
        lambda = rgamma(sites,5,0.5),
        sample.prob = runif(sites,0.5,1),
        x = abind(ceiling(apply(samples[,i:(i+1),,], c(1:3), max)*1.2),
                  matrix(0, nrow = dim(samples)[1], ncol = sites),
                  along = 2)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = pprb.phase1,
                inits = inits, n.chains = 3, n.iter=10000)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",1:sites, sep = "")
    colnames(outs$beta2) <- paste("site",1:sites, sep = "")
    colnames(outs$beta3) <- paste("site", 1:sites, sep = "")
    colnames(outs$int1) <- paste("site",1:sites, sep = "")
    colnames(outs$int2) <- paste("site",1:sites, sep = "")
    colnames(outs$int3) <- paste("site",1:sites, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:sites, sep = "")
    rb.outs[[i]] <- outs
    
    priors <- data.frame(mu.int1 = colMeans(outs$int1),
                         tau.int1 = apply(outs$int1, 2, function(x) 1/var(x)),
                         mu.int2 = colMeans(outs$int2),
                         tau.int2 = apply(outs$int2, 2, function(x) 1/var(x)),
                         mu.int3 = colMeans(outs$int3),
                         tau.int3 = apply(outs$int3, 2, function(x) 1/var(x)),
                         mu.b1 = colMeans(outs$beta1),
                         tau.b1 = apply(outs$beta1, 2, function(x) 1/var(x)),
                         mu.b2 = colMeans(outs$beta2),
                         tau.b2 = apply(outs$beta2, 2, function(x) 1/var(x)),
                         mu.b3 = colMeans(outs$beta3),
                         tau.b3 = apply(outs$beta3, 2, function(x) 1/var(x)))
    
    pr.x <- apply(outs$x, c(2,4), median)
    
    # Part II: Recursive Bayesian updating of site-level coefs
    q.pr <- q.pr
    r.pr <- r.pr
    
    b1.mu.pr <- b1.mu.pr
    b1.var.pr <- b1.var.pr
    
    b2.mu.pr <- b2.mu.pr
    b2.var.pr <- b2.var.pr
    
    b3.mu.pr <- b3.mu.pr
    b3.var.pr <- b3.var.pr
    
    data.rb <- list(q.pr=q.pr, r.pr=r.pr, b1.mu.pr=b1.mu.pr, b1.var.pr=b1.var.pr,
                    mu.b1 = priors$mu.b1, tau.b1 = priors$tau.b1, 
                    b2.mu.pr=b2.mu.pr, b2.var.pr=b2.var.pr, mu.b2=priors$mu.b2,
                    tau.b2=priors$tau.b2, b3.mu.pr=b3.mu.pr, b3.var.pr=b3.var.pr,
                    mu.b3=priors$mu.b3, tau.b3=priors$tau.b3, sites=sites,
                    n.beta = 3)
    params.rb <- c("q", "r", "b1.mu", "b1.tau", "b1", "b2.mu", "b2.tau", "b2",
                   "b3.mu", "b3.tau", "b3")
    
    mod.rb <- jags(data=data.rb, parameters.to.save = params.rb,
                   model.file = pprb.phase2, n.chains = 3, n.iter=5000,
                   DIC = F)
    
    outs.update <- mod.rb$BUGSoutput$sims.list
    rb.outs.comm[[i]] <- outs.update
    
    priors$mu.b1 <- colMeans(outs.update$b1)
    priors$tau.b1 <- apply(outs.update$b1, 2, function(x) 1/var(x))
    
    priors$mu.b2 <- colMeans(outs.update$b2)
    priors$tau.b2 <- apply(outs.update$b2, 2, function(x) 1/var(x))
    
    priors$mu.b3 <- colMeans(outs.update$b3)
    priors$tau.b3 <- apply(outs.update$b3, 2, function(x) 1/var(x))
    
    q.pr <- apply(outs.update$q, 2, 
                  function(x) fitdist(x, distr='gamma')$estimate)
    r.pr <- apply(outs.update$r, 2, 
                  function(x) fitdist(x, distr='gamma')$estimate)
    
    b1.mu.pr <- mean(outs.update$b1.mu)
    b1.var.pr <- 1/(mean(outs.update$b1.tau))
    
    b2.mu.pr <- mean(outs.update$b2.mu)
    b2.var.pr <- 1/(mean(outs.update$b2.tau))
    
    b3.mu.pr <- mean(outs.update$b3.mu)
    b3.var.pr <- 1/(mean(outs.update$b3.tau))
  }
}

# Recursive Bayes: Figures -------------------------
# Betas after phase 2
rb.betas <- tibble()
for(i in 1:length(rb.outs.comm)){
  b1.raw <- as.data.frame(rb.outs.comm[[i]]$b1) %>%
    mutate(time = i, param = 'beta1') %>%
    rename_with(~paste0('site', 1:sites), V1:V10) %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')
  
  b2.raw <- as.data.frame(rb.outs[[i]]$beta2) %>%
    mutate(time = i, param = 'beta2') %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')
  
  b3.raw <- as.data.frame(rb.outs[[i]]$beta3) %>%
    mutate(time = i, param = 'beta3') %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')
  
  betas <- bind_rows(b1.raw, b2.raw, b3.raw) %>%
    group_by(time, site, param) %>%
    summarise(mean = mean(estimate), median=median(estimate), 
              lower95 = quantile(estimate, 0.025),
              upper95 = quantile(estimate, 0.975),
              var=var(estimate,na.rm = T)) %>%
    suppressMessages()
  
  rb.betas <- bind_rows(rb.betas, betas)
}

all.betas <- rb.betas %>%
  ungroup() %>%
  group_by(site, param) %>%
  summarise(mean = median(mean), lower95=median(lower95), upper95=median(upper95),
            var = median(var)) %>%
  suppressMessages()

tru.betas <- as.data.frame(rbind(stage1.beta, stage2.beta, transition.beta)) %>%
  rename_with(~paste0("site", 1:sites)) %>%
  mutate(param = c('beta1', 'beta2', 'beta3')) %>%
  pivot_longer(-param, names_to = 'site', values_to = 'val')

ggplot(data = all.betas, aes(x = mean, y = site))+
  geom_point()+
  geom_errorbar(aes(xmin = lower95, xmax = upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_point(data = tru.betas, aes(x = val, y = site), color = 'firebrick')+
  facet_wrap(~param)+
  labs(x = "Estimate")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank(), axis.title.y = element_blank())

last.betas <- rb.betas %>%
  filter(time > 10) %>%
  ungroup() %>%
  group_by(site, param) %>%
  summarise(mean = median(mean), lower95=median(lower95), upper95=median(upper95),
            var = median(var)) %>%
  suppressMessages()

ggplot(data = last.betas, aes(x = mean, y = site))+
  geom_point()+
  geom_errorbar(aes(xmin = lower95, xmax = upper95))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_point(data = tru.betas, aes(x = val, y = site), color = 'firebrick')+
  facet_wrap(~param)+
  labs(x = "Estimate")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank(), axis.title.y = element_blank())

ggplot(data = rb.betas, aes(x = time, y = mean, color = site))+
  geom_line()+
  # geom_hline(data = tru.betas, aes(yintercept = val, color = site))+
  facet_wrap(~param)+
  scale_color_viridis_d(end = 0.8)+
  labs(x = "Iter", y = "Estimate")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank())

# Compare to phase 1 betas
rb.og <- tibble()
for(i in 1:length(rb.outs)){
  b1.raw <- as.data.frame(rb.outs[[i]]$beta1) %>%
    mutate(time = i, param = 'beta1') %>%
    # rename_with(~paste0('site', 1:sites), V1:V10) %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')
  
  b2.raw <- as.data.frame(rb.outs[[i]]$beta2) %>%
    mutate(time = i, param = 'beta2') %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')
  
  b3.raw <- as.data.frame(rb.outs[[i]]$beta3) %>%
    mutate(time = i, param = 'beta3') %>%
    pivot_longer(cols = site1:site10, names_to = 'site', values_to = 'estimate')
  
  betas <- bind_rows(b1.raw, b2.raw, b3.raw) %>%
    group_by(time, site, param) %>%
    summarise(mean = mean(estimate), median=median(estimate), 
              lower95 = quantile(estimate, 0.025),
              upper95 = quantile(estimate, 0.975),
              var=var(estimate,na.rm = T)) %>%
    suppressMessages()
  
  rb.og <- bind_rows(rb.og, betas)
}

rb.betas$phase <- 2
rb.og$phase <- 1

all.rb <- bind_rows(rb.betas, rb.og) %>%
  filter(param == "beta1") %>%
  group_by(time, site, phase) %>%
  summarise(mean = mean(mean), lower95=mean(lower95), upper95=mean(upper95))

ggplot(data = all.rb, aes(x = time, y = mean))+
  geom_point(aes(color = factor(phase)))+
  geom_errorbar(aes(ymin = lower95, ymax = upper95, color = factor(phase)))+
  facet_wrap(~site)
a