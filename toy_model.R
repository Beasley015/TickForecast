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

set.seed(10)
time.steps <- 20
var.seq <- seq(from = -2, to = 2, length.out = time.steps+1)

# Generate environmental variables ---------------
var1 <- var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)
var2 <-  (var.seq)^2 + var.seq + rnorm(time.steps+1, mean = 0, sd = 0.3)

# Coefficients -----------------
stage1.beta <- c(-2,0,2)
stage2.beta <- sample(c(-2, 0, 2), size = 3, replace = F)
transition.beta <- sample(c(-2,0,2), size = 3, replace = F)

# Survival/transition probs ---------------
get.probs <- function(param, variable, intercept){
  prob <- matrix(NA, nrow = time.steps+1, ncol = 3)
  for(i in 1:3){
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
A <- array(0, dim = c(2,2,time.steps+1, 3))

A[1,1,,] <- stage1*(1-transition)
A[1,2,,] <- repro
A[2,1,,] <- stage1*transition
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
# base model (non-Cauchy priors)
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
    for(site in 1:3){
      x[stage,1,site] ~ dpois(pr.x[stage,site])
    }
  }
  
  for(site in 1:3){
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

# iterative w/Cauchy priors

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

pr.x <- matrix(apply(ts, c(1,3), max), nrow = 2, ncol = 3)

data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
             int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
             b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
             b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
             coef1=var1, coef2=var2, steps=time.steps, y=samples, pr.x = pr.x)

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
    lambda = rgamma(1,5,0.5),
    sample.prob = runif(1,0.5,1),
    x = abind(ceiling(apply(samples, c(1:3), max)*1.2),
              matrix(0, nrow = dim(samples)[1], ncol = 3),
              along = 2)
  )
}

mod <- jags(data=data, parameters.to.save = params, model.file = model.base,
            inits = inits, n.chains = 3, n.iter=7500)

# Base model: figures ------------------
# Time series
x <- mod$BUGSoutput$sims.list$x

x <- apply(x, 2:4, mean)

x.df <- as.data.frame(apply(x, 1, rbind)) %>%
  rename('stage1' = 'V1', 'stage2' = 'V2') %>%
  mutate(time = rep(1:(time.steps+1), 3),
         site = rep(c('site1', 'site2', 'site3'), each = time.steps+1)) %>%
  pivot_longer(stage1:stage2, names_to='life_stage', values_to='est_count')

ts.df <- as.data.frame(apply(ts, 1, rbind)) %>%
  rename('stage1' = 'V1', 'stage2' = 'V2') %>%
  mutate(time = rep(1:(time.steps), 3),
         site = rep(c('site1', 'site2', 'site3'), each = time.steps)) %>%
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
  rename('site1'='V1', 'site2'='V2', 'site3'='V3') %>%
  mutate(param = c('beta1', 'beta2', 'beta3')) %>%
  pivot_longer(-param, names_to = 'site', values_to = 'val')

ggplot(betas, aes(x = mean, y = site))+
  geom_point(size = 1.5)+
  geom_errorbar(aes(xmin = lower95, xmax=upper95), size = 1)+
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
    
    pr.x <- matrix(apply(ts, c(1,3), max), nrow = 2, ncol = 3)
    
    steps <- 2
    
    obs <- samples[,1:steps,,]
    
    data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
                 int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
                 b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
                 b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
                 coef1=var1, coef2=var2, steps=steps, y=obs, pr.x = pr.x)
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
        lambda = rgamma(1,5,0.5),
        sample.prob = runif(1,0.5,1),
        x = abind(ceiling(apply(samples[,1:steps,,], c(1:3), max)*1.2),
                  matrix(0, nrow = dim(samples)[1], ncol = 3),
                  along = 2)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = model.base,
                inits = inits, n.chains = 3, n.iter=7500)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",1:3, sep = "")
    colnames(outs$beta2) <- paste("site",1:3, sep = "")
    colnames(outs$beta3) <- paste("site", 1:3, sep = "")
    colnames(outs$int1) <- paste("site",1:3, sep = "")
    colnames(outs$int2) <- paste("site",1:3, sep = "")
    colnames(outs$int3) <- paste("site",1:3, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:3, sep = "")
    iter.outs[[i]] <- outs
    
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
    
    steps <- ifelse(time.steps+1- i < 2, time.steps+1 - i, 2)
    
    obs <- samples[,i:(i+1),,]
    
    data <- list(int.mu1=int.mu1, int.tau1=int.tau1, int.mu2=int.mu2, 
                 int.tau2=int.tau2, int.mu3=int.mu3, int.tau3=int.tau3,
                 b1.mu.pr=b1.mu.pr,b1.tau.pr=b1.tau.pr, b2.mu.pr=b2.mu.pr, 
                 b2.tau.pr=b2.tau.pr, b3.mu.pr=b3.mu.pr, b3.tau.pr=b3.tau.pr,
                 coef1=var1, coef2=var2, steps=steps, y=obs, pr.x = pr.x)
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
        lambda = rgamma(1,5,0.5),
        sample.prob = runif(1,0.5,1),
        x = abind(ceiling(apply(samples[,i:(i+1),,], c(1:3), max)*1.2),
                  matrix(0, nrow = dim(samples)[1], ncol = 3),
                  along = 2)
      )
    }
    
    mod <- jags(data=data, parameters.to.save = params, model.file = model.base,
                inits=inits, n.chains = 3, n.iter=7500)
    
    outs <- mod$BUGSoutput$sims.list
    colnames(outs$beta1) <- paste("site",1:3, sep = "")
    colnames(outs$beta2) <- paste("site",1:3, sep = "")
    colnames(outs$beta3) <- paste("site", 1:3, sep = "")
    colnames(outs$int1) <- paste("site",1:3, sep = "")
    colnames(outs$int2) <- paste("site",1:3, sep = "")
    colnames(outs$int3) <- paste("site",1:3, sep = "")
    dimnames(outs$x)[[4]] <- paste("site",1:3, sep = "")
    iter.outs[[i]] <- outs
    
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

# Iterative figures -----------------------
# Time series

# Params

# Betas
iter.betas <- tibble()
for(i in 1:length(iter.outs)){
  b1.raw <- as.data.frame(iter.outs[[i]]$beta1) %>%
    mutate(time = i, param = 'beta1') %>%
    pivot_longer(cols = site1:site3, names_to = 'site', values_to = 'estimate')

  b2.raw <- as.data.frame(iter.outs[[i]]$beta2) %>%
    mutate(time = i, param = 'beta2') %>%
    pivot_longer(cols = site1:site3, names_to = 'site', values_to = 'estimate')
 
  b3.raw <- as.data.frame(iter.outs[[i]]$beta3) %>%
    mutate(time = i, param = 'beta3') %>%
    pivot_longer(cols = site1:site3, names_to = 'site', values_to = 'estimate')

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
  facet_wrap(~param)
  
ggplot(data = iter.betas, aes(x = time, y = mean, color = site))+
  geom_line()+
  facet_wrap(~param)
