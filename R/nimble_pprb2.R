library(nimble)

model.code <- nimbleCode({
  # Draw coefficients from posterior of phase I
  for(site in 1:sites){
    for(b in 1:n.coef){
      coef[site,b] ~ dnorm(mu.coef[site,n], tau.coef[site,n])
    }
  }
  
  for(b in 1:n.coef){
    # proposed variance
    q.start[b] ~ dgamma(q.pr[1,b], q.pr[2,b])
    r.start[b] ~ dgamma(r.pr[1,b], r.pr[2,b])
    
    q[b] <- sites/(2+q.start[b])
    r[b] <- 1/sum((coef[,b]-mu.coef[,b])^2 + 1/r.start[b])
    
    s2.temp[b] ~ dgamma(q[b], r[b])
    coef.tau[b] <- 1/s2.temp[b]
    
    # proposed mu
    tmp.sd[b] <- 1/((sites/s2.temp[b])+(1/var.pr[b]))
    tmp.mn[b] <- tmp.sd[b]*((sum(coef[,b])/s2.temp[b]) + (mu.pr[b]/var.pr[b]))
    coef.mu[b] <- dnorm(tmp.mn[b], sd=1/(tmp.sd[b]^2))
  }
})