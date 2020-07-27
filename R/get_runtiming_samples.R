#' Estimate parameters of hierarchical logistic regression using MCMC routine.
#' @description The sonar species apportionment model uses species priors that are weakly informed by a logistic curve. To estimate these parameters, a hierarchical logistic regression model is used where the non-expanded visual counts for each day are used as input data. Regression coefficients are treated as hierarchically distributed.  Since the timing of the chum and king runs for the Chena and Salcha rivers tend to be similar each year, the coefficient pairs (intercepts for both rivers, slopes for both rivers) are assumed to be MVN, allowing the inclusion of a correlation parameter. If one river's sonar season is compromised, this allows the borrowing of information from the other river.  
#' @param runtiming_data Output of \code{\link{prepare_runtiming_data}}.
#' @param niter The number of MCMC iterations. 100k will take roughly 6 minutes whereas 500k will take 30 minutes. 
#' @param ncores The number of cores for parallel chains.
#' @author Matt Tyres and Jordy Bernard.
#' @export

get_runtiming_samples <- function(runtiming_data, file_dir, niter=100000, ncores=3){
  cat('model {
  for(i in 1:Cn) {
    logit(Cpi[i]) <- a0[Cyear[i],1] + a1[Cyear[i],1]*Cday[i]
    Cchin[i] ~ dbin(Cpi[i],Ctot[i])
  }
  for(i in 1:Sn) {
    logit(Spi[i]) <- a0[Syear[i],2] + a1[Syear[i],2]*Sday[i]
    Schin[i] ~ dbin(Spi[i],Stot[i])
  }
  for(i in 1:nyear) {
    a0[i,1:2] ~ dmnorm(b0[],tau0[,])
    a1[i,1:2] ~ dmnorm(b1[],tau1[,])
  }
  tau0[1:2,1:2] <- inverse(Sigma0[,])
  tau1[1:2,1:2] <- inverse(Sigma1[,])
  Sigma0[1,1] <- pow(sig01,2)
  Sigma0[2,2] <- pow(sig02,2)
  Sigma0[1,2] <- rho0*sig01*sig02
  Sigma0[2,1] <- Sigma0[1,2]
  sig01 ~ dunif(0,10)   # was 1000
  sig02 ~ dunif(0,10)   # was 1000
  rho0 ~ dunif(-1,1)
  Sigma1[1,1] <- pow(sig11,2)
  Sigma1[2,2] <- pow(sig12,2)
  Sigma1[1,2] <- rho1*sig11*sig12
  Sigma1[2,1] <- Sigma1[1,2]
  sig11 ~ dunif(0,10)
  sig12 ~ dunif(0,10)
  rho1 ~ dunif(-1,1)
  b0[1] ~ dnorm(0,0.001)
  b1[1] ~ dnorm(0,0.001)
  b0[2] ~ dnorm(0,0.001)
  b1[2] ~ dnorm(0,0.001)
  }', file=file_dir)
  tstart <- Sys.time()
  CS.jags.out <- jagsUI::jags(model.file=file_dir, data=runtiming_data, parameters.to.save=c("b0","b1","a0","a1","rho0","rho1"),
                              n.chains=ncores, parallel=T, n.iter=niter, n.thin=niter/2000, n.burnin=niter/2)
  print(paste("Time to run model:", round(Sys.time()-tstart,2), "minutes"))
  return(CS.jags.out)
}
