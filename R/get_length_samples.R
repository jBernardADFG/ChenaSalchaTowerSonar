#' Estimate mean, sd, and se of carcass lengths.
#' @description Runs Hierarchical Bayesian model to estimate mean, sd, and se of carcass lengths and returns posterior samples. This information is used to inform priors in the mixture model (see \code{\link{run_mixture_model}}).
#' @param carcass_data The output of \code{\link{prepare_length_data}}.
#' @param ncores The number of cores for parallel chains.
#' @param niter The number of MCMC iterations. 10k will take roughly 8 minutes whereas 50k will take 35 minutes. 
#' @author Matt Tyres and Jordy Bernard.
#' @export

get_length_samples <- function(carcass_data, file_dir, ncores=3, niter=10000){
  
  print("Be patient -- this could take a while")
  tstart <- Sys.time()
  cat('model {
  for (i in 1:N) {
    y[i] ~ dnorm(mu[cat[i],year[i]], tau[cat[i]])
  }
  for(j in 1:Ncat) {
    for(k in 1:Nyear) {
      mu[j,k] ~ dnorm(mumu[j], taumu[j])
    }
    mumu[j] ~ dnorm(500,0.0001)
    sigmu[j] ~ dunif(0,100)
    taumu[j] <- pow(sigmu[j], -2)
    tau[j] <- pow(sigma[j], -2)
    sigma[j] ~ dunif(0,500)
  }
  catmean[1:Ncat] <- mu[1:Ncat,Nyear]
  }', file=file_dir)
  length.jags.out <- jagsUI::jags(model.file=file_dir, data=carcass_data,
                                  parameters.to.save=c("mu","mumu","sigmu","sigma","catmean"), 
                                  n.chains=ncores, parallel=T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000)
  print(paste("Time to run model:", round(Sys.time()-tstart,2), "minutes"))
  return(length.jags.out)
}



