#' Use sonar data to apportion and enumerate fish passage by species.
#' @description The function runs the big mixture model and outputs the posterior samples.
#' @param mixture_data Output of \code{\link{gather_mixture_data}}.
#' @param niter The number of MCMC iterations. 50k takes roughly 2.5 hours, 200k takes 9 hours, 500k takes 25ish. 
#' @param ncores The number of cores for parallel chains.
#' @author Jordy Bernard.
#' @export

run_mixture_model_2 <- function(mixture_data, file_dir, niter = 100000, ncores = 4){
  
  mixture_data
  file_dir <- "Jags/mixture2.jags"
  niter=1000
  ncores=4
  
  cat('model {
    
    # Tethered Fish Experiment #
    for (i in 1:length(L_sonar_tether)){
      L_sonar_tether[i] ~ dnorm(beta_0 + beta_1*L_act_tether[i], tau_s)T(0,)
    }
    beta_0 ~ dexp(0.0001) # Might need to change #
    beta_1 ~ dexp(0.0001) # Might need to change #
    tau_s <- pow(sig_s, -2)
    sig_s ~ dexp(0.0001)
    
    # Mixture Component #
    for (i in 1:length(L_sonar)){
      L_sonar[i] ~ dnorm(beta_0 + beta_1*L_act[i], tau_s)T(0,)
      L_act[i] ~ dnorm(mu[species[i]+1], tau[species[i]+1])T(0,)
      species[i] ~ dbern(pi[i])
      pi[i] ~ dbeta(0.5, 0.5)
    }
    
    # Historic Lengths #
    for (i in 1:length(L_hist_chin)){
      L_hist_chin[i] ~ dnorm(mu[1], tau[1])T(0,)
    }
    for (i in 1:length(L_hist_chum)){
      L_hist_chum[i] ~ dnorm(mu[2], tau[2])T(0,)
    }
    mu[1] ~ dexp(0.0001)
    mu[2] ~ dexp(0.0001)
    tau[1] <- pow(sig[1], -2)
    tau[2] <- pow(sig[2], -2)
    sig[1] ~ dexp(0.0001)
    sig[2] ~ dexp(0.0001)
    
  }', file=file_dir)
  L_sonar <- mixture_data$L.mm.D
  L_sonar_tether <- c(632,602,1049,664,768,663,1025,685,681,957,953,747,646,666,627,531,584) 
  L_act_tether <- c(740,600,1170,710,950,650,1020,665,760,1040,970,840,700,700,620,620,690)
  L_hist_chin <- as.vector(suppressWarnings(na.omit(as.numeric(hist_dat$Length[hist_dat$species=="Chinook"]))))
  L_hist_chum <- as.vector(suppressWarnings(na.omit(as.numeric(hist_dat$Length[hist_dat$species=="Chum"]))))
  data <- list(L_sonar=L_sonar, 
               L_sonar_tether=L_sonar_tether, 
               L_act_tether=L_act_tether,
               L_hist_chin=L_hist_chin,
               L_hist_chum=L_hist_chum)
  t.start <- Sys.time()
  jags_out <- jagsUI::jags(model.file=file_dir, 
                           data=data, 
                           parameters.to.save=c("species", "L_act"),
                           n.chains=ncores, 
                           parallel = F, 
                           n.iter=niter, 
                           n.burnin=niter/2, 
                           n.thin=1, 
                           n.adapt=niter/10)
  diff <- round(Sys.time()-t.start,2)
  print(paste("Time to run model:", diff, units(diff)))
  return(jags_out)
}