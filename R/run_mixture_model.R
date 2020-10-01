#' Use sonar data to apportion and enumerate fish passage by species.
#' @description The function runs the big mixture model and outputs the posterior samples.
#' @param mixture_data Output of \code{\link{gather_mixture_data}}.
#' @param niter The number of MCMC iterations. 50k takes roughly 2.5 hours, 200k takes 9 hours, 500k takes 25ish. 
#' @param ncores The number of cores for parallel chains.
#' @author Matt Tyres and Jordy Bernard.
#' @export

run_mixture_model <- function(mixture_data, file_dir, niter = 500000, ncores = 3){
  cat('model {
    for(i in 1:n.fish) {
      L.mm.D[i] ~ dnorm(muL[i],precL)
      muL[i] <- betaD0[sonar[i]] + betaD1[sonar[i]]*L.mm.act[i]        ########
      L.mm.act[i] ~ dnorm(mu[i],tau[i])
      mu[i] <- lambda[species[i],sex[i],river[i]]
      tau[i] <- prec[species[i],sex[i],river[i]]
  
      species[i] ~ dcat(ps[i,1:2])
      sex[i] ~ dcat(psex[species[i],1:2])
  
      logit(pi[i]) <- b0[river[i]]+b1[river[i]]*day[i]
      alpha.inf[i,1] <- pi[i]
      alpha.inf[i,2] <- (1-pi[i])
      ps[i,1:2] ~ ddirch(alpha.inf[i,1:2])
    }
    
    prec[1,1,1] <- pow(chena_chin_m_sd,-2)
    prec[1,2,1] <- pow(chena_chin_f_sd,-2)
    prec[2,1,1] <- pow(chena_chum_m_sd,-2)
    prec[2,2,1] <- pow(chena_chum_f_sd,-2)
    prec[1,1,2] <- pow(salcha_chin_m_sd,-2)
    prec[1,2,2] <- pow(salcha_chin_f_sd,-2)
    prec[2,1,2] <- pow(salcha_chum_m_sd,-2)
    prec[2,2,2] <- pow(salcha_chum_f_sd,-2)
    
    chena_chin_m_sd ~ dnorm(chena_chin_m_sd_mn, chena_chin_m_sd_prec)
    chena_chin_f_sd ~ dnorm(chena_chin_f_sd_mn, chena_chin_f_sd_prec)
    chena_chum_m_sd ~ dnorm(chena_chum_m_sd_mn, chena_chum_m_sd_prec)
    chena_chum_f_sd ~ dnorm(chena_chum_f_sd_mn, chena_chum_f_sd_prec)
    salcha_chin_m_sd ~ dnorm(salcha_chin_m_sd_mn, salcha_chin_m_sd_prec)
    salcha_chin_f_sd ~ dnorm(salcha_chin_f_sd_mn, salcha_chin_f_sd_prec)
    salcha_chum_m_sd ~ dnorm(salcha_chum_m_sd_mn, salcha_chum_m_sd_prec)
    salcha_chum_f_sd ~ dnorm(salcha_chum_f_sd_mn, salcha_chum_f_sd_prec)
    chena_chin_m_sd_prec <- pow(chena_chin_m_sd_sd,-2)
    chena_chin_f_sd_prec <- pow(chena_chin_f_sd_sd,-2)
    chena_chum_m_sd_prec <- pow(chena_chum_m_sd_sd,-2)
    chena_chum_f_sd_prec <- pow(chena_chum_f_sd_sd,-2)
    salcha_chin_m_sd_prec <- pow(salcha_chin_m_sd_sd,-2)
    salcha_chin_f_sd_prec <- pow(salcha_chin_f_sd_sd,-2)
    salcha_chum_m_sd_prec <- pow(salcha_chum_m_sd_sd,-2)
    salcha_chum_f_sd_prec <- pow(salcha_chum_f_sd_sd,-2)

    # for(j in 1:m) {
    #   DL.star[j] ~ dnorm(mu.starD[j],prec.star)
    #   mu.star[j] <- betaD0[1] + betaD1[1]*AL.star[j]         ########
    # }
    precL <- 1/(54.59*54.59)
    # betaD0[1] ~ dnorm(0,0.01)         ########
    # betaD1[1] ~ dnorm(1,0.01)         ########
    # betaD0[2] ~ dnorm(0,0.01)         ########
    # betaD1[2] ~ dnorm(1,0.01)         ########
    # prec.star ~ dunif(0.0001,1000)
    # sig.star <- 1/sqrt(prec.star)

    betaD0[1] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    betaD1[1] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    betaD0[2] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    betaD1[2] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    # precbD0 <- 1/betaD0_se/betaD0_se         ########
    # precbD1 <- 1/betaD1_se/betaD1_se         ########

    # betaD0_mn <- 44.81988
    # betaD1_mn <- 0.87156
    # betaD0_se <- 63.36818
    # betaD1_se <- 0.07835

    b0 <- mu.b0 # ~ dnorm(mu.b0,prec.b0) #
    b1 <- mu.b1 # ~ dnorm(mu.b1,prec.b1) #

    # b0 ~ dmnorm(mu.b0[],tau.b0[,])
    # b1 ~ dmnorm(mu.b1[],tau.b1[,])
    # tau.b0[1:2,1:2] <- inverse(sig.b0[,])
    # tau.b1[1:2,1:2] <- inverse(sig.b1[,])
    # b0[1] ~ dnorm(mu.b0[1],tau.b0[1])
    # b0[2] ~ dnorm(mu.b0[2],tau.b0[2])
    # b1[1] ~ dnorm(mu.b1[1],tau.b1[1])
    # b1[2] ~ dnorm(mu.b1[2],tau.b1[2])

    # mu.b1 <- -0.1642404                    # calculated from 2015 visual counts (limited)
    # mu.b0 <- -0.7403224
    # mu.b1 <- -0.2812552                    # calculated from 1993-2015 counts
    # mu.b0 <- -0.4587457#-0.5185915
    prec.b0 <- 1/0.6543603#1/2.01842
    prec.b1 <- 1/0.003060084

    psex[1,1:2] ~ ddirch(alpha.sex.chin[])
    psex[2,1:2] ~ ddirch(alpha.sex.chum[])

    lambda[1,1,1] ~ dnorm(chena_chin_m_mn,t1)
    lambda[1,2,1] ~ dnorm(chena_chin_f_mn,t2)
    lambda[2,1,1] ~ dnorm(chena_chum_m_mn,t3)
    lambda[2,2,1] ~ dnorm(chena_chum_f_mn,t4)
    lambda[1,1,2] ~ dnorm(salcha_chin_m_mn,t5)
    lambda[1,2,2] ~ dnorm(salcha_chin_f_mn,t6)
    lambda[2,1,2] ~ dnorm(salcha_chum_m_mn,t7)
    lambda[2,2,2] ~ dnorm(salcha_chum_f_mn,t8)

    t1 <- pow(chena_chin_m_se,-2)
    t2 <- pow(chena_chin_f_se,-2)
    t3 <- pow(chena_chum_m_se,-2)
    t4 <- pow(chena_chum_f_se,-2)
    t5 <- pow(salcha_chin_m_se,-2)
    t6 <- pow(salcha_chin_f_se,-2)
    t7 <- pow(salcha_chum_m_se,-2)
    t8 <- pow(salcha_chum_f_se,-2)
    
    # N.chum <- sum(species[]) - n.fish       # posterior distributions of the totals of each species
    # N.chin <- (2*n.fish) - sum(species[])
    
    # # Nchum_chena <- sum(species[1:9042]) - nchena
    # # Nchin_chena <- (2*nchena) - sum(species[1:9042])
    # # Nchum_salcha <- sum(species[9043:12361]) - nsalcha
    # # Nchin_salcha <- (2*nsalcha) - sum(species[9043:12361])
    # for(k in 1:ndays) {   ### this is each day, but I dont think I want it after all
    # N.chum.day[k] <- sum(species[ifirst[k]:ilast[k]]) - nfishday[k]
    # N.chin.day[k] <- 2*nfishday[k] - sum(species[ifirst[k]:ilast[k]])
    # }
    # # N.chum.before <- sum(species[9693:9704]) + sum(species[9712:9717]) - 18
    # # N.chin.before <- 2*18 - sum(species[9693:9704]) - sum(species[9712:9717])
    # # N.chum.after <- sum(species[9705:9711]) + sum(species[9718:9719]) - 9
    # # N.chin.after <- 2*9 - sum(species[9705:9711]) - sum(species[9718:9719])
    
  }', file=file_dir)
  ## these nodes needed initial values at one point, I think recentering the lengths may have fixed it??
  # someinits <- function() list(betaD0=rnorm(2,-190,5),betaD1=rnorm(2,1.4,0.1))
  # print(paste("anticipated number of hours:", round(.1+.000044*niter, 2)))
  t.start <- Sys.time()
  jags_out <- jagsUI::jags(model.file=file_dir, data=mixture_data, 
                                          parameters.to.save=c("N.chum.day","N.chin.day","lambda","betaD0","betaD1","N.chin","N.chum","sig","species","L.mm.act","sex","prec"),
                                          n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 
  diff <- round(Sys.time()-t.start,2)
  print(paste("Time to run model:", diff, units(diff)))
  return(jags_out)
}
