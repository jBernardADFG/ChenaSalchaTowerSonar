#' Run Hamachan's hierarchical run-timing model. 
#' @param historical_counts Output of \code{\link{prepare_historical_counts}}. 
#' @param file_dir The file directory to write the jags model. The directory needs to have a .jags extension.
#' @param ncores The number of cores for parallel chains.
#' @param niter The number of MCMC iterations.
#' @author Jordy Bernard.
#' @export

run_hamachan <- function(historical_counts, file_dir, ncores=3, niter=200000){
  
  cat(
    'model {
    
    for(j in 1:nyrs) {
      for(i in 1:ndays){
      
        y1[i,j] ~ dnorm(theta1[i,j], tausq1[j])
        y2[i,j] ~ dnorm(theta2[i,j], tausq2[j])
            
        # Assume that run timing distribution takes log normal distribution 
        theta1[i,j] <- a1[j]*exp(-0.5*pow(log(x[i]/mu1[j])/b1[j],2))
        theta2[i,j] <- a2[j]*exp(-0.5*pow(log(x[i]/mu2[j])/b2[j],2))
        
        # Assume that run timing distribution takes Extreme value distribution 
        # theta1[i,j] <- a1[j]*exp(-exp(-(x[i]-mu1[j])/b1[j])-(x[i]-mu1[j])/b1[j]+1)
        # theta2[i,j] <- a2[j]*exp(-exp(-(x[i]-mu2[j])/b2[j])-(x[i]-mu2[j])/b2[j]+1)
        
        # Assume that run timing distribution takes log-logistic distribution 
        # theta1[i,j] <- (a1[j]*(b1[j]/mu1[j])*pow((x[i]/mu1[j]),b1[j]-1))/pow(1+pow((x[i]/mu1[j]),b1[j]),2)
        # theta2[i,j] <- (a2[j]*(b2[j]/mu2[j])*pow((x[i]/mu2[j]),b2[j]-1))/pow(1+pow((x[i]/mu2[j]),b2[j]),2)
           
      }
    }
    
    # Priors
    for(i in 1:nyrs) {
       
      a1[i] ~ dnorm(0,0.0001)T(0.001,)
      a2[i] ~ dnorm(0,0.0001)T(0.001,)
      
      b1[i] ~ dnorm(b01,b01.prec)T(0.0001,)
      b2[i] ~ dnorm(b02,b02.prec)T(0.0001,)
      
      mu1[i] ~ dnorm(mu[i], mu01.prec)T(0.0001,)
      mu2[i] ~ dnorm(mu[i], mu02.prec)T(0.0001,)
      mu[i] ~ dnorm(mu0, mu0.prec)T(0.0001,)
      
    } 
    
    b01 ~ dexp(0.0001)
    b02 ~ dexp(0.0001)
    
    b01.prec <- pow(1/b01.sig, 2)
    b02.prec <- pow(1/b02.sig, 2)
    b01.sig ~ dexp(0.0001)
    b02.sig ~ dexp(0.0001)
    
    mu01.prec <- pow(1/mu01.sig, 2)
    mu02.prec <- pow(1/mu02.sig, 2)
    mu01.sig ~ dexp(0.0001)
    mu02.sig ~ dexp(0.0001)
    
    mu0 ~ dexp(0.0001)
    mu0.prec <- pow(1/mu0.sig, 2)
    mu0.sig ~  dexp(0.0001)
    
    for(i in 1:nyrs) {
    
      tausq1[i] <- pow(1/sigma1[i],2)
      tausq2[i] <- pow(1/sigma2[i],2)
      sigma1[i] ~ dexp(0.0001)
      sigma2[i] ~ dexp(0.0001)
    
    }
    
    # Backestimate escapement 
    for(j in 1:nyrs){
      for(i in 1:ndays){ 
        y1est[i,j] <- y1[i,j]
        y2est[i,j] <- y2[i,j]
      }
    }
    
  }', file=file_dir)
  
  
  runHamachan <- function(y1,y2) {
    
    y1[y1<0]<-0
    y2[y2<0]<-0
    
    hiermod1.jags.data <- list(y1=log(y1+1),
                               y2=log(y2+1),
                               nyrs=dim(y1)[2],
                               ndays=dim(y1)[1],
                               x=1:nrow(y1))
    
    hiermod1.jags.out <- jagsUI::jags(model.file=file_dir,
                                      data=hiermod1.jags.data, 
                                      parameters.to.save=c("y1est","y2est"), 
                                      n.chains=ncores, parallel=T, n.iter=niter, n.burnin=niter/2, n.adapt=niter/10, n.thin=niter/1000)
    
    return(hiermod1.jags.out)
  }
  
  chin_samples <- runHamachan(y1=historical_counts$Cchin_histo_counts, y2=historical_counts$Schin_histo_counts)
  chum_samples <- runHamachan(y1=historical_counts$Cchum_histo_counts, y2=historical_counts$Schum_histo_counts)
  
  return(list(chin_samples=chin_samples, chum_samples=chum_samples))
  
}






