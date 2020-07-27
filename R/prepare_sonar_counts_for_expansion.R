#' Formats mixture model output and visual count data for expansion.
#' @description The posterior totals are formatted as a set of arrays, in which each cell corresponds to a 20-min counting block.  Array dimensions correspond to date, 8-hr shift, hour period, and 20-min block. Arrays are generated for point estimates (median) and variance, for each species and each sonar station. This information will be taken as input in expand_counts.
#' @param visual_counts Visual count data. See main_2019.R for more information.
#' @param sonar_counts Sonar count data. See main_2019.R for more information.
#' @param mixture_samples Output of run_mixture_model
#' @param trunc Lower bound for post-truncation based on input length. In past years, 450 seemed to work best.
#' @author Matt Tyres and Jordy Bernard
#' @export

prepare_sonar_counts_for_expansion <- function(visual_counts, sonar_counts, mixture_samples, trunc=450, lthresh=650){
  
  all_sonar <- sonar_counts$all_sonar
  sonar_fish <- sonar_counts$sonar_fish
  
  df <- as.data.frame(as.matrix(mixture_samples$samples))
  
  pull_post <- function(x, p){
    x[,substr(names(x),1,nchar(p))==p]
  }
  
  specmat <- pull_post(df, "species")
  sonar_fish$modlength <- apply(pull_post(df, "L.mm"), 2, median) + mean(sonar_fish$length)
  
  # For post-truncation based on input length, in past years, 450 seemed to work best.
  trunc_subset <- (sonar_fish$length >= trunc)
  specmat <- specmat[,trunc_subset]
  sonar_fish <- sonar_fish[trunc_subset,]
  
  # simplifying to TRUE or FALSE for each species
  chinmat <- specmat==1
  chummat <- specmat==2
  
  dates <- sort(unique(paste(year=as.numeric(substr(sonar_fish$date[1],1,4)), substr(as.character(visual_counts[[1]]$Day),6,10),sep="-")))
  
  CN_chum <- CS_chum <- SN_chum <- SS_chum <- CN_chin <- CS_chin <- SN_chin <- SS_chin <- 
    vCN_chum <- vCS_chum <- vSN_chum <- vSS_chum <- vCN_chin <- vCS_chin <- vSN_chin <- vSS_chin <- array(0, dim=c(length(dates),3,8,3))
  
  attr(CN_chum, "dates") <- attr(SN_chum, "dates") <- attr(SS_chum, "dates") <- attr(CN_chum, "dates") <- attr(CN_chin, "dates") <-
    attr(CS_chin, "dates") <- attr(SN_chin, "dates") <- attr(SS_chin, "dates") <- attr(vCN_chum, "dates") <- attr(vCS_chum, "dates") <-
    attr(vSN_chum , "dates") <- attr(vSS_chum, "dates") <- attr(vCN_chin, "dates") <- attr(vCS_chin, "dates") <- attr(vSN_chin, "dates") <- 
    attr(vSS_chin, "dates") <- dates
  
  idate <- 1
  
  for(datei in dates) {
    for(shifti in 1:3) {
      for(periodi in 1:8) {
        for(min20i in 1:3) {
          a <- with(sonar_fish, station=="Chena North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)>0) {
            CN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
            CN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
            vCN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
            vCN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          }
          
          a <- with(sonar_fish, station=="Chena South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)>0) {
            CS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
            CS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
            vCS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
            vCS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          }
          a <- with(sonar_fish, station=="Salcha North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)>0) {
            SN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
            SN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
            vSN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
            vSN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          }
          a <- with(sonar_fish, station=="Salcha South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)>0) {
            SS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
            SS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
            vSS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
            vSS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          }
          
          a <- with(all_sonar, station=="Chena North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)==0) {
            CN_chin[idate,shifti,periodi,min20i] <- NA
            CN_chum[idate,shifti,periodi,min20i] <- NA
            vCN_chin[idate,shifti,periodi,min20i] <- NA
            vCN_chum[idate,shifti,periodi,min20i] <- NA
          }
          a <- with(all_sonar, station=="Chena South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)==0) {
            CS_chin[idate,shifti,periodi,min20i] <- NA
            CS_chum[idate,shifti,periodi,min20i] <- NA
            vCS_chin[idate,shifti,periodi,min20i] <- NA
            vCS_chum[idate,shifti,periodi,min20i] <- NA
          }
          a <- with(all_sonar, station=="Salcha North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)==0) {
            SN_chin[idate,shifti,periodi,min20i] <- NA
            SN_chum[idate,shifti,periodi,min20i] <- NA
            vSN_chin[idate,shifti,periodi,min20i] <- NA
            vSN_chum[idate,shifti,periodi,min20i] <- NA
          }
          a <- with(all_sonar, station=="Salcha South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
          if(sum(a)==0) {
            SS_chin[idate,shifti,periodi,min20i] <- NA
            SS_chum[idate,shifti,periodi,min20i] <- NA
            vSS_chin[idate,shifti,periodi,min20i] <- NA
            vSS_chum[idate,shifti,periodi,min20i] <- NA
          }
        }
      }
    }
    idate <- idate+1
  }
  list(CN_chum=CN_chum,
      CS_chum=CS_chum,
      SN_chum=SN_chum,
      SS_chum=SS_chum,
      CN_chin=CN_chin,
      CS_chin=CS_chin,
      SN_chin=SN_chin,
      SS_chin=SS_chin, 
      vCN_chum=vCN_chum,
      vCS_chum=vCS_chum,
      vSN_chum=vSN_chum,
      vSS_chum=vSS_chum,
      vCN_chin=vCN_chin,
      vCS_chin=vCS_chin,
      vSN_chin=vSN_chin,
      vSS_chin=vSS_chin)
  
}
