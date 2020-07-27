#' Expand visual and sonar counts
#' @description Function calculates the variance due to expansion from incomplete counts as well as from the expansion itself by implementing the variance calculations for the visual data (from the spreadsheet). In the case of the sonar, it is assumed that the the variances due to expansion and due to species apportionment are independent of one another, and can be summed.  
#' @param x Typically the sum of two arrays where one cooresponds to each side of the river. When expanding sonar counts the arrays output from prepare_sonar_counts_for_expansion. When expanding visual counts, the arrays will be output from prepare_visual_counts_for_expansion. See examples.
#' @param vx Variance due to species apportionment. When expanding visual counts, use vx=NULL. See examples. 
#' @author Matt Tyres and Jordy Bernard
#' @export

expand_counts <- function(x, vx=NULL) {
  worstcase=T
  Sydij <- apply(x, 1:2, sum, na.rm=T)         # count per shift
  # Sydi <- rowSums(Sydij)                       # count per day
  mdi <- apply(!is.na(x), 1:2, sum, na.rm=T)   # number of periods sampled per shift
  Sydij[mdi<4] <- 0 # or NA
  mdi[mdi<4] <- 0
  md <- rowSums(mdi)                           # number of periods sampled per day
  hdi <- mdi>0                                 # shifts with counts
  hd <- rowSums(hdi)                           # number of shifts with counts per day
  Md <- 72                                     # total possible periods per day
  Mdi <- 24                                    # total possible periods per shift
  Hd <- 3                                      # total possible shifts per day
  Ydi <- Mdi/mdi*Sydij                         # expanded count per shift
  Ydavg <- rowMeans(Ydi,na.rm=T)               # avg shift escapement per day
  Nd <- Ydavg*Hd                               # expanded daily escapement
  
  if(!is.null(vx)) {
    assign_var_raw <- apply(vx,1, sum, na.rm=T)
    mdi[mdi==0] <- NA
    assign_var_expanded <- ((Hd/hd)^2)*rowSums(((Mdi/mdi)^2)*apply(vx, 1:2, sum, na.rm=T), na.rm=T)
  }
  
  RDS <- (x[,,-1,]-x[,,-8,])^2
  sumRDS <- apply(RDS, 1:2, sum, na.rm=T)
  s22di <- sumRDS/2/(mdi-1)
  s21d <- rowSums((Ydi-Ydavg)^2,na.rm=T)/(hd-1)
  f1d <- hd/Hd
  f2di <- mdi/Mdi
  V1 <- (1-f1d)*(Hd^2)*s21d/hd
  V2 <- (1/f1d)*rowSums((1-f2di)*(Mdi^2)*s22di/mdi, na.rm=T)
  VNd <- V1+V2
  
  ## -- this section was for applying the interpolation (with variance).
  ## -- decided to interpolate AFTER putting sonar & visual together.
  # if(worstcase) {
  #   cvmax <- max(sqrt(VNd)/Nd, na.rm=T)
  #   vworstcase <- (Nd*cvmax)^2
  #   VNd[is.na(VNd)] <- vworstcase[is.na(VNd)]
  # }
  
  # # this is where I should figure out how to interpolate
  # # interpolate where md==0
  # md0 <- md==0
  # md01 <- md02 <- F
  # n <- length(md0)
  # md01[-n] <- md0[-1]
  # md02[-1] <- md0[-n]
  # # wait... should interpolate AFTER putting sonar & visual together
  # # do need to take out Nd where hd==1 (i think)
  Nd[md==0 | hd==1] <- NA
  
  if(!is.null(vx)) out <- data.frame(count_raw=apply(x,1,sum,na.rm=T), count_expanded=Nd, var_expansion=VNd,
                                     nperiods1=mdi[,1],nperiods2=mdi[,2],nperiods3=mdi[,3],
                                     var_assign_raw=assign_var_raw, var_assign_expanded=assign_var_expanded,
                                     var_total=VNd+assign_var_expanded)
  if(is.null(vx)) out <- data.frame(count_raw=apply(x,1,sum,na.rm=T), count_expanded=Nd, var_expansion=VNd,
                                    nperiods1=mdi[,1],nperiods2=mdi[,2],nperiods3=mdi[,3])
  attr(out, "dates") <- attr(x, "dates")
  return(out)
}
