#' Read in Historical Run Timing Data
#' @description Function reads in historical run timing data and formats the data for use in get_runtiming_samples.
#' @param chena_file Directory of the Chena run timing data
#' @param salcha_file Directory of the Salcha run timing data
#' @author Matt Tyres and Jordy Bernard
#' @export

prepare_runtiming_data <- function(chena_file, salcha_file){
  Chena_runtiming <- read.csv(chena_file)
  Chena_runtiming$date205 <- Chena_runtiming$julian - 205 # approximately centering date
  Chena_runtiming$chinook[Chena_runtiming$chinook<0] <- 0
  Chena_runtiming$chum[Chena_runtiming$chum<0] <- 0
  Salcha_runtiming <- read.csv(salcha_file)
  Salcha_runtiming$date205 <- Salcha_runtiming$julian - 205
  Salcha_runtiming$chinook[Salcha_runtiming$chinook<0] <- 0
  Salcha_runtiming$chum[Salcha_runtiming$chum<0] <- 0
  
  ## only including years that have data for both rivers
  yearsboth <- intersect(unique(Chena_runtiming$year),unique(Salcha_runtiming$year))
  Chena_sub <- subset(Chena_runtiming,year %in% yearsboth)
  Salcha_sub <- subset(Salcha_runtiming,year %in% yearsboth)
  
  ## only including dates with nonzero counts
  Chena_sub <- Chena_sub[rowSums(Chena_sub[,2:3])>0,]
  Salcha_sub <- Salcha_sub[rowSums(Salcha_sub[,2:3])>0,]
  
  CS.data <- list(Stot=Salcha_sub$chinook+Salcha_sub$chum,Schin=Salcha_sub$chin,Sday=Salcha_sub$date205,Syear=as.numeric(as.factor(Salcha_sub$year)),
                  Ctot=Chena_sub$chinook+Chena_sub$chum,Cchin=Chena_sub$chin,Cday=Chena_sub$date205,Cyear=as.numeric(as.factor(Chena_sub$year)),
                  nyear=length(unique(Salcha_sub$year)),Sn=nrow(Salcha_sub),Cn=nrow(Chena_sub))
  return(CS.data)
}
