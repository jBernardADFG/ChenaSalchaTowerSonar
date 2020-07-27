#' Extract the final results from the model.
#' @param hamachan_samples The output of run_hamachan.
#' @param historical_counts The output of prepare_historical_counts.
#' @param ra_ests The output of runningavg_interp.
#' @author Matt Tyres and Jordy Bernard
#' @export

extract_final_results <- function(hamachan_samples, historical_counts, ra_ests){
  
  nyrs <- ncol(historical_counts$Cchin_histo_counts)
  
  hamachan_samples$chin_samples
  hamachan_samples$chum_samples
  
  Cchin_ham <- exp(hamachan_samples$chin_samples$sims.list$y1est[,,nyrs])-1
  Cchum_ham <- exp(hamachan_samples$chum_samples$sims.list$y1est[,,nyrs])-1
  Schin_ham <- exp(hamachan_samples$chin_samples$sims.list$y2est[,,nyrs])-1
  Schum_ham <- exp(hamachan_samples$chum_samples$sims.list$y2est[,,nyrs])-1
  
  hierinterp <- function(x, ham, hamstart=as.Date("2019-06-23")) {
    x1a <- matrix(nrow=hamstart-min(as.Date(rownames(x))), ncol=ncol(x))
    x1b <- matrix(nrow=dim(ham)[2]-nrow(x)-(hamstart-min(as.Date(rownames(x)))), ncol=ncol(x))
    colnames(x1a) <- colnames(x1b) <- colnames(x)
    if(nrow(x1a)>0) rownames(x1a) <- as.character(hamstart-(nrow(x1a):1))
    if(nrow(x1b)>0) rownames(x1b) <- as.character(max(as.Date(rownames(x)))+(1:nrow(x1b)))
    x1 <- rbind(x1a,x,x1b)
    # nr <- nrow(x)           # this only works because they start on the same date!!
    x1$hierinterp <- apply(ham, 2, median)#[1:nr]
    x1$hierinterp_var <- apply(ham, 2, var)#[1:nr]
    x1$hierinterp[!is.na(x1$vis_count_expanded) | !is.na(x1$sonar_count_expanded)] <- NA
    x1$hierinterp_var[!is.na(x1$vis_count_expanded) | !is.na(x1$sonar_count_expanded)] <- NA
    return(x1)
  }
  
  Cchin_ests2 <- hierinterp(x=ra_ests$Cchin_ra_ests, ham=Cchin_ham)
  Cchum_ests2 <- hierinterp(x=ra_ests$Cchum_ra_ests, ham=Cchum_ham)
  Schin_ests2 <- hierinterp(x=ra_ests$Schin_ra_ests, ham=Schin_ham)
  Schum_ests2 <- hierinterp(x=ra_ests$Schum_ra_ests, ham=Schum_ham)
  
  makeests <- function(x) {
    x$DailyEst <- ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded,
                         ifelse(!is.na(x$sonar_count_expanded), x$sonar_count_expanded,
                                ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp)))
    x$DailyVar <- ifelse(!is.na(x$vis_count_expanded), x$vis_var_expansion,
                         ifelse(!is.na(x$sonar_count_expanded), x$sonar_var_total,
                                ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp_var)))
    x$DailyMethod <- ifelse(!is.na(x$vis_count_expanded), "Visual",
                            ifelse(!is.na(x$sonar_count_expanded), "Sonar",
                                   ifelse(!is.na(x$interp_count), "Running Avg Interpolation", "Hier Run-timing Mod")))
    nc <- ncol(x)
    x <- x[,c((nc-2):nc,1:(nc-3))]
    return(x)
  }
  
  Cchin_final <- makeests(x=Cchin_ests2)
  Cchum_final <- makeests(x=Cchum_ests2)
  Schin_final <- makeests(x=Schin_ests2)
  Schum_final <- makeests(x=Schum_ests2)
  
  return(list(Cchin_final=Cchin_final, Cchum_final=Cchum_final, Schin_final=Schin_final, Schum_final=Schum_final))
  
}
