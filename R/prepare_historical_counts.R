#' Prepare the historical fish counts for Hamachan's hierarchical run-timing model 
#' @param path File to the historical fish count excel file. Note: Jordy standardized the file format in 2020 -- if this function throws an error, make sure you are using the current version.
#' @param year The current year
#' @author Matt Tyres and Jordy Bernard
#' @export
prepare_historical_counts <- function(path, year){

  letter <- LETTERS[year-2018]
  range <- paste("A6:B", letter, "89", sep="")
  Cchin_histo <- suppressMessages(as.data.frame(readxl::read_xlsx(path, "Chena Historic Chinook", range, col_names=F)))
  Cchum_histo <- suppressMessages(as.data.frame(readxl::read_xlsx(path, "Chena Historic Chum", range, col_names=F)))
  Schin_histo <- suppressMessages(as.data.frame(readxl::read_xlsx(path, "Salcha Historic Chinook", range, col_names=F)))
  Schum_histo <- suppressMessages(as.data.frame(readxl::read_xlsx(path, "Salcha Historic Chum", range, col_names=F)))
  
  Cchin_histo_counts <- Cchin_histo[,seq(2,ncol(Cchin_histo),by=2)]
  Cchum_histo_counts <- Cchum_histo[,seq(2,ncol(Cchum_histo),by=2)]
  Schin_histo_counts <- Schin_histo[,seq(2,ncol(Schin_histo),by=2)]
  Schum_histo_counts <- Schum_histo[,seq(2,ncol(Schum_histo),by=2)]
  Cchin_histo_counttype <- Cchin_histo[,seq(3,ncol(Cchin_histo),by=2)]
  Cchum_histo_counttype <- Cchum_histo[,seq(3,ncol(Cchum_histo),by=2)]
  Schin_histo_counttype <- Schin_histo[,seq(3,ncol(Schin_histo),by=2)]
  Schum_histo_counttype <- Schum_histo[,seq(3,ncol(Schum_histo),by=2)]
  
  ## censoring data types "not allowed". v=visual, s=sonar, 0=interpolated somehow
  notallowed <- c(0)
  for(i in 1:length(notallowed)) {
    Cchin_histo_counts[Cchin_histo_counttype == notallowed[i]] <- NA
    Cchum_histo_counts[Cchum_histo_counttype == notallowed[i]] <- NA
    Schin_histo_counts[Schin_histo_counttype == notallowed[i]] <- NA
    Schum_histo_counts[Schum_histo_counttype == notallowed[i]] <- NA
  }
  
  ## setting up to use current year's available daily estimates in the Hamachan model
  Cchin_histo_counts[,ncol(Cchin_histo_counts)+1] <- NA
  Cchum_histo_counts[,ncol(Cchum_histo_counts)+1] <- NA
  Schin_histo_counts[,ncol(Schin_histo_counts)+1] <- NA
  Schum_histo_counts[,ncol(Schum_histo_counts)+1] <- NA
  
  getests <- function(x) ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded, x$sonar_count_expanded)
  
  Cchin_histo_counts[,ncol(Cchin_histo_counts)][1:nrow(Cchin_ra_ests)] <- getests(Cchin_ra_ests)
  Cchum_histo_counts[,ncol(Cchum_histo_counts)][1:nrow(Cchum_ra_ests)] <- getests(Cchum_ra_ests)
  Schin_histo_counts[,ncol(Schin_histo_counts)][1:nrow(Schin_ra_ests)] <- getests(Schin_ra_ests)
  Schum_histo_counts[,ncol(Schum_histo_counts)][1:nrow(Schum_ra_ests)] <- getests(Schum_ra_ests)
  
  colnames(Cchin_histo_counts) <- paste0("count",1993:year)
  colnames(Cchum_histo_counts) <- paste0("count",1993:year)
  colnames(Schin_histo_counts) <- paste0("count",1993:year)
  colnames(Schum_histo_counts) <- paste0("count",1993:year)
  
  
  return(list(Cchin_histo_counts=Cchin_histo_counts, Cchum_histo_counts=Cchum_histo_counts, Schin_histo_counts=Schin_histo_counts, Schum_histo_counts=Schum_histo_counts))
  
}












