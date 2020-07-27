#' Format the visual counts for expansion.
#' @description Function formats the visual counts so that they can be expanded using the function \code{\link{expand_counts}}. This function will return an array where each cell corresponds to a 20-min counting block.  Array dimensions correspond to date, 8-hr shift, hour period, and 20-min block. 
#' @param visual_counts Visual count data. See examples.
#' @param sonar_expansion_data Output of \code{\link{prepare_sonar_counts_for_expansion}}.
#' @export

prepare_visual_counts_for_expansion <- function(visual_counts, sonar_expansion_data){
  Schin_vis <- Schum_vis <- Cchin_vis <- Cchum_vis <- NA*sonar_expansion_data[[1]]
  attr(Schin_vis, "dates") <- attr(Schum_vis, "dates") <- attr(Cchin_vis, "dates") <- attr(Cchum_vis, "dates") <- attr(sonar_expansion_data[[1]], "dates")
  
  for(i in 1:3) {
    Cnr <- nrow(visual_counts$Chena_2019_Chin_vis)
    Snr <- nrow(visual_counts$Salcha_2019_Chin_vis)
    Cchin_vis[,i,,1] <- as.matrix(visual_counts$Chena_2019_Chin_vis[seq(i,Cnr,by=3),3:10])
    Cchum_vis[,i,,1] <- as.matrix(visual_counts$Chena_2019_Chum_vis[seq(i,Cnr,by=3),3:10])
    Schin_vis[,i,,1] <- as.matrix(visual_counts$Salcha_2019_Chin_vis[seq(i,Snr,by=3),3:10])
    Schum_vis[,i,,1] <- as.matrix(visual_counts$Salcha_2019_Chum_vis[seq(i,Snr,by=3),3:10])
  }
  list(Schin_vis=Schin_vis, Schum_vis=Schum_vis, Cchin_vis=Cchin_vis, Cchum_vis=Cchum_vis)
}
