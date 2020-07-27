#' Compile visual and sonar estimates into a single data.frame
#' @param vis_expanded Expanded visual counts output from expand_counts
#' @param son_expanded Expanded sonar counts output from expand_counts
#' @author Matt Tyres and Jordy Bernard
#' @export

compile_estimates <- function(vis_expanded, son_expanded){
  com <- cbind(vis_expanded, son_expanded)
  colnames(com) <- c(paste("vis",colnames(vis_expanded),sep="_"), paste("sonar",colnames(son_expanded),sep="_"))
  rownames(com) <- attr(vis_expanded, "dates")
  return(com)
}
