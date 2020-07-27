#' Apply the running-average interpolation method where applicable.
#' @param x Output of \code{\link{compile_estimates}}.
#' @author Matt Tyres and Jordy Bernard.
#' @export
runningavg_interp <- function(x) {
  avgcv <- mean(sqrt(x$vis_var_expansion)/x$vis_count_expanded, na.rm=T)
  est <- ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded, x$sonar_count_expanded)
  # estvar <- ifelse(!is.na(x$vis_count_expanded), x$vis_var_expansion, x$sonar_var_expansion)
  interp_count <- interp_var <- rep(NA,nrow(x))
  for(i in 2:(length(est)-1)) {
    runcheck <- is.na(est[(i-1):(i+1)])
    if(all(runcheck==c(F,T,F))) {
      interp_count[i] <- mean(est[(i-1):(i+1)], na.rm=T)
      interp_var[i] <- (avgcv*interp_count[i])^2
    }
  }
  for(i in 3:(length(est)-2)) {
    runcheck <- is.na(est[(i-2):(i+2)])
    if(all(runcheck==c(F,T,T,F,F)) | all(runcheck==c(F,F,T,T,F))) {
      interp_count[i] <- mean(est[(i-2):(i+2)], na.rm=T)
      interp_var[i] <- (avgcv*interp_count[i])^2
    }
  }
  x1 <- cbind(x,interp_count,interp_var)
}
