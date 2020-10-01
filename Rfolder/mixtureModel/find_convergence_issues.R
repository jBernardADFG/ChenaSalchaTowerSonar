#' Identify parameters with Rhat values > 1.1
#' @param mixture_samples Output of \code{\link{run_mixture_model}}.
#' @author Jordy Bernard.
#' @export

find_convergence_issues <- function(mixture_samples){
  Rhat <- mixture_samples$Rhat
  rlist <- list()
  for(i in 1:length(R_hat)){
    issue_mat <- abs(Rhat[[i]]-1) > 0.1
    issue_mat[is.na(issue_mat)] <- F
    rlist <- c(rlist, list(issue_mat))
  }
  names(rlist) <- names(Rhat)
  return(rlist)
}