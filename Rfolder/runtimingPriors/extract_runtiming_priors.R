#' Extract relavant data from posterior samples.
#' @description Extracts regression parameters of hierarchical logistic regression for use in mixture model (see \code{\link{run_mixture_model}}).
#' @param runtiming_samples The output of \code{\link{get_runtiming_samples}}.
#' @author Matt Tyres and Jordy Bernard.
#' @export

extract_runtiming_priors <- function(runtiming_samples){
  list(a0 = runtiming_samples$mean$a0,
      a1 = runtiming_samples$mean$a1,
      b0 = runtiming_samples$mean$b0,
      b1 = runtiming_samples$mean$b1)
}
