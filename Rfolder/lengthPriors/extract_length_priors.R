#' Extract relavant data from posterior samples.
#' @description Extracts the mean, sd, and se of carcass lengths for use in mixture model (see \code{\link{run_mixture_model}}).
#' @param length_samples The output of \code{\link{get_length_samples}}.
#' @author Matt Tyres and Jordy Bernard.
#' @export

extract_length_priors <- function(length_samples){
  list(prior_mn = length_samples$mean$catmean,
    prior_se = length_samples$sd$catmean,
    prior_sd_mn = length_samples$mean$sigma,
    prior_sd_sd = length_samples$sd$sigma,
    mu_mn = length_samples$mean$mu,
    mu_sd = length_samples$sd$mu)
}
