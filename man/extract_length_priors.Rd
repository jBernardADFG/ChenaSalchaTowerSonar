% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_length_priors.R
\name{extract_length_priors}
\alias{extract_length_priors}
\title{Extract relavant data from posterior samples.}
\usage{
extract_length_priors(length_samples)
}
\arguments{
\item{length_samples}{The output of \code{\link{get_length_samples}}.}
}
\description{
Extracts the mean, sd, and se of carcass lengths for use in mixture model (see \code{\link{run_mixture_model}}).
}
\author{
Matt Tyres and Jordy Bernard.
}
