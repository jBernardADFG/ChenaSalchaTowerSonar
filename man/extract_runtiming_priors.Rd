% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_runtiming_priors.R
\name{extract_runtiming_priors}
\alias{extract_runtiming_priors}
\title{Extract relavant data from posterior samples.}
\usage{
extract_runtiming_priors(runtiming_samples)
}
\arguments{
\item{runtiming_samples}{The output of \code{\link{get_runtiming_samples}}.}
}
\description{
Extracts regression parameters of hierarchical logistic regression for use in mixture model (see \code{\link{run_mixture_model}}).
}
\author{
Matt Tyres and Jordy Bernard.
}
