% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_DIDSON.R
\name{standardize_DIDSON}
\alias{standardize_DIDSON}
\title{Converts DIDSON data to a standardized format.}
\usage{
standardize_DIDSON(x, river, station)
}
\arguments{
\item{x}{data.frame returned from \code{\link{ProcessFiles_DIDSON}}.}

\item{river}{Name of the river (e.g. "Chena").}

\item{station}{Name of sonar station (e.g. "Chena North").}
}
\description{
Takes in the output of \code{\link{ProcessFiles_DIDSON}} and returns a data frame containing river, station, sonar type, date, hour, minute, second, fractional day, length, shift, and hour within shift ASSOCIATED WITH EACH RECORDED SONAR TARGET.  Rows with NA for length correspond to sonar files with no fish recorded. This is where "zero" counts will come from in the expansion.
}
\author{
Matt Tyers.
}
