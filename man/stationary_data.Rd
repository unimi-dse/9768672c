% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{stationary_data}
\alias{stationary_data}
\title{To verify the order of integration of a time series}
\usage{
stationary_data(ts)
}
\arguments{
\item{ts}{A univariate time series of class 'ts' object.}
}
\value{
A logical TRUE/FALSE object.
}
\description{
This function allows to verify if the time series is stationary
or not (consider first differences).
}
\examples{
dataset<-data.frame('something'=c(...))
dataset_ts<-ts(dataset, ...)
stationary_data(dataset_ts[,1])
}
