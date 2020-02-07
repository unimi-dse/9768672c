% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{autoparam_tsarima}
\alias{autoparam_tsarima}
\title{To select automatically the best ARIMA model for a specified time series}
\usage{
autoparam_tsarima(ts, n_ts)
}
\arguments{
\item{ts}{A class 'ts' object.}

\item{n_ts}{A number referring to the colum of the time series.}
}
\value{
A class 'forecast_ARIMA' , "ARIMA" and 'Arima' object.
}
\description{
This function allows to look for the best parameters for the
tailored ARIMA model for a specific time series.
}
\examples{
INSERT EXAMPLE
}