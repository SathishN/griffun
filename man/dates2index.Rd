\name{dates2index}
\alias{dates2index}
\title{Determine the index of subset of dates in the time series}
\usage{
  dates2index(x, dates = 1:nrow(x))
}
\arguments{
  \item{x}{xts time series}

  \item{dates}{string represnting subset of dates i.e.
  '2010::2012'}
}
\value{
  index of subset of dates in the time series
}
\description{
  Determine the index of subset of dates in the time series
}
\examples{
\dontrun{
dates2index(data$prices, '2010::2012')
}
}

