\name{peaks}
\alias{peaks}
\title{Peaks}
\usage{
  peaks(x, span = 3)
}
\arguments{
  \item{x}{time series}

  \item{span}{function}
}
\value{
  boolean vector
}
\description{
  This function takes a time series and returns a TRUE or
  FALSE in finding the peaks that exist in the series.
}
\examples{
\dontrun{
dat<-runif(10000,0,1)
peaks(dat)
}
}

