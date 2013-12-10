\name{seasonals}
\alias{seasonals}
\title{Seasonal Averages}
\usage{
  seasonals(x, frequency)
}
\arguments{
  \item{x}{dataset}

  \item{frequency}{how often}
}
\value{
  vector
}
\description{
  This function will calculate the seasonal averages of a
  time series based on the frequency (average of N rows)
}
\examples{
\dontrun{
seasonals(x,12)
}
}

