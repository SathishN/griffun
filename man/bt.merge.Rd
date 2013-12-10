\name{bt.merge}
\alias{bt.merge}
\title{Align dates, faster version of merge function}
\usage{
  bt.merge(b, align = c("keep.all", "remove.na"),
    dates = NULL)
}
\arguments{
  \item{b}{enviroment with symbols time series}

  \item{align}{alignment type}

  \item{dates}{subset of dates}
}
\value{
  array of list of dates
}
\description{
  Align dates, faster version of merge function
}
\examples{
\dontrun{
bt.merge(b, align, dates)
}
}

