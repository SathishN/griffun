\name{fund.data}
\alias{fund.data}
\title{Get Fundamental Data from advfn
http://advfn.com/p.php?pid=financials&symbol=CSCO&mode=quarterly_reports
http://advfn.com/p.php?pid=financials&symbol=CSCO&mode=annual_reports}
\usage{
  fund.data(Symbol, n = 10,
    mode = c("quarterly", "annual"), max.attempts = 5)
}
\arguments{
  \item{Symbol}{ticker}

  \item{n}{number of periods}

  \item{mode}{periodicity}

  \item{max.attempts}{maximum number of attempts to
  download before exit}
}
\description{
  Get Fundamental Data from advfn
  http://advfn.com/p.php?pid=financials&symbol=CSCO&mode=quarterly_reports
  http://advfn.com/p.php?pid=financials&symbol=CSCO&mode=annual_reports
}
\examples{
\dontrun{
fund.data('XOM')
}
}

