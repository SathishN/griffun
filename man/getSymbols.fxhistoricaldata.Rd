\name{getSymbols.fxhistoricaldata}
\alias{getSymbols.fxhistoricaldata}
\title{Download FX qoutes: end of day and hourly}
\usage{
  getSymbols.fxhistoricaldata(Symbols,
    type = spl("hour,day"), env = .GlobalEnv,
    auto.assign = TRUE, download = FALSE)
}
\arguments{
  \item{Symbols}{tickers}

  \item{type}{hourly or daily}

  \item{env}{envelope}

  \item{auto.assign}{assignment}

  \item{download}{true or false}
}
\description{
  An xml alternative
}
\examples{
\dontrun{
getSymbols.fxhistoricaldata(spl('MSFT,AAPL,NYSE:RY'))
}
}

