\name{get.fund.data}
\alias{get.fund.data}
\title{Extract and process fundamental data item}
\usage{
  get.fund.data(label, fund, fund.date, is.12m.rolling = F,
    cash.flow = F)
}
\arguments{
  \item{label}{label of fundamental data item you looking
  for}

  \item{fund}{matrix with fundamental data}

  \item{fund.date}{dates}

  \item{is.12m.rolling}{flag to compute 12 month rolling
  sum}

  \item{cash.flow}{flag to adjust cash flow data items}
}
\description{
  Extract and process fundamental data item
}
\examples{
\dontrun{
get.fund.data(data)
}
}

