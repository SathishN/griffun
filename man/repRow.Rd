\name{repRow}
\alias{repRow}
\title{Repeat Rows}
\usage{
  repRow(m, nr)
}
\arguments{
  \item{m}{vector (row)}

  \item{nr}{number of copies along rows}
}
\value{
  new matrix
}
\description{
  Repeat Rows
}
\examples{
\dontrun{
matrix(1:3, nr=5, nc=3, byrow=T)
repRow(1:3, 5)
}
}

