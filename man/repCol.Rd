\name{repCol}
\alias{repCol}
\title{Repeat Rows}
\usage{
  repCol(m, nc)
}
\arguments{
  \item{m}{vector (column)}

  \item{nc}{number of copies along columns}
}
\value{
  new matrix
}
\description{
  Repeat Rows
}
\examples{
\dontrun{
matrix(1:5, nr=5, nc=3, byrow=F)
repCol(1:5, 3)
}
}

