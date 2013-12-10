\name{write.excel}
\alias{write.excel}
\title{R object to Excel}
\usage{
  write.excel(x, row.names = FALSE, col.names = TRUE, ...)
}
\arguments{
  \item{x}{object}

  \item{row.names}{boolean}

  \item{col.names}{boolean}
}
\value{
  object
}
\description{
  Exporting a R object to Excel via the clipboard
}
\examples{
\dontrun{
write.excel(dat)
}
}

