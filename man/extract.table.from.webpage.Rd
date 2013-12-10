\name{extract.table.from.webpage}
\alias{extract.table.from.webpage}
\title{Extract.table.from.webpage}
\usage{
  extract.table.from.webpage(txt, marker, hasHeader = T)
}
\arguments{
  \item{txt}{source text of webpage}

  \item{marker}{key-phrase(s) located in the table to
  extract}

  \item{hasHeader}{flag if table has a header}
}
\value{
  temp
}
\description{
  Extract.table.from.webpage
}
\examples{
\dontrun{
url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
txt = join(readLines(url))
temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
}
}

