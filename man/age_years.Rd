\name{age_years}
\alias{age_years}
\title{Age Years}
\usage{
  age_years(earlier, later)
}
\arguments{
  \item{earlier}{first date}

  \item{later}{second date}
}
\value{
  integer
}
\description{
  This function for calculating age with two dates
}
\examples{
\dontrun{
x <- as.Date("2000-02-29")
y <- as.Date("2004-02-28")
age_years(x, y)
}
}

