###############################################################################
#
# Copyright (C) 2014  Drew Griffith
#
# For more information please visit my blog at http://drewgriffith15.tumblr.com/
###############################################################################
#' Adding future dates to a forecast
#'
#' @param dates the dates of the historical values
#' @param forecast a vector of forecasted values
#'
#' @keywords forecast dates
#' @export
#' @examples
#' ##NULL
extendForecast <- function(dates, forecast) {
  require(timeDate)
  require(xts)
  # TESTING FOR FREQ: 1-daily; 4-quarterly; 12-monthly
  # weekly is not supported by timeDate package
  freq <- frequency(as.timeDate(dates))
  if (freq == 1) {
    h <- NROW(forecast)
    new.dates <- as.Date(dates)
    new.dates <- seq(last(new.dates) + 1, last(new.dates) + 365, by = "day")
    new.dates <- timeDate(new.dates)
    new.dates <- as.Date(new.dates[isBizday(new.dates, holidayNYSE())])
    new.dates <- new.dates[1:h]
    return(as.xts(forecast, new.dates))
  }
  if (freq == 4) {
    h <- NROW(forecast)
    new.dates <- as.timeDate(dates)
    new.dates <- as.Date(alignQuarterly(timeSequence(from = last(dates) + 
                                                     1, by = "quarter", length.out = h)))
    return(as.xts(forecast, new.dates))
  }
  if (freq == 12) {
    h <- NROW(forecast)
    new.dates <- as.timeDate(dates)
    new.dates <- as.Date(alignMonthly(timeSequence(from = last(dates) + 
                                                     1, by = "month", length.out = h)))
    return(as.xts(forecast, new.dates))
  }
}