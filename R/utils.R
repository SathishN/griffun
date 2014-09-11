###############################################################################
# Collection of General Utilities
# Copyright (C) 2013  Drew Griffith
#
# For more information please visit my blog at http://drewgriffith15.tumblr.com/
###############################################################################

###############################################################################
#' Remove Outliers from a dataset
#'
#' This function removes outliers from a dataset
#' credit for this source code goes to aL3xa on StackOverflow
#'
#' @param x object
#' @param na.rm boolean
#' 
#' @return object
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' x <- c(-10, x, 10)
#' y <- remove_outliers(x)
#' par(mfrow = c(1, 2))
#' boxplot(x)
#' boxplot(y)
#' }
#' @export
###############################################################################

remove.outliers <- function(x, na.rm = TRUE, ...) {
  quant <- quantile(x, probs=c(.1, .9), na.rm = na.rm, ...)
  i <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - i)] <- NA
  y[x > (qnt[2] + i)] <- NA
  y
}

###############################################################################
#' Excel data to R
#'
#' This function specifies that you are reading data from the clipboard,
#' that it is tab delimited, and that it has a header.
#'
#' @param header boolean
#' 
#' @return object
#'
#' @examples
#' \dontrun{
#' dat=read.excel()
#' }
#' @export
###############################################################################

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

###############################################################################
#' R object to Excel
#'
#' Exporting a R object to Excel via the clipboard
#'
#' @param x object
#' @param row.names boolean
#' @param col.names boolean
#' 
#' @return object
#'
#' @examples
#' \dontrun{
#' write.excel(dat)
#' }
#' @export
###############################################################################

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

###############################################################################
#' Reverse First and Last Name
#'
#' When there is a "last, first" name or "last first" name string, you can 
#' reverse the string to look like "first last" name with this function.
#'
#' @param x string
#' @param p boolean
#' 
#' @return string
#'
#' @examples
#' \dontrun{
#' x = "Ruth, Babe"
#' reverse_name(x)
#' }
#' @export
###############################################################################

reverse_name <- function(x, p="TRUE"){
  require(stringr); require(Hmisc)
  if (p == "TRUE"){
    #x = str_replace_all(x, "[^[:print:]]", " ")
    x = str_replace_all(x, "[^a-zA-Z.-]", " ") #keep periods and hyphens
    la = capitalize(substr(str_trim(x, side = "both"),1,
                           str_locate(x, " ")-1))
    fi = capitalize(substr(str_trim(x, side = "both"),
                           str_locate(x, " ")+1,str_length(x)))
    out = str_trim(paste(fi,la), side = "both")
  } else {
    x = str_replace_all(x, "[^a-zA-Z]", " ") #leave out all punctuation
    la = capitalize(substr(str_trim(x, side = "both"),1,
                           str_locate(x, " ")-1))
    fi = capitalize(substr(str_trim(x, side = "both"),
                           str_locate(x, " ")+1,str_length(x)))
    out = str_trim(paste(fi,la), side = "both")
  }
  return(out)
}

###############################################################################
#' Peaks
#'
#' This function takes a time series and returns a TRUE or FALSE in finding
#' the peaks that exist in the series.
#'
#' @param x time series
#' @param span function
#' 
#' @return boolean vector
#'
#' @examples
#' \dontrun{
#' dat<-runif(10000,0,1)
#' peaks(dat)
#' }
#' @export
###############################################################################

peaks<-function(x, span = 3) { 
  z <- embed(series, span) 
  s <- span%/%2
  v<- max.col(z) == 1 + s 
  result <- c(rep(FALSE, s), v) 
  return(result[1:(length(result) - s)])
}

###############################################################################
#' Summarize content
#'
#' This function takes the R object on which we need to obtain statistics (x),
#' how many entries should each summary contain (step, defaulting to 1000),
#' and the function we want to apply (fun, defaulting to mean).
#'
#' @param x object
#' @param step number of entries
#' @param fun function
#' 
#' @return vector
#'
#' @examples
#' \dontrun{
#' dat<-data.frame(matrix(runif(100000,0,1),ncol=10))
#' summarize.by(dat)
#' }
#' @export
###############################################################################

summarize.by<-function(x,step=1000,fun="mean")
{
  
  if(is.data.frame(x))
  {
    group<-sort(rep(seq(1,ceiling(nrow(x)/step)),step)[1:nrow(x)])
  }
  if(is.vector(x))
  {
    group<-sort(rep(seq(1,ceiling(length(x)/step)),step)[1:length(x)])
  }
  x<-data.frame(group,x)
  x<-aggregate(x,by=list(x$group),FUN=fun)
  x<-x[,-c(1,2)]
  return(x)
}

###############################################################################
#' Age Years
#'
#' This function for calculating age with two dates
#'
#' @param earlier first date
#' @param later second date
#' 
#' @return integer
#'
#' @examples
#' \dontrun{ 
#' x <- as.Date("2000-02-29")
#' y <- as.Date("2004-02-28")
#' age_years(x, y)
#' }
#' @export
###############################################################################

age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}

###############################################################################
#' Between
#'
#' This function mimics the SQL between clause and returns a logical index
#'
#' @param x numeric
#' @param low numeric
#' @param high numeric
#' @param ineq boolean
#' 
#' @return logical
#'
#' @examples
#' \dontrun{ 
#' between( 1:10, 5.5, 6.5 )
#' }
#' @export
###############################################################################

between <- function(x, low, high, ineq=F) {
  if (ineq) {
    x >= low & x <= high
  } else {
    x > low & x < high
  }
}

###############################################################################
#' Buy/Sell Indicator
#'
#' This function will provide one buy and one sell indicator for the given
#' series
#'
#' @param x numeric
#'
#' @return numeric
#'
#' @examples
#' \dontrun{ 
#' buy.sell(x)
#' }
#' @export 
###############################################################################

buy.sell <- function(x){
  buy = x[which.min(x)]
  sell = x[which.max(x)] 
  z=x
   for (i in 1:NROW(x)){
     if (i == which.min(x)) {z[i,1] = 100
     }
     if (i == which.max(x)) {z[i,1] = -100
     }
     if (i != which.min(x) & i != which.max(x)) {z[i,1] = 0
     }
   }
  out = list(z,buy,sell)
  names(out)[1] = "Buy.Sell"
  names(out)[2] = "Buy.Price"
  names(out)[3] = "Sell.Price"
  return(out)
}

###############################################################################
#' Unadjusted R Squared
#'
#' This function will calculate unadjusted R squared value which explains
#' the model more accurately, because the intercept is factored back into the 
#' model
#'
#' @param fit lm
#'
#' @return numeric
#'
#' @examples
#' \dontrun{ 
#' unadj.rsquared(fit)
#' }
#' @export 
###############################################################################

unadj.rsquared <- function(fit){
  rc = summary(fit)$r.squared
  n = NROW(fit$fitted.values)
  ybar = mean(fit$model[,1])
  n_ybar2 = n*ybar^2
  sumsq_y = sum(fit$model[,1]^2)
  k = n_ybar2/sumsq_y
  ru = rc*(1-k)+k
  out = list(ru)
  names(out)[1] = "unadj.rsquared"
  return(out)
}

###############################################################################
#' Seasonal Averages
#'
#' This function will calculate the seasonal averages of a time series
#' based on the frequency (average of N rows)
#'
#' @param x dataset
#' @param frequency how often
#'
#' @return vector
#'
#' @examples
#' \dontrun{ 
#' seasonals(x,12)
#' }
#' @export 
###############################################################################

seasonals <- function(x,frequency){
  return(rowMeans(matrix(x,frequency))/mean(x))
}

###############################################################################
#' Normalization
#'
#' This function will normalize a vector
#'
#' @param x dataset
#'
#' @return numeric
#'
#' @examples
#' \dontrun{ 
#' normalize(x)
#' }
#' @export 
###############################################################################

normalize <- function(x){
  minV = min(x)
  maxV = max(x)
  for (i in 1:NROW(x)) {
    x[i] = ((x[i]-minV)/(maxV-minV))
  }
  return(x)
}

###############################################################################
#' Absolute Average Accuracy
#'
#' This function will calculate the absolute average accuracy of compartive datasets
#'
#' @param y dataset 1
#' @param x dataset 2
#'
#' @return numeric
#'
#' @examples
#' \dontrun{ 
#' acc(y,x)
#' }
#' @export 
###############################################################################
acc <- function (y, x) { 
  return(mean(1-abs((y-x)/y)));
}

###############################################################################
###############################################################################
# Collection of General Utilities
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Convenience Utilities
###############################################################################
#' Split string into tokens using delim
#'
#' This function will split given string into tokens using delim
#'
#' @param s input string
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return array of tokens
#'
#' @examples
#' \dontrun{ 
#' spl('a,b,c')
#' }
#' @export 
###############################################################################
spl <- function 
(
  s,			# input string
  delim = ','	# delimiter
)
{ 
  return(unlist(strsplit(s,delim))); 
}

###############################################################################
#' Join vector of strings into one string using delim
#'
#' This function will join vector of strings into one string using delim
#'
#' @param v vector of strings
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return resulting string
#'
#' @examples
#' \dontrun{ 
#' join(c('a','b','c'), ',')
#' }
#' @export 
###############################################################################
join <- function
(
  v, 			# vector of strings
  delim = ''	# delimiter
)
{ 
  return(paste(v,collapse=delim)); 
}

###############################################################################
#' Remnove any leading and trailing spaces
#'
#' This function will remnove any leading and trailing spaces
#'
#' @param s string
#'
#' @return resulting string
#'
#' @examples
#' \dontrun{ 
#' trim('  a b c  ')
#' }
#' @export 
###############################################################################
trim <- function
(
  s	# string
)
{
  s = sub(pattern = '^ +', replacement = '', x = s)
  s = sub(pattern = ' +$', replacement = '', x = s)
  return(s)
}

###############################################################################
#' Shortcut for length function
#'
#' This function is a shortcut for length function
#'
#' @param x vector / string / matrix
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{ 
#' len(1:10)
#' }
#' @export 
###############################################################################
len <- function
(
  x	# vector
)
{
  return(length(x)) 
}

###############################################################################
#' Faster version of ifelse function
#'
#' This function is a faster version of ifelse function
#'
#' @param cond true / false condition
#' @param truepart resulting value(s) if condition is true
#' @param falsepart resulting value(s) if condition is false
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{ 
#' iif(1:10 > 5, 1, 1:10)
#' }
#' @export 
###############################################################################
iif <- function
(
  cond,		# condition
  truepart,	# true part
  falsepart	# false part
)
{
  if(len(cond) == 1) { if(cond) truepart else falsepart }
  else {  
    if(length(falsepart) == 1) {
      temp = falsepart
      falsepart = cond
      falsepart[] = temp
    }
    
    if(length(truepart) == 1) 
      falsepart[cond] = truepart 
    else {
      cond = ifna(cond,F)
      falsepart[cond] = truepart[cond]
    }
    
    #falsepart[!is.na(cond)] = temp
    
    return(falsepart);
  }
} 

###############################################################################
#' Replace NA, NaN, Inf values
#'
#' This function will replace all NA, NaN, Inf with given values
#'
#' @param x data to check for NA, NaN, Inf
#' @param y values(s) to be used in place of NA, NaN, Inf
#'
#' @return updated data
#'
#' @examples
#' \dontrun{ 
#' ifna(c(1,NA,2,Inf,3), 4)
#' }
#' @export 
###############################################################################
ifna <- function
(
  x,	# check x for NA, NaN, Inf
  y	# if found replace with y
) { 	
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}


###############################################################################
#' Replace NULL values
#'
#' This function will replace all NULL with given value
#'
#' @param x data to check for NULL
#' @param y values to be used in place of NULL
#'
#' @return updated data
#'
#' @examples
#' \dontrun{ 
#' temp = list()
#' temp$val1 = ifnull(temp$val1, 4)
#' }
#' @export 
############################################################################### 
ifnull <- function
(
  x,	# check x for NULL
  y	# if found replace with y
) { 	
  return(iif(is.null(x), y, x))
}



###############################################################################
#' Faster version of rep fucntion
#'
#' This function is a faster version of rep fucntion
#'
#' @param x data to be repeated
#' @param times number of times to repeat the data
#'
#' @return new data
#'
#' @examples
#' \dontrun{ 
#' fast.rep(c(1,NA,2,Inf,3), 4)
#' }
#' @export 
###############################################################################
fast.rep <- function(x, times) { 
  length(x) = times
  x[] = x[1]		
  x
}

###############################################################################
#' Count number of non NA elements
#'
#' This function will count number of non NA elements in the given matrix
#'
#' @param x data matrix
#' @param side margin along which to count
#'
#' @return counts
#'
#' @examples
#' \dontrun{ 
#' count(matrix(c(1,NA,2,3),2,2))
#' }
#' @export 
###############################################################################
count <- function(
  x,			# matrix with data
  side = 2	# margin along which to count
)
{
  if( is.null(dim(x)) ) { 
    sum( !is.na(x) ) 
  } else { 
    apply(!is.na(x), side, sum) 
  }
}  

###############################################################################
#' Running Count over given window
#'
#' This function will count number of non NA elements over given window
#'
#' @param x data matrix
#' @param window.len window length
#'
#' @return counts
#'
#' @examples
#' \dontrun{ 
#' run.count(matrix(1:9,3,3),2)
#' }
#' @export 
###############################################################################
run.count <- function
(
  x, 			# vector with data
  window.len	# window length
)
{ 
  n    = length(x) 
  xcount = cumsum( !is.na(x) )
  ycount = xcount[-c(1 : (k-1))] - c(0, xcount[-c((n-k+1) : n)])
  return( c( xcount[1:(k-1)], ycount))
}

###############################################################################
#' Dates Functions
#'
#' @param dates collection of dates
#'
#' @return transformed dates
#'
#' @examples
#' \dontrun{ 
#' date.dayofweek(Sys.Date())
#' }
#' @export 
#' @rdname DateFunctions
###############################################################################
date.dayofweek <- function(dates) 
{	
  return(as.double(format(dates, '%w')))
}

#' @export 
#' @rdname DateFunctions
date.day <- function(dates) 
{	
  return(as.double(format(dates, '%d')))
}

#' @export 
#' @rdname DateFunctions
date.week <- function(dates) 
{	
  return(as.double(format(dates, '%U')))
}

#' @export 
#' @rdname DateFunctions
date.month <- function(dates) 
{	
  return(as.double(format(dates, '%m')))
}

#' @export 
#' @rdname DateFunctions
date.year <- function(dates) 
{	
  return(as.double(format(dates, '%Y')))
}


###############################################################################
#' Dates Index Functions
#'
#' @param dates collection of dates
#'
#' @return location of the week/month/year ends
#'
#' @examples
#' \dontrun{ 
#' date.week.ends(seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
#' @rdname DateFunctionsIndex
###############################################################################
date.week.ends <- function(dates) 
{	
  return( unique(c(which(diff( 100*date.year(dates) + date.week(dates) ) != 0), len(dates))) )
}

#' @export 
#' @rdname DateFunctionsIndex
date.month.ends <- function(dates) 
{	
  return( unique(c(which(diff( 100*date.year(dates) + date.month(dates) ) != 0), len(dates))) )
}

#' @export 
#' @rdname DateFunctionsIndex
date.year.ends <- function(dates) 
{	
  return( unique(c(which(diff( date.year(dates) ) != 0), len(dates))) )
}


###############################################################################
#' Map given time series to monthly
#'
#' If frequency of observations in the given time series is less than monthly,
#' i.e. quaterly or annually, properly align this time series to monthly
#'
#' @param equity time series
#'
#' @return xts object 
#'
#' @examples
#' \dontrun{ 
#' map2monthly(equity) 
#' }
#' @export 
###############################################################################
map2monthly <- function(equity) 
{
  #a = coredata(Cl(to.monthly(equal.weight$equity)))
  
  if(compute.annual.factor(equity) >= 12) return(equity)
  
  dates = index(equity)
  equity = coredata(equity)
  
  temp = as.Date(c('', 10000*date.year(dates) + 100*date.month(dates) + 1), '%Y%m%d')[-1]
  new.dates = seq(temp[1], last(temp), by = 'month')		
  
  map = match( 100*date.year(dates) + date.month(dates), 100*date.year(new.dates) + date.month(new.dates) ) 
  temp = rep(NA, len(new.dates))
  temp[map] = equity
  
  #range(a - temp)
  
  return( make.xts( ifna.prev(temp), new.dates) )
}


###############################################################################
#' Create monthly table
#'
#' Transform given monthly time series into matrix with Months as columns and Years as rows
#'
#' @param monthly.data monthly time series
#'
#' @return matrix with Months as columns and Years as rows
#'
#' @examples
#' \dontrun{ 
#' create.monthly.table(monthly.ret)
#' }
#' @export 
###############################################################################
create.monthly.table <- function(monthly.data) 
{
  nperiods = nrow(monthly.data)
  
  years = date.year(index(monthly.data[c(1,nperiods)]))
  years = years[1] : years[2]
  
  # create monthly matrix
  temp = matrix( double(), len(years), 12)
  rownames(temp) = years
  colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec')
  
  # align months
  index = date.month(index(monthly.data[c(1,nperiods)]))
  temp[] = matrix( c( rep(NA, index[1]-1), monthly.data, rep(NA, 12-index[2]) ), ncol=12, byrow = T)
  
  return(temp)
}


###############################################################################
#' Compute the expiration date of stock options (3rd Friday of the month)
#'
#' @param year year
#' @param month month
#'
#' @return date for the third Friday of the given month and year
#'
#' @references 
#' \url{http://bytes.com/topic/python/answers/161147-find-day-week-month-year}
#'
#' \url{http://www.mysmp.com/options/options-expiration-week.html}
#' The week beginning on Monday prior to the Saturday of options expiration is referred to as options expiration week. 
#' Since the markets are closed on Saturday, the third Friday of each month represents options expiration.
#' If the third Friday of the month is a holiday, all trading dates are moved forward; meaning that Thursday will be the last trading day to exercise options.
#'
#' \url{http://www.cboe.com/TradTool/ExpirationCalendar.aspx}
#'
#' @examples
#' \dontrun{ 
#' third.friday.month(2012,1)
#' }
#' @export 
###############################################################################
third.friday.month <- function(year, month)
{
  day = date.dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
  day = c(20,19,18,17,16,15,21)[1 + day]
  return(as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1])
}


###############################################################################
#' Determine the index of subset of dates in the time series
#'
#' @param x xts time series
#' @param dates string represnting subset of dates i.e. '2010::2012'
#'
#' @return index of subset of dates in the time series
#'
#' @examples
#' \dontrun{ 
#' dates2index(data$prices, '2010::2012') 
#' }
#' @export 
###############################################################################
dates2index <- function( x, dates = 1:nrow(x) ) {
  dates.index = dates
  if(!is.numeric(dates)) {
    temp = x[,1]
    temp[] = 1:nrow(temp)
    dates.index = as.numeric(temp[dates])
  }
  return(dates.index)
} 


###############################################################################
#' Load Packages that are available and install ones that are not available
#'
#' This function a convience wrapper for install.packages() function
#'
#' @param packages names of the packages separated by comma
#' @param repos default repository
#' @param dependencies type of dependencies to install
#' @param ... additional parameters for the \code{\link{install.packages}} function
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' load.packages('quantmod')
#' }
#' @export 
############################################################################### 
load.packages <- function
(
  packages, 							# names of the packages separated by comma
  repos = "http://cran.r-project.org",# default repository
  dependencies = c("Depends", "Imports"),	# install dependencies
  ...									# other parameters to install.packages
)
{
  packages = spl(packages)
  for( ipackage in packages ) {
    if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
      install.packages(ipackage, repos=repos, dependencies=dependencies, ...) 
      
      if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
        stop("package", sQuote(ipackage), 'is needed.  Stopping')
      }
    }
  }
}


###############################################################################
#' Begin Timing
#'
#' @param identifier name for this timing session
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' tic(1)
#' }
#' @export 
#' @rdname TimingFunctions
############################################################################### 
tic <- function
(
  identifier	# integer value
)
{
  assign(paste('saved.time', identifier, sep=''), proc.time()[3], envir = .GlobalEnv)
}


###############################################################################
#' End Timing and report elapsed time
#'
#' @param identifier name for this timing session
#'
#' @return elapsed time
#'
#' @examples
#' \dontrun{ 
#' toc(1)
#' }
#' @export 
#' @rdname TimingFunctions
############################################################################### 
toc <- function
(
  identifier	# integer value
)
{
  if( exists(paste('saved.time', identifier, sep=''), envir = .GlobalEnv) ) {
    prevTime = get(paste('saved.time', identifier, sep=''), envir = .GlobalEnv)
    diffTimeSecs = proc.time()[3] - prevTime
    cat('Elapsed time is', round(diffTimeSecs, 2), 'seconds\n')
  } else {
    cat('Toc error\n')
  }    
  return (paste('Elapsed time is', round(diffTimeSecs,2), 'seconds', sep=' '))
}

test.tic.toc <- function()
{
  tic(10)
  for( i in 1 : 100 ) {
    temp = runif(100)
  }
  toc(10)
}







###############################################################################
#' Lag matrix or vector
#'
#' This function shifts elemnts in a vector or a mtrix by a given lag.
#' For example: mlag(x,1) - use yesterday's values and
#'  mlag(x,-1) - use tomorrow's values
#'
#' @param x vector / matrix
#' @param nlag number of lags, \strong{defaults to 1}
#'
#' @return modified object
#'
#' @examples
#' \dontrun{ 
#' mlag(1:10)
#' }
#' @export 
###############################################################################
mlag <- function
(
  m,			# matrix or vector
  nlag = 1	# number of lags
)
{ 
  # vector
  if( is.null(dim(m)) ) { 
    n = len(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    } 	
    
    # matrix	
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    } 
  }
  return(m);
}


###############################################################################
#' Replicate and tile a given vector
#'
#' @param v vector
#' @param n number of copies along rows
#' @param m number of copies along columns
#'
#' @return new matrix
#' 
#' @references 
#' \url{http://www.mathworks.com/help/techdoc/ref/repmat.html}
#'
#' @examples
#' \dontrun{ 
#' repmat(1:10,1,2)
#' }
#' @export 
###############################################################################
repmat <- function
(
  v,	# vector
  n,	# number of copies along rows
  m	# number of copies along columns
)
{
  kronecker( matrix(1, n, m), v )
}


###############################################################################
#' Repeat Rows
#'
#' @param m vector (row)
#' @param nr number of copies along rows
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' matrix(1:3, nr=5, nc=3, byrow=T)
#' repRow(1:3, 5)
#' }
#' @export 
###############################################################################
repRow <- function
(
  m, # vector (row)
  nr # number of copies along rows
)
{
  matrix(m, nr=nr, nc=len(m), byrow=T)
}


###############################################################################
#' Repeat Rows
#'
#' @param m vector (column)
#' @param nc number of copies along columns
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' matrix(1:5, nr=5, nc=3, byrow=F)
#' repCol(1:5, 3)
#' }
#' @export 
###############################################################################
repCol <- function
(
  m,	# vector (column)
  nc	# number of copies along columns
)
{
  matrix(m, nr=len(m), nc=nc, byrow=F)
}




###############################################################################
#' Find location: row, col in the matrix, given index of of observation
#'
#' @param data matrix
#' @param i index of observations
#' @param details flag to provide details, \strong{defaults to FALSE}
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' data = matrix(1:16,4,4)
#' lookup.index(data, which(data > 4))
#' }
#' @export 
# play with following example: update 1 %% 4	
###############################################################################
lookup.index <- function
(
  data, 	# matrix
  i, 		# index of observations
  details = F	# flag to return additional details
) 
{
  n = nrow(data)
  irow = ((i - 1) %% n) + 1	
  icol = ((i - 1) %/% n) +1 
  if(details)
    list(irow=irow,icol=icol,obs=data[irow,icol],obsr=data[max(0,irow-5):min(nrow(data),irow+5),icol])
  else
    list(irow=irow,icol=icol)
}


###############################################################################
#' Convert beta (slope of reggression line) to degrees
#'
#' @param beta slope of regression line
#'
#' @return angle
#' 
#' @references 
#' \url{http://r.789695.n4.nabble.com/slope-calculation-td858652.html	}
#'
#' @examples
#' \dontrun{ 
#' beta.degree(1)
#' }
#' @export 
###############################################################################
beta.degree <- function(beta) 
{ 
  atan(beta)*360/(2*pi) 
}


###############################################################################
# XTS helper functions
###############################################################################

# must set timezone before any calls to xts
Sys.setenv(TZ = 'GMT')
#Sys.setenv(TZ = 'EST')

###############################################################################
#' The timezone is set to 'GMT' by defult
#'
#' The reason for setting the default timezone is because the following code 
#' produces different results if the timezone is NOT set and if timezone has a value.
#' 
#' @examples
#' \dontrun{ 
# 
#' # We want to set the timezone, so that following code produces expected results
#' Sys.getenv('TZ')
#' test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
#'	as.numeric(test)
#'	as.numeric(as.POSIXct(as.Date(test)))
#' as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
#' test == as.POSIXct(as.Date(test))
#'
#' # Set Time Zone
#' Sys.setenv(TZ = 'GMT')
#' Sys.getenv('TZ')
#' test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
#'	as.numeric(test)
#'	as.numeric(as.POSIXct(as.Date(test)))
#' as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
#' test == as.POSIXct(as.Date(test))
#'
#' }
#' @export 
#' @rdname XTSFunctions
###############################################################################
XTSFunctions <- function() {}


###############################################################################
#' Create \code{\link{xts}} object, faster version of \code{\link{xts}} fucntion
#'
#' @param x vector / matrix / data frame
#' @param order.by dates that correspond to rows of x
#'
#' @return \code{\link{xts}} object
#' 
#' @examples
#' \dontrun{ 
#' make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
###############################################################################
make.xts <- function
(
  x,			# data
  order.by	# date
)
{
  #Sys.setenv(TZ = 'GMT')
  tzone = Sys.getenv('TZ')
  
  orderBy = class(order.by)
  index = as.numeric(as.POSIXct(order.by, tz = tzone))
  if( is.null(dim(x)) ) dim(x) = c(len(x), 1)
  x = as.matrix(x)
  
  x = structure(.Data = x, 
                index = structure(index, tzone = tzone, tclass = orderBy), 
                class = c('xts', 'zoo'), .indexCLASS = orderBy, tclass=orderBy, .indexTZ = tzone, tzone=tzone)
  return( x )
}


###############################################################################
#' Write \code{\link{xts}} object to file
#'
#' @param x \code{\link{xts}} object
#' @param filename file name
#' @param append flag to inidicate if file is overwritten or appended, \strong{defaults to FALSE}
#' @param ... additional paramaeters to the \code{\link{format}} function
#'
#' @return nothing
#' 
#' @examples
#' \dontrun{ 
#' write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
#' }
#' @export 
###############################################################################
write.xts <- function
(
  x,			# XTS object
  filename,	# file name
  append = FALSE,	
  ...
)
{
  cat('Date', file = filename, append = append)
  
  write.table(x, sep=',',  row.names = format(index(x), ...), 
              col.names = NA, file = filename, append = T, quote = F)
  #write.csv(x, row.names = format(index(x)), filename)	
}


###############################################################################
#' Read \code{\link{xts}} object from file
#'
#' @param filename file name
#' @param date.fn function to preprocess string dates, \strong{defaults to \code{\link{paste}} - i.e. no preprocessing}
#' @param index.class class of the date object, \strong{defaults to 'Date'}
#' @param ... additional paramaeters to the \code{\link{as.POSIXct}} function
#'
#' @return \code{\link{xts}} object
#' 
#' @examples
#' \dontrun{ 
#' write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
#' read.xts('temp.csv')
#' }
#' @export 
###############################################################################
read.xts <- function
(
  filename,	# file name
  date.fn = paste,
  index.class = 'Date',
  ...
)
{
  out = read.csv(filename, stringsAsFactors=F)
  #return( make.xts(out[,-1,drop=F], as.Date(out[,1], ...)) )
  out = make.xts(out[,-1,drop=F], as.POSIXct(match.fun(date.fn)(out[,1]), tz = Sys.getenv('TZ'), ...))
  indexClass(out) = index.class
  return( out )
  
  # Example code from	getSymbols.yahoo (quantmod): as.POSIXct is used to avoid Dates conversion problems
  # fr = xts(1, as.POSIXct('2012-10-31', tz = Sys.getenv("TZ"), format='%Y-%m-%d'),  src = "yahoo", updated = Sys.time())
  # indexClass(fr) = "Date"	
}


###############################################################################
#' Fast alternative to index() function for \code{\link{xts}} object
#'
#' NOTE index.xts is the same name as the index function in the XTS package
#'
#' @param x \code{\link{xts}} object
#'
#' @return dates
#' 
#' @examples
#' \dontrun{ 
#' index.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)))
#' }
#' @export 
###############################################################################
# maybe rename to bt.index.xts
index.xts <- function
(
  x			# XTS object
)
{
  temp = attr(x, 'index')
  class(temp) = c('POSIXct', 'POSIXt')
  
  type = attr(x, '.indexCLASS')[1]
  if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
    temp = as.Date(temp)
  return(temp)
}


# other variants that are not currently used
index4xts <- function
(
  x			# XTS object
)
{
  temp = attr(x, 'index')
  class(temp)='POSIXct' 
  
  return(temp)
}

index2date.time <- function(temp) {
  class(temp)='POSIXct' 
  
  if( attr(x, '.indexCLASS')[1] == 'Date') {	
    as.Date(temp)
  } else {
    as.POSIXct(temp, tz = Sys.getenv('TZ'))
  }
}


###############################################################################
#' File name Functions
#'
#' @param x file name
#'
#' @return part of the file name
#'
#' @examples
#' \dontrun{ 
#' get.extension('test.csv')
#' }
#' @export 
#' @rdname FilenameFunctions
###############################################################################
get.extension <- function(x) 
{ 
  trim( tail(spl(x,'\\.'),1) ) 
}	

#' @export 
#' @rdname FilenameFunctions
get.full.filename <- function(x) 
{ 
  trim( tail(spl(gsub('\\\\','/',x),'/'),1) ) 
}

#' @export 
#' @rdname FilenameFunctions
get.filename <- function(x) 
{ 
  temp = spl(get.full.filename(x),'\\.')
  join(temp[-len(temp)])
}


###############################################################################
#' Helper function to read historical stock prices saved by Seasonality tool
#'
#' @param Symbols vector of symbols
#' @param env enviroment to store prices, \strong{defaults to .GlobalEnv}
#' @param auto.assign flag to auto assign symbols, \strong{defaults to TRUE}
#' @param stock.folder stock folder, \strong{defaults to 'c:/temp/Seasonality/stocks'}
#' @param stock.date.format stock date format, \strong{defaults to '\%Y-\%m-\%d'}
#' @param ... other parameters for getSymbols function
#'
#' @return nothing is auto.assign = TRUE, prices are stored in the env enviroment
#' if auto.assign = FALSE, returns first symbol
#' 
#' @references 
#' \url{http://stackoverflow.com/questions/8970823/how-to-load-csv-data-file-into-r-for-use-with-quantmod}
#'
#' @examples
#' \dontrun{ 
#' data <- new.env()
#' getSymbols.sit(spl('SPY,IBM'), env = data, auto.assign = T)
#' }
#' @export
######################################################################x#########
getSymbols.sit <- function
(
  Symbols, 
  env = .GlobalEnv, 
  auto.assign = TRUE, 
  stock.folder = 'c:/temp/Seasonality/stocks',
  stock.date.format = '%Y-%m-%d',
  ...
) 
{
  require(quantmod)	
  
  # http://stackoverflow.com/questions/8970823/how-to-load-csv-data-file-into-r-for-use-with-quantmod
  for(i in 1:len(Symbols)) {
    s = Symbols[i]
    
    temp = list()
    temp[[ s ]] = list(src='csv', format=stock.date.format, dir=stock.folder)
    setSymbolLookup(temp)
    
    temp = quantmod::getSymbols(s, env = env, auto.assign = auto.assign)		
    if (!auto.assign) {
      cat(s, format(range(index(temp)), '%d-%b-%Y'), '\n', sep='\t')	
      return(temp)
    }
    if(!is.null(env[[ s ]]))
      cat(i, 'out of', len(Symbols), 'Reading', s, format(range(index(env[[ s ]])), '%d-%b-%Y'), '\n', sep='\t')	
    else
      cat(i, 'out of', len(Symbols), 'Missing', s, '\n', sep='\t')	
  }
}