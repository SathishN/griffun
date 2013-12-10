###############################################################################
# Backtest Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
#' Align dates, faster version of merge function
#'
#' @param b enviroment with symbols time series
#' @param align alignment type
#' @param dates subset of dates
#' 
#' @return array of list of dates
#'
#' @examples
#' \dontrun{ 
#' bt.merge(b, align, dates)
#' }
#' @export 
###############################################################################

bt.merge <- function
(
  b,				# enviroment with symbols time series
  align = c('keep.all', 'remove.na'),	# alignment type
  dates = NULL	# subset of dates
) 
{
  align = align[1]
  symbolnames = b$symbolnames
  nsymbols = len(symbolnames) 
  
  # count all series
  ncount = sapply(symbolnames, function(i) nrow(b[[i]]))
  all.dates = double(sum(ncount))		
  
  # put all dates into one large vector
  itemp = 1
  for( i in 1:nsymbols ) {
    all.dates[itemp : (itemp + ncount[i] -1)] = attr(b[[ symbolnames[i] ]], 'index')
    itemp = itemp + ncount[i]
  }
  
  # find unique
  temp = sort(all.dates)
  unique.dates = c(temp[1], temp[-1][diff(temp)!=0])
  
  # trim if date is supplied	
  if(!is.null(dates)) { 
    class(unique.dates) = c('POSIXct', 'POSIXt')	
    temp = make.xts(integer(len(unique.dates)), unique.dates) 		
    unique.dates = attr(temp[dates], 'index')
  }
  
  # date map
  date.map = matrix(NA, nr = len(unique.dates), nsymbols)
  itemp = 1
  for( i in 1:nsymbols ) {
    index = match(all.dates[itemp : (itemp + ncount[i] -1)], unique.dates)
    sub.index = which(!is.na(index))
    date.map[ index[sub.index], i] = sub.index
    itemp = itemp + ncount[i]
  }
  
  # trim logic
  if( align == 'remove.na' ) { 
    index = which(count(date.map, side=1) < nsymbols )
  } else {
    index = which(count(date.map, side=1) < max(1, 0.1 * nsymbols) )
  }
  
  if(len(index) > 0) { 
    date.map = date.map[-index,, drop = FALSE]
    unique.dates = unique.dates[-index] 
  }
  
  class(unique.dates) = c('POSIXct', 'POSIXt')	
  return( list(all.dates = unique.dates, date.map = date.map))
}
###############################################################################
#' Find location of given names in all names
#'
#' @param find.names find names of columns
#' @param all.names all names
#'
#' @return nothing
#' 
#' @examples
#' \dontrun{ 
#' find.names('Close,Volume', colnames(b[[ symbolnames[i] ]]))
#' }
#' @export
###############################################################################
find.names <- function(find.names, all.names) 
{ 
  as.list(sapply(spl(find.names), function(x) {
    loc = grep(x, all.names, ignore.case = TRUE)
    iif(len(loc) > 0, loc, NA)
  }))
}
###############################################################################
#' Prepare backtest data
#'
#' @param b enviroment with symbols time series
#' @param align alignment type
#' @param dates subset of dates
#' @param F fill gaps introduced by merging
#'
#' @return nothing
#'
#' @examples
#' \dontrun{
#' bt.prep(data, align='keep.all', dates='1970::2011')
#' }
#' @export 
###############################################################################
bt.prep <- function
(
  b,				# enviroment with symbols time series
  align = c('keep.all', 'remove.na'),	# alignment type
  dates = NULL,	# subset of dates
  fill.gaps = F	# fill gaps introduced by merging
) 
{
  # setup
  if( !exists('symbolnames', b, inherits = F) ) b$symbolnames = ls(b)
  symbolnames = b$symbolnames
  nsymbols = len(symbolnames) 
  
  if( nsymbols > 1 ) {
    # merge
    out = bt.merge(b, align, dates)
    
    for( i in 1:nsymbols ) {
      b[[ symbolnames[i] ]] = 
        make.xts( coredata( b[[ symbolnames[i] ]] )[ out$date.map[,i],, drop = FALSE], out$all.dates)
      
      # fill gaps logic
      map.col = find.names('Close,Volume', colnames(b[[ symbolnames[i] ]]))
      if(fill.gaps & !is.na(map.col$Close)) {	
        close = coredata(b[[ symbolnames[i] ]][,map.col$Close])
        n = len(close)
        last.n = max(which(!is.na(close)))
        close = ifna.prev(close)
        if(last.n + 5 < n) close[last.n : n] = NA
        b[[ symbolnames[i] ]][, map.col$Close] = close
        index = !is.na(close)	
        
        if(!is.na(map.col$Volume)) {
          index1 = is.na(b[[ symbolnames[i] ]][, map.col$Volume]) & index
          b[[ symbolnames[i] ]][index1, map.col$Volume] = 0
        }
        
        for(j in colnames(b[[ symbolnames[i] ]])) {
          index1 = is.na(b[[ symbolnames[i] ]][,j]) & index
          b[[ symbolnames[i] ]][index1, j] = close[index1]
        }						
      }
    }	
  } else {
    if(!is.null(dates)) b[[ symbolnames[1] ]] = b[[ symbolnames[1] ]][dates,]	
    out = list(all.dates = index.xts(b[[ symbolnames[1] ]]) )
  }
  
  # dates
  b$dates = out$all.dates
  
  # empty matrix		
  dummy.mat = matrix(double(), len(out$all.dates), nsymbols)
  colnames(dummy.mat) = symbolnames
  dummy.mat = make.xts(dummy.mat, out$all.dates)
  
  # weight matrix holds signal and weight information		
  b$weight = dummy.mat
  
  # execution price, if null use Close	
  b$execution.price = dummy.mat
  
  # populate prices matrix
  for( i in 1:nsymbols ) {
    if( has.Cl( b[[ symbolnames[i] ]] ) ) {
      dummy.mat[,i] = Cl( b[[ symbolnames[i] ]] );
    }
  }
  b$prices = dummy.mat	
}