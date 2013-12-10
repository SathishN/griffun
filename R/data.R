###############################################################################
# Collection of routines to work with data
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################

###############################################################################
#' Extract.table.from.webpage
#'
#' @param txt source text of webpage
#' @param marker key-phrase(s) located in the table to extract
#' @param hasHeader flag if table has a header
#' 
#' @return temp
#' 
#' @examples
#' \dontrun{ 
#' url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
#' txt = join(readLines(url))
#' temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
#' }
#' @export
###############################################################################
extract.table.from.webpage <- function
(
  txt, 		# source text of webpage
  marker,		# key-phrase(s) located in the table to extract
  hasHeader=T	# flag if table has a header
)
{
  tryCatch({		
    # find location of data
    marker = spl(marker)
    pos1=1
    
    for(i in 1:len(marker)) {
      pos1 = regexpr(marker[i], substr(txt, pos1, nchar(txt))) + pos1
    }
    
    # find start/end of table
    pos0 = tail(gregexpr('<table', substr(txt, 1, pos1))[[1]], 1)
    pos2 = head(gregexpr('</table', substr(txt, pos1, nchar(txt)))[[1]], 1)
    temp =  substr(txt, pos0, pos1 + pos2 - 2)
    
    # remove all formating	
    temp = gsub(pattern = '<br>', replacement = '', temp, perl = TRUE) 
    
    temp = gsub(pattern = '</tr>', replacement = ';row;', temp, perl = TRUE) 
    temp = gsub(pattern = '</td>', replacement = ';col;', temp, perl = TRUE) 
    temp = gsub(pattern = '</th>', replacement = ';col;', temp, perl = TRUE) 
    
    temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE) 
    
    temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE) 		
    
    # parse into matrix	
    temp = lapply( strsplit(temp, ';row;'), strsplit, ';col;')	
    n = max( sapply(temp[[1]], function(x) len(x)) )
    temp = t( sapply(temp[[1]], function(x) x[1:n]) )
    
    if(hasHeader) {
      colnames(temp) = temp[(hasHeader + 0), ]
      temp = temp[-c(1:(hasHeader + 0)), ,drop=F]
    }
    
  }, error = function(ex) {
    temp <<- txt
  }, finally = {
    return(temp)
  })
}

###############################################################################
#' Get Dow Jones Components
#'
#' @return tickers
#' 
#' @examples
#' \dontrun{ 
#' dow.jones.components()
#' }
#' @export
###############################################################################
dow.jones.components <- function()
{
  url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
  txt = join(readLines(url))
  
  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
  tickers = temp[, 'Symbol']
  
  return(tickers)
}

###############################################################################
#' Get NASDAQ 100 Components
#' 
#' @examples
#' \dontrun{ 
#' nasdaq.100.components()
#' }
#' @export
###############################################################################
nasdaq.100.components <- function()
{
  url = 'http://www.nasdaq.com/markets/indices/nasdaq-100.aspx'
  txt = join(readLines(url))
  
  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = 2)
  tickers = temp[, 'Symbol']
  
  return(tickers)
}


###############################################################################
#' Get Sector SPDR Components
#'
#' @param sector.etf SPDR symbol
#' 
#' @return tickers
#' 
#' @examples
#' \dontrun{ 
#' sector.spdr.components()
#' }
#' @export
###############################################################################
sector.spdr.components <- function(sector.etf = 'XLE')
{
  url = paste('http://www.sectorspdr.com/spdr/composition/?symbol=', sector.etf, sep='')
  txt = join(readLines(url))
  
  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
  tickers = temp[, 'Symbol']
  
  return(tickers)
}


###############################################################################
#' S&P 500 Components
#'
#' @return tickers and sector
#' 
#' @examples
#' \dontrun{ 
#' sp500.components()
#' }
#' @export
###############################################################################
sp500.components <- function()
{
  url = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
  txt = join(readLines(url))
  
  # extract table from this page	
  temp = extract.table.from.webpage(txt, 'Ticker', hasHeader = T)
  tickers = temp[, 'Ticker symbol']
  sector = temp[, 'GICS Sector']
  
  return(list(tickers=tickers, sector=sector))
}

# List of sites that keep SP500 Components
# http://www.s-p-500.com/stocks-a-b/
#http://www.forexpros.com/indices/us-spx-500-components
#http://marketvolume.com/indexes_exchanges/sp500_components.asp
#http://en.wikipedia.org/wiki/List_of_S%26P_500_companies
#http://en.wikipedia.org/wiki/Dow_Jones_Index


###############################################################################
#' S&P 100 Components
#' 
#' @examples
#' \dontrun{ 
#' sp100.components()
#' }
#' @export
###############################################################################
sp100.components <- function()
{
  url = 'http://www.barchart.com/stocks/sp100.php'
  txt = join(readLines(url))
  
  # extract table from this page	
  temp = extract.table.from.webpage(txt, 'Components', hasHeader = T)
  i.start = grep('Name', temp[,2])
  tickers = trim(temp[-c(1:i.start), 1])
  
  return(tickers)	
}


###############################################################################
#' iShares FTSE 100 (ISF)
#' 
#' Yahoo ticker for UK stocks ABF.L
#'
#' 
#' 
#' @examples
#' \dontrun{ 
#' ftse100.components()
#' }
#' @export
###############################################################################
ftse100.components <- function()
{
  # get holdings from uk.ishares.com
  url = 'http://uk.ishares.com/en/rc/products/ISF/all-holdings/'
  txt = join(readLines(url))
  
  # extract table from this page		
  txt = gsub('&#37;','%',txt)
  temp = extract.table.from.webpage(txt, 'Security', hasHeader = T)
  
  temp = trim(temp)
  colnames(temp) = temp[1,]
  temp = temp[-1,]		
  holdings = temp
  
  
  # get ISIN to ticker map from www.londonstockexchange.com
  page.label = ''	
  ticker2ISIN = c()
  for(i in 1:100) {	
    cat(i,'\n')
    
    # download
    url = paste('http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX&page=', i, sep='')
    txt = join(readLines(url))
    
    # get page label	
    pos = regexpr('Page [0-9]+ of [0-9]+', txt, ignore.case = T)
    page.label.new = substr(txt, pos, pos + attr(pos, 'match.length')-1)
    
    if(page.label == page.label.new) break
    page.label = page.label.new
    
    # extract table
    temp.table = extract.table.from.webpage(txt, 'Price', hasHeader = T)
    colnames(temp.table)[1] = 'tickers'
    
    # extract links
    temp = gsub(pattern = '<a', replacement = '<td>', txt, perl = TRUE)
    temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE)	
    
    temp = extract.table.from.webpage(temp, 'Price', hasHeader = T)
    pos = regexpr('fourWayKey=', temp[,2])
    ISIN = as.vector(sapply(1:nrow(temp), function(j) 
      substr(temp[j,2], pos[j] + attr(pos, 'match.length')[j], pos[j] + attr(pos, 'match.length')[j] + 12 - 1)
    ))
    
    
    ticker2ISIN = rbind(ticker2ISIN, cbind(temp.table[,spl('ticker,Name,Price'), drop=F], ISIN))
  }
  
  ISIN = intersect(holdings[,'ISIN'],ticker2ISIN[,'ISIN'])
  holdings = cbind(holdings[match(ISIN, holdings[,'ISIN']), ],
                   ticker2ISIN[match(ISIN, ticker2ISIN[,'ISIN']), spl('ticker,Name,Price')])
  
  return(apply(holdings, 2, list))
}


###############################################################################
#' Get the latest prices from the Google finance:
#'
#' @param tickers symbols
#' 
#' @examples
#' \dontrun{ 
#' getQuote.google(spl('MSFT,AAPL,IBM'))
#' }
#' @export
###############################################################################
getQuote.google <- function(tickers) {
  url = paste('http://finance.google.com/finance/info?client=ig&q=', join(tickers,','), sep='')
  txt = join(readLines(url))	
  temp = gsub(':', ',', txt) 	
  temp = scan(text = temp, what='', sep=',', quiet=T)
  temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
  
  index = match(spl('t,l,lt'), tolower(temp[,1]))+1
  names(index) = spl('ticker,last,date')
  
  last = as.double(temp[index['last'],])
  date = strptime(temp[index['date'],],format=' %b %d, %H,%M')
  
  out = data.frame(last,date)
  rownames(out) = temp[index['ticker'],]
  out
}

###############################################################################
#' An xml alternative
#' 
#' @param tickers symbols
#' 
#' @examples
#' \dontrun{ 
#' getQuote.google.xml(spl('MSFT,AAPL,NYSE:RY'))
#' }
#' @export
###############################################################################
getQuote.google.xml <- function(tickers) {
  url = paste('http://www.google.com/ig/api?', paste('stock=',tickers, '&', sep='', collapse=''), sep='')
  txt = join(readLines(url))	
  
  temp = txt		
  temp = gsub('<finance.*?>', '', temp, perl = TRUE) 
  temp = gsub('</finance>', '', temp, perl = TRUE) 
  temp = gsub('<xml.*?>', '', temp, perl = TRUE) 
  temp = gsub('</xml.*?>', '', temp, perl = TRUE) 
  temp = gsub('<\\?xml.*?>', '', temp, perl = TRUE) 
  temp = gsub('data=', '', temp, perl = TRUE) 
  temp = gsub('/><', ' ', temp) 	
  temp = gsub('>', '', temp) 	
  temp = gsub('<', '', temp) 	
  temp = scan(text = temp, what='', sep=' ', quiet=T)
  temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
  
  cnames = spl('trade_date_utc,trade_time_utc,symbol,last,high,low,volume,open,avg_volume,market_cap,y_close')
  index = match(cnames, tolower(temp[,1]))+1
  names(index) = cnames
  
  date = strptime(paste(temp[index['trade_date_utc'],], temp[index['trade_time_utc'],]), format='%Y%m%d %H%M%S',tz='UTC')
  date = as.POSIXct(date, tz = Sys.getenv('TZ'))
  
  out = data.frame(t(temp[index[-c(1:3)],]))
  colnames(out) = cnames[-c(1:3)]	
  rownames(out) = temp[index['symbol'],]
  out
}

###############################################################################
#' Download historical prices from Pi Trading - Free Market Data
#'
#' @param Symbols tickers
#' @param env envelope
#' @param auto.assign assignment
#' @param download downloaded
#' 
#' @examples
#' \dontrun{ 
#' getSymbols.PI()
#' }
#' @export
###############################################################################
getSymbols.PI <- function
(
  Symbols, 
  env = .GlobalEnv, 
  auto.assign = TRUE,
  download = TRUE	
) 
{
  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
  
  # read all Symbols
  for (i in 1:len(Symbols)) {	
    if(download) {
      # http://pitrading.com/free_eod_data/SPX.zip
      url = paste('http://pitrading.com/free_eod_data/', Symbols[i], '.zip', sep='')
      filename = paste(temp.folder, '/', Symbols[i], '.zip', sep='')			
      download.file(url, filename,  mode = 'wb')
      
      # unpack
      unzip(filename, exdir=temp.folder)	
    }
    
    filename = paste(temp.folder, '/', Symbols[i], '.txt', sep='')
    
    temp = read.delim(filename, header=TRUE, sep=',')		
    #out = make.xts(temp[,-1], as.Date(temp[,1],'%m/%d/%Y'))
    out = make.xts(temp[,-1], as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%m/%d/%Y'))
    indexClass(out) = 'Date'
    out$Adjusted = out$Close
    
    cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')					
    
    if (auto.assign) {		
      assign(paste(gsub('\\^', '', Symbols[i]), sep='_'), out, env)	
    }	
  }
  if (!auto.assign) {
    return(out)
  } else {		
    return(env)				
  }	
}

###############################################################################
#' Download FX qoutes: end of day and hourly
#' 
#' An xml alternative
#'
#' @param Symbols tickers
#' @param type hourly or daily
#' @param env envelope
#' @param auto.assign assignment
#' @param download true or false
#' 
#' @examples
#' \dontrun{ 
#' getSymbols.fxhistoricaldata(spl('MSFT,AAPL,NYSE:RY'))
#' }
#' @export
###############################################################################
getSymbols.fxhistoricaldata <- function
(
  Symbols, 
  type = spl('hour,day'),
  env = .GlobalEnv, 
  auto.assign = TRUE,
  download = FALSE	
) 
{		
  type = type[1]
  
  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
  
  # read all Symbols
  for (i in 1:len(Symbols)) {	
    if(download) {
      # http://www.fxhistoricaldata.com/download/EURUSD?t=hour
      url = paste('http://www.fxhistoricaldata.com/download/', Symbols[i], '?t=', type, sep='')
      filename = paste(temp.folder, '/', Symbols[i], '_', type, '.zip', sep='')			
      download.file(url, filename,  mode = 'wb')
      
      # unpack
      unzip(filename, exdir=temp.folder)	
    }
    
    filename = paste(temp.folder, '/', Symbols[i], '_', type, '.csv', sep='')
    
    temp = read.delim(filename, header=TRUE, sep=',')		
    colnames(temp) = gsub('[X\\.|\\.]', '', colnames(temp))			
    out = make.xts(temp[,spl('OPEN,LOW,HIGH,CLOSE')], 
                   strptime(paste(temp$DATE, temp$TIME), format='%Y%m%d %H:%M:%S'))
    
    cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')					
    
    if (auto.assign) {		
      assign(paste(gsub('\\^', '', Symbols[i]), type, sep='_'), out, env)	
    }	
  }
  if (!auto.assign) {
    return(out)
  } else {		
    return(env)				
  }	
}