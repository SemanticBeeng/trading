# Source: https://github.com/evelynmitchell/RFinanceDemos/blob/e336227252179bb437f0394ba6c5613237524567/blotter/pkg/FinancialInstrument/inst/tests/test-getSymbols.FI.R
#require(latticeExtra)
#require(ggplot2)
#require(reshape2)
#suppressPackageStartupMessages(
  #require(googleVis)
#)
require(quantmod)
require(FinancialInstrument)
require(dplyr)
require(lubridate)
require(chron)

# Set defaults
#setSymbolLookup.FI(storage_method = "rda",
#                   Symbols = c(symbol),
#                   base_dir = file.path("/datascience/marketdata/storage"),
#                   etension = "RData")
###################################################################
# Filtering
###################################################################
isHoliday <- function (x) {
  return (month(x) == 12) && (day(x) == 25 || day(x) == 26)
}

###################################################################
# Produce the date ranges used for loading the data
# Resources
# http://stackoverflow.com/questions/11308367/error-in-my-code-object-of-type-closure-is-not-subsettable
# http://www.win-vector.com/blog/2015/04/what-can-be-in-an-r-data-frame-column/
# https://stat.ethz.ch/pipermail/r-help//2013-April/352482.html
# http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
# http://stackoverflow.com/questions/20689650/how-to-append-rows-to-an-r-data-frame
###################################################################
dateRanges <- function() {

  dummyDate <- as.POSIXct('1900-01-01', tz = "GMT")

  df <- data.frame(y = 0:14, from = dummyDate, to = dummyDate, stringsAsFactors = FALSE)
  
  for(y in 0:14) {
  
    year <- 2000 + y
    daysInYear <- if(leap_year(year)) 366 else 365
    from <- as.POSIXct(paste(year, '-01-01', sep = ""), tz = "GMT")
    df$from[y+1] <- from
    df$to[y+1] <- floor_date(from + daysInYear * 86400 - 1, "day")
  }

  df$from[15] <- as.POSIXct(paste(2000 + 15, '-01-01', sep = ""), tz = "GMT")
  df$to[15] <- as.POSIXct(paste(2000 + 15, '-07-08', sep = ""), tz = "GMT")
  df
}

###################################################################
# Get symbol data
###################################################################
getSymbol_Data <- function(symbol) {

  symbolEnv <- get(symbol, globalenv())
  get(symbol, symbolEnv)  
}

###################################################################
# Get symbol environment
###################################################################
getSymbol_Env <- function(symbol) {
  
    if(exists(symbol, globalenv())) {
      symbolEnv <- get(symbol, globalenv())
    } else {
      symbolEnv <- new.env()
      assign(symbol, symbolEnv, envir = globalenv())
      symbolEnv
  }
}

###################################################################
# Get symbol missing days
###################################################################
getSymbol_MissingsDays <- function(symbol) {
  
  symbolEnv <- get(symbol, globalenv())
  symbolEnv$missingDays
}

###################################################################
# Load data for one symbol for a date range
###################################################################
loadSymbolForRange <- function(symbol # : String
                               , from # : POSIXct
                               , to # : POSIXct
) {
  
  # todo: unsure if can load one day at a time and accumulate results under one symbol
  # http://databasefaq.com/index.php/answer/235383/r-error-handling-xts-lapply-quantmod-have-lapply-continue-even-after-encountering-an-error-using-getsymbols-from-quantmod-duplicate
  
  symbolEnv <- getSymbol_Env(symbol)
  
  # 1) Load symbol data
  result <- try(getSymbols(
    symbol,
    from = from,
    to = to,
    src = "FI",
    dir = file.path("/datascience/marketdata/storage"),
    etension = "RData",
    env = symbolEnv,
    auto.assign = TRUE,
    verbose = TRUE))
  
  # ... now available here
  symbolData = get(symbol, symbolEnv)
  
  # 2) Determine days missing data
  # https://tonybreyal.wordpress.com/2011/11/29/outersect-the-opposite-of-rs-intersect-function/
  
  daysRange <- seq.POSIXt(from = from, to = to, by = 'day')
  daysTraded <- unique(floor_date(index(symbolData), "day"))
  
  daysDiff <- as.data.frame(setdiff(daysRange, daysTraded))
  colnames(daysDiff) <- c("date")
  
  # Filter out Saturdays, Sundays and Christmans
  # http://stackoverflow.com/questions/9216138/find-the-day-of-a-week-in-r
  # http://stackoverflow.com/questions/2792819/r-dates-origin-must-be-supplied
  daysDiff <- daysDiff %>% 
    transmute(date = as.POSIXct(daysDiff$date, tz='GMT', origin="1970-01-01")) %>%
    mutate(wday(date)) %>% 
    #mutate(isHoliday(date)) %>% 
    filter(wday(date) != 7) %>% 
    filter(wday(date) != 1) %>% 
    filter(!isHoliday(date))
  
  print(paste("Found missing days ", nrow(daysDiff)))
  if(is.null(symbolEnv$missingDays)) {
    symbolEnv$missingDays = daysDiff
  } else {
    symbolEnv$missingDays = rbind(symbolEnv$missingDays, daysDiff)
  }
  
  # Return the symbol data  
  #get(symbol, symbolEnv)
}

###################################################################
# Print symbol statistics
###################################################################
printStats <- function(symbol) {

  symbolData = getSymbol_Data(symbol)
  
  cat(paste("Symbol" , symbol, "has", nrow(symbolData), "rows :\n------------------------------\n"))
  print(paste("has.Vo", has.Vo(symbolData)))
  print(paste("has.Ask", has.Ask(symbolData)))
  print(paste("has.Bid", has.Bid(symbolData)))
  print(paste("has.HLC", has.HLC(symbolData)))
        
  #p <- periodicity(symbolData)
  #unclass(p)
  
  print(head(symbolData, n = 50))
  
  cat("Missing days :\n------------------------------\n")
  print(getSymbol_MissingsDays(symbol))
}

# Source: http://www.quantmod.com/examples/data/

###################################################################
# To OHLC
###################################################################
toMinuteBars <- function(symbol, # : String
                         kMinutes # : int
                        ) {
  #ps <- period.sum(tmpenv$ORAN.PA[, 1], endpoints(tmpenv$ORAN.PA, on = "minutes", k = 1)); 
  #ps
  #ts <- align.time(ps, 60)
  
  symbolData = getSymbol_Data(symbol)

  ohlc <- to.period(symbolData[, 1:2], period = "minutes", k = kMinutes)
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")

  ohlc <- align.time(ohlc, 60)
  
  ohlc
}

###################################################################
# Draw chart series
# # Zoom interactivelly: http://www.quantmod.com/documentation/zoomChart.html
#chartSeries(x = window(ohlc, start = c(2000, 1), end = c(2000, 2)), name = symbol, TA='addVo()')
###################################################################
drawChart <- function(ohlc) {
  
  chartSeries(x = ohlc, name = symbol, TA= c(addVo()))
  #zoomChart("2000-01::2000-05")
  #zooom()
}

###################################################################
# Main program
###################################################################


symbol <- "AIRP.PA"
dRanges = dateRanges()

loadSymbolForRange(symbol = symbol, from = dRanges$from[1], to = dRanges$to[1])
loadSymbolForRange(symbol = symbol, from = dRanges$from[2], to = dRanges$to[2])
loadSymbolForRange(symbol = symbol, from = dRanges$from[3], to = dRanges$to[3])
loadSymbolForRange(symbol = symbol, from = dRanges$from[4], to = dRanges$to[4])
loadSymbolForRange(symbol = symbol, from = dRanges$from[5], to = dRanges$to[5])

loadSymbolForRange(symbol = symbol, from = dRanges$from[15], to = dRanges$to[15])

printStats(symbol)
class(getSymbol_Data(symbol))
drawChart(toMinuteBars(symbol, 1440))

###################################################################
# googleVis charts
###################################################################
renderGoogleVisChart <- function(ohlc) {

  ohlc.df =data.frame(date = as.Date(index(ohlc)), 
                      open = as.numeric(ohlc$Open),
                      close = as.numeric(ohlc$Close),
                      high = as.numeric(ohlc$High),
                      low = as.numeric(ohlc$Low))
  
  mplot = gvisCandlestickChart(ohlc.df, xvar = "date", low = "low", 
                               open = "open", close = "close", high = "high",
                               options = list(legend = 'none', width = 900, title = "Stock Price"))
  plot(mplot)
}
