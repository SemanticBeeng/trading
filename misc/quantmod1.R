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

tmpenv <- new.env()
storageDir <- file.path("/datascience/marketdata/storage")
# todo: set defaults
#setSymbolLookup.FI(storage_method = "rda",
#                   base_dir = file.path("/datascience/marketdata/storage"))

isChristmas <- function (x) {
  #cat("isChristmas " + month(x) + day(x)  + " = " + (month(x) == 12 && (day(x) == 25 || day(x) == 26)) + "\n")
  #cat("isChristmas " + x + "\n")
  return (month(x) == 12) && (day(x) == 25 || day(x) == 26)
}
  
symbol <- "AIRP.PA"

# Define the date range for data loading
from <- as.POSIXct('2000-01-01', tz = "GMT")
to <- from + 365 * 10 * 86400

# todo: unsure if can load one day at a time and accumulate results under one symbol
# http://databasefaq.com/index.php/answer/235383/r-error-handling-xts-lapply-quantmod-have-lapply-continue-even-after-encountering-an-error-using-getsymbols-from-quantmod-duplicate

result <- try(getSymbols(
            symbol,
            from = from,
            to = to,
            src = "FI",
            env = tmpenv,
            dir = storageDir,
            etension = "RData",
            auto.assign = TRUE,
            verbose = TRUE))

symbolData = get(symbol, tmpenv)

# Determine missing days
# https://tonybreyal.wordpress.com/2011/11/29/outersect-the-opposite-of-rs-intersect-function/
daysRange <- seq.POSIXt(from = from, to = to, by = 'day')
daysTraded <- unique(floor_date(index(symbolData), "day"))

daysDiff <- as.data.frame(setdiff(daysRange, daysTraded))
colnames(daysDiff) <- c("date")

# Filter out Saturdays
# http://stackoverflow.com/questions/9216138/find-the-day-of-a-week-in-r
#http://stackoverflow.com/questions/2792819/r-dates-origin-must-be-supplied
daysDiff <- daysDiff %>% 
            transmute(date = as.POSIXct(daysDiff$date, tz='GMT', origin="1970-01-01")) %>%
            mutate(wday(date)) %>% 
            #mutate(isChristmas(date)) %>% 
            filter(wday(date) != 7) %>% 
            filter(wday(date) != 1) %>% 
            filter(!isChristmas(date))
daysDiff

# Merge to detect missing days
# http://artax.karlin.mff.cuni.cz/r-help/library/xts/html/merge.html
# http://stackoverflow.com/a/4139124/4032515
#tmpenv$allDays <- xts( , as.Date(from:to))
# Build an xts for all days of the year from endpoints of real trade data (not aligned to day endpoint!)
#ep <- index(tmpenv$ORAN.PA[endpoints(tmpenv$ORAN.PA, on = 'days', k = 1)])
#tmpenv$allTradedDays <- xts(rep(-1, length(ep)) , ep)
                      
#tmpenv$ORAN_PA_Daily <- index(to.daily(tmpenv$ORAN.PA[, 1:2]))
#tmpenv$allDays <- xts( , index(to.daily(tmpenv$ORAN.PA[, 1:2])))

#tmp <- merge(tmpenv$allDays, tmpenv$ORAN.PA, fill = -2)
#tmp
# http://stackoverflow.com/a/1686614/4032515

#tmp [tmp$Price == -1 ]

has.Vo(symbolData)
has.Ask(symbolData)
has.Bid(symbolData)
has.HLC(symbolData)
#symbolData['2013-07-01 09']

p <- periodicity(symbolData)
unclass(p)

head(symbolData, n = 50)

# Source: http://www.quantmod.com/examples/data/

#ps <- period.sum(tmpenv$ORAN.PA[, 1], endpoints(tmpenv$ORAN.PA, on = "minutes", k = 1)); 
#ps
#ts <- align.time(ps, 60)

# To OHLC
ohlc <- to.period(symbolData[, 1:2], period = "minutes", k = 1440)
colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
ohlc <- align.time(ohlc, 60)

# Zoom interactivelly: http://www.quantmod.com/documentation/zoomChart.html
#chartSeries(x = window(ohlc, start = c(2000, 1), end = c(2000, 2)), name = symbol, TA='addVo()')
chartSeries(x = ohlc, name = symbol, TA= c(addVo()))
zoomChart("2000-01::2000-05")
zooom()

# googleVis charts
ohlc.df =data.frame(date = as.Date(index(ohlc)), 
                    open = as.numeric(ohlc$Open),
                    close = as.numeric(ohlc$Close),
                    high = as.numeric(ohlc$High),
                    low = as.numeric(ohlc$Low))

mplot = gvisCandlestickChart(ohlc.df, xvar = "date", low = "low", 
                             open = "open", close = "close", high = "high",
                             options = list(legend = 'none', width = 900, title = "Stock Price"))
plot(mplot)
