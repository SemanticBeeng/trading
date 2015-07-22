# Source: https://github.com/evelynmitchell/RFinanceDemos/blob/e336227252179bb437f0394ba6c5613237524567/blotter/pkg/FinancialInstrument/inst/tests/test-getSymbols.FI.R
#require(latticeExtra)
#require(ggplot2)
#require(reshape2)
#suppressPackageStartupMessages(
  #require(googleVis)
#)
require(quantmod)
require(FinancialInstrument)

tmpenv <- new.env()

symbol <- "AIRP.PA"
from <- as.Date('2000-04-21')
to <- from + 5 #364

storageDir <- file.path("/datascience/marketdata/storage")

#setSymbolLookup.FI(storage_method = "rda",
#                   base_dir = file.path("/datascience/marketdata/storage"))

result <- tryCatch(getSymbols(
            symbol,
            from = from,
            to = to,
            src = "FI",
            env = tmpenv,
            dir = storageDir,
            etension = "RData",
            auto.assign = FALSE,
            verbose = TRUE), 
            error = function(e) {
                cat("Error '", e, "' found!\n", sep = "")
              }
            )

tmpenv$ORAN.PA <- na.omit(getSymbols(
    symbol,
    from = from,
    to = to,
    src = "FI",
    env = tmpenv,
    dir = storageDir,
    etension = "RData",
    auto.assign = FALSE,
    verbose = TRUE))

# tmpenv$ORAN.PA [tmpenv$ORAN.PA$Price < 128.3] # 5 rows

# Merge to detect missing days
# http://artax.karlin.mff.cuni.cz/r-help/library/xts/html/merge.html
# http://stackoverflow.com/a/4139124/4032515
tmpenv$allDays <- xts( , as.Date(from:to))
# Build an xts for all days of the year from endpoints of real trade data (not aligned to day endpoint!)
ep <- index(tmpenv$ORAN.PA[endpoints(tmpenv$ORAN.PA, on = 'days', k = 1)])
tmpenv$allTradedDays <- xts(rep(-1, length(ep)) , ep)
                      
#tmpenv$ORAN_PA_Daily <- index(to.daily(tmpenv$ORAN.PA[, 1:2]))
#tmpenv$allDays <- xts( , index(to.daily(tmpenv$ORAN.PA[, 1:2])))

tmp <- merge(tmpenv$allDays, tmpenv$ORAN.PA, fill = -2)
tmp
# http://stackoverflow.com/a/1686614/4032515

tmp [tmp$Price == -1 ]

has.Vo(tmpenv$ORAN.PA)
has.Ask(tmpenv$ORAN.PA)
has.Bid(tmpenv$ORAN.PA)
has.HLC(tmpenv$ORAN.PA)
#tmpenv$ORAN.PA['2013-07-01 09']

p <- periodicity(tmpenv$ORAN.PA)
unclass(p)

head(tmpenv$ORAN.PA, n = 50)

# Source: http://www.quantmod.com/examples/data/

#ps <- period.sum(tmpenv$ORAN.PA[, 1], endpoints(tmpenv$ORAN.PA, on = "minutes", k = 1)); 
#ps
#ts <- align.time(ps, 60)

# To OHLC
ohlc <- to.period(tmpenv$ORAN.PA[, 1:2], period = "minutes", k = 1440)
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
