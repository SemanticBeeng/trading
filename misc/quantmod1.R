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
from <- as.Date('2015-01-01')
to <- as.Date('2015-07-08')
#tmpenv$ORAN.PA <- xts(1:517, as.Date(from:to))
storageDir <- file.path("/datascience/marketdata/storage")

#setSymbolLookup.FI(storage_method = "rda",
#                   base_dir = file.path("/datascience/marketdata/storage"))

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
ohlc <- align.time(ohlc, 60)

chartSeries(x = ohlc, name = symbol, TA='addVo()')

