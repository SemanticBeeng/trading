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

symbol <- "ORAN.PA"
from <- as.Date('2013-07-01')
to <- from + 516
#tmpenv$ORAN.PA <- xts(1:517, as.Date(from:to))
storageDir <- file.path("/datascience/marketdata/storage")

#setSymbolLookup.FI(storage_method = "rda",
#                   base_dir = file.path("/datascience/marketdata/storage"))

tmpenv$ORAN.PA <- getSymbols(
    symbol,
    from = from,
    to = from + 5,
    src = "FI",
    env = tmpenv,
    dir = storageDir,
    etension = "RData",
    auto.assign = FALSE,
    verbose = TRUE)

has.Vo(tmpenv$ORAN.PA)
has.Ask(tmpenv$ORAN.PA)
has.Bid(tmpenv$ORAN.PA)
has.HLC(tmpenv$ORAN.PA)
tmpenv$ORAN.PA['2013-07-01 09']

p <- periodicity(tmpenv$ORAN.PA)
unclass(p)

to.minutes5(tmpenv$ORAN.PA, "Price")
