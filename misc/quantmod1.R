# Source: https://github.com/evelynmitchell/RFinanceDemos/blob/e336227252179bb437f0394ba6c5613237524567/blotter/pkg/FinancialInstrument/inst/tests/test-getSymbols.FI.R
#require(latticeExtra)
#require(ggplot2)
#require(reshape2)
#suppressPackageStartupMessages(
  #require(googleVis)
#)
require(quantmod)
require(FinancialInstrument)
#require(PerformanceAnalytics)
#require(xtsExtra)
#require(rCharts)

#setwd("/datascience/marketdata/storage")

#setDefaults(getSymbols,src='RData')

# get S&P 500 data from FRED (St. Louis Fed)
#ORAN.PA <- na.omit( 
#  getSymbols(
#    "2013.07.01.ORAN.PA",
#    src="RData",
#    file.path="2015.01.02.ORAN.PA",
#    dir = "ORAN.PA",
#    extension = "RData",
#    from = "2015-01-01",
#    auto.assign = FALSE,
#    verbose = TRUE
#  )
#)

tmpenv <- new.env()

symbol <- "ORAN.PA"
from <- as.Date('2013-07-01')
to <- from + 516
tmpenv$ORAN.PA <- xts(1:517, as.Date(from:to))
storageDir <- file.path("/datascience/marketdata/storage")

getSymbols(
    symbol,
    from=from,
    to=to,
    src="FI",
    env = tmpenv,
    dir=storageDir,
    etension="RData",
    auto.assign = FALSE,
    verbose = TRUE)
# 

