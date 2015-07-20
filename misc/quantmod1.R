require(latticeExtra)
require(ggplot2)
require(reshape2)
suppressPackageStartupMessages(
  require(googleVis)
)
require(quantmod)
require(PerformanceAnalytics)
require(xtsExtra)
require(rCharts)

setwd("/datascience/marketdata/storage")

# get S&P 500 data from FRED (St. Louis Fed)
ORAN.PA <- na.omit( 
  getSymbols(
    "2013.07.01.ORAN.PA",
    src="RData",
#    file.path="2015.01.02.ORAN.PA",
    dir = "ORAN.PA",
    extension = "RData",
    from = "2015-01-01",
    auto.assign = FALSE,
    verbose = TRUE
  )
)

