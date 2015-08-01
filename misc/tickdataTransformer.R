# Source: https://github.com/evelynmitchell/RFinanceDemos/blob/e336227252179bb437f0394ba6c5613237524567/blotter/pkg/FinancialInstrument/inst/tests/test-getSymbols.FI.R
# Other resources
# http://adv-r.had.co.nz/Environments.html
# http://adv-r.had.co.nz/Environments.html#binding

#require(latticeExtra)
#require(ggplot2)
#require(reshape2)
#suppressPackageStartupMessages(
  #require(googleVis)
#)
require(stringr)
require(quantmod)
require(FinancialInstrument)
require(dplyr)
require(reshape2)
require(lubridate)
require(chron)
library(pryr)
library(lineprof)

STORAGE_ROOT = "/datascience/marketdata/storage"
YEAR_COUNT <- 16

symbols = c(
            #"AEGN.AS", ok
            #"AIRP.PA", ok
            #"ALSO.PA", ok
            #"ALVG.DE", ok
            #"AXAF.PA", ok
            #"BASF.DE", ok
            #"BAYG.DE", ok
            #"BBVA.MC", ok
            #"BNPP.PA", #skipped
            "CAGR.PA", #skipped
            #"CARR.PA", ok
            "CRDI.MI", #skipped
            #"CRH.I", ok
            #"DANO.PA", ok
            #"DB1Gn.DE", ok
            #"DBKGn.DE", ok
            #"DCXGn.DE", ok
            #"DTEGn.DE", ok
            #"ENEI.M", empty
            #"ENI.MI", ok
            #"EONG.DE", ok
            #"ESSI.PA", ok
            #"GASI.MI", ok
            #"GSZ.PA", ok
            #"IBE.MC", ok
            "ING.AS", #skipped
            #"INTB.BR", ok
            #"ISPA.AS", ok!
            #"ISP.MI", ok
            #"LVMH.PA", ok
            #"MUVGn.DE", ok
            #"NOK1V.HE", ok
            #"ORAN.PA", ok
            #"OREP.PA", ok
            #"PHG.AS", ok
            #"REP.MC", ok
            #"RWEG.DE", ok
            "SAN.MC", #skipped
            #"SAPG.DE", ok
            "SASY.PA", #skipped
            "SCHN.PA", #skipped
            "SGEF.PA", #skipped
            "SGOB.PA", #skipped
            #"SIEGn.DE", ok
            "SOGN.PA", #skipped
            #"TEF.MC", ok
            #"TLIT.MI", ok
            "TOTF.PA" #skipped
            #"ULVR.L", ok
            #"UNc.AS", ok
            #"VIV.PA", ok
            #"VOWG.DE" ok
)

###################################################################
newPOSIXct <- function(d) {
  as.POSIXct(d, tz='GMT', origin="1970-01-01")
}

# Set defaults
#setSymbolLookup.FI(storage_method = "rda",
#                   Symbols = c(symbol),
#                   base_dir = file.path("/datascience/marketdata/storage"),
#                   etension = "RData")
###################################################################
# Produce the date ranges used for loading the data
# Resources
# http://stackoverflow.com/questions/11308367/error-in-my-code-object-of-type-closure-is-not-subsettable
# http://www.win-vector.com/blog/2015/04/what-can-be-in-an-r-data-frame-column/
# https://stat.ethz.ch/pipermail/r-help//2013-April/352482.html
# http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
# http://stackoverflow.com/questions/20689650/how-to-append-rows-to-an-r-data-frame
# http://stackoverflow.com/questions/7929668/efficiently-create-a-matrix-from-function-values
# http://stackoverflow.com/questions/24936330/adding-multiple-columns-in-a-dplyr-mutate-call
###################################################################
dateRanges <- function () {
  
  years  <- 0:15
  months <- 1:12
  
  concatFromTo <- function(y, m) {
    paste(
      ymd(paste(2000 + y, m, "1", sep="-"), tz = "GMT"),
      floor_date(ymd(paste(2000 + y, m + 1, "1", sep="-"), tz = "GMT") - 1, "day"),
      sep=":"
    )
  }
  
  df <- as.data.frame((as.vector(t(outer(years, months, concatFromTo)))))
  colnames(df) <- c("fromToStr")
  
  df <- df %>% 
    do(cbind(., data.frame(fromTo = str_split_fixed(.$fromToStr, ":", 2)))) %>% 
    mutate(from = newPOSIXct(fromTo.1)) %>% 
    mutate(to = newPOSIXct(fromTo.2)) %>% 
    select(from, to)
  
  # Truncate and adjust the end
  df <- df %>% 
    filter(from < newPOSIXct("2015-09-01")) 
  
  df$to[nrow(df)] = newPOSIXct("2015-08-07")
  df
}

###################################################################
# Get symbol data
# http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling
###################################################################
getSymbol_Data <- function(symbol) {

  tryCatch({ 
    symbolEnv <- get(symbol, globalenv())
    get(symbol, symbolEnv)  
  }, 
  error = function(e) NULL)
  
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
  cat(paste("\nLoading symbol", symbol, "for range", from, ":", to, "..."))
  
  object_size(globalenv())
  #removeSymbols(NULL, globalenv())
  symbolData = getSymbol_Data(symbol)
  rm(symbolData)
  
  symbolEnv <- getSymbol_Env(symbol)
  
  # 1) Load symbol data
  mc <- mem_change(result <- try(getSymbols(
    symbol,
    from = from,
    to = to,
    src = "FI",
    dir = file.path(paste(STORAGE_ROOT, "/tick", sep="")),
    etension = "RData",
    env = symbolEnv,
    auto.assign = TRUE,
    verbose = FALSE)))
  
  cat(paste("\nDone loading; mem_change = ", mc, ".\n"))

  # ... now available here
  symbolData = get(symbol, symbolEnv)
  
  cat(paste("Symbol data object size: ", object_size(symbolData)))
  
  # 2) Determine days missing data
  if(Config_MissingDays) {
    determineMissingDays(symbol, from, to)
  }
  
  printStats(symbol) 
  # Return the symbol data  
  #get(symbol, symbolEnv)
}

###################################################################
# Is a Holiday
# http://rpackages.ianhowson.com/cran/dplyr/man/setops.html
# http://stackoverflow.com/questions/22767893/find-number-of-rows-using-dplyr-group-by
###################################################################
# https://www.euronext.com/trading-calendars-hours
# https://www.euronext.com/en/holidays-and-hours
# http://chronos-st.blogspot.ro/2008/03/easter-dates-from-2000-to-2020.html
# http://stackoverflow.com/questions/6558921/r-boolean-operators-and
# http://stackoverflow.com/questions/21888910/how-to-specify-names-of-columns-for-x-and-y-when-joining-in-dplyr
# http://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key
# http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
# http://stackoverflow.com/questions/27149306/dplyrselect-one-column-and-output-as-vector **
EasterDays = data.frame(year =  c(2000, 2000, 2001, 2001, 2002, 2002, 2003, 2003, 2004, 2004, 2005, 2005, 2006, 2006, 2007, 2007, 2008, 2008, 2009, 2009, 2010, 2010, 2011, 2011, 2012, 2012, 2013, 2013, 2014, 2014, 2015, 2015), 
                        month = c(   4,    4,    4,    4,    3,    4,    4,    4,    4,    4,    3,    3,    4,    4,    4,    4,    3,    3,    4,    4,    4,    4,    4,    4,    4,    4,    3,    4,    4,    4,    4,    4), 
                        day =   c(  21,   24,   13,   16,   29,    1,   18,   21,    9,   12,   25,   28,   14,   17,    6,    9,   21,   24,   10,   13,    2,    5,   22,   25,    6,    9,   29,    1,   18,   21,    3,    6)
)
EasterDays = EasterDays %>% mutate(isHoliday = TRUE)
###################################################################
isHoliday <- function (x) {
  
  # Create a data.frame by splitting the POSIXct into year, month and day
  xdf <- as.data.frame(x) %>% 
    mutate(year = year(x), month = month(x), day = as.numeric(day(x))) #%>%
  #select(-x)
  # Left join with holidays to mark the days that are in that set
  isEasterDay <- left_join(xdf, EasterDays, by = c("year" = "year", "month" = "month", "day" = "day")) %>%
    # extract the column with the desired value
    .$isHoliday
  #isHoliday <- transform(isHoliday, isHoliday2 = ifelse(is.na('isHoliday'), FALSE, isHoliday)) %>% .$isHoliday2
  
  #isHoliday <- (left_join(xdf, Holidays, by = c("year" = "year", "month" = "month", "day" = "day")) %>% select(isHoliday))[,1]
  #left_join(xdf, Holidays, by = c("year" = "year", "month" = "month", "day" = "day")) %>% collect %>% .[["Holidays"]]
  #anti_join(xdf, Holidays, by = c("year" = "year", "month" = "month", "day" = "day"))
  
  isIt <- 
    (month(x) ==  1) & (day(x) ==  1) | # Jan 1st
    (month(x) ==  5) & (day(x) ==  1) | # Labor day
    (month(x) == 12) & (day(x) == 25 | day(x) == 26 | day(x) == 31) | # Christmas
    isEasterDay # Easter
  
  #print(isIt)
  isIt
}

###################################################################
# Determine missing days
# https://tonybreyal.wordpress.com/2011/11/29/outersect-the-opposite-of-rs-intersect-function/
###################################################################
determineMissingDays <- function(symbol, from, to) {

  cat("\nCalculating missing days...")
  
  symbolEnv <- getSymbol_Env(symbol)
  symbolData = get(symbol, symbolEnv)
  
  daysRange <- seq.POSIXt(from = from, to = to, by = 'day')
  if(class(symbolData) == c("xts", "zoo")) {
    daysTraded <- unique(floor_date(index(symbolData), "day"))
    
  } else {
    daysTraded <- NA
  }
  
  daysDiff <- as.data.frame(dplyr::setdiff(daysRange, daysTraded))
  colnames(daysDiff) <- c("date")
  
  # Filter out Saturdays, Sundays and Christmans
  # http://stackoverflow.com/questions/9216138/find-the-day-of-a-week-in-r
  # http://stackoverflow.com/questions/2792819/r-dates-origin-must-be-supplied
  daysDiff <- daysDiff %>% 
    transmute(date = newPOSIXct(daysDiff$date)) %>%
    mutate(wday = wday(date)) %>% 
    mutate(isHoliday = isHoliday(date)) %>% 
    filter(wday(date) != 7) %>% 
    filter(wday(date) != 1) %>% 
    filter(is.na(isHoliday)) %>%
    select(date, wday)
  
  #  if(is.null(symbolEnv$missingDays)) {
  #    symbolEnv$missingDays = 0
  #  }
  
  if(nrow(daysDiff) != 0) {
    cat(paste("\nSymbol:", symbol, 
              "(current)Missing days:", nrow(symbolEnv$missingDays), 
              ", days traded:", nrow(daysTraded), 
              ", (+)days missing:", nrow(daysDiff)))
    symbolEnv$missingDays = rbind(symbolEnv$missingDays, daysDiff)
  }
}

###################################################################
# Print symbol statistics
###################################################################
printStats <- function(symbol, extended = FALSE) {

  symbolData = getSymbol_Data(symbol)
  
  cat(paste("\nSymbol" , symbol, "has", nrow(symbolData), "rows :\n------------------------------\n"))

  #cat(paste("\n",
  #          "has.Vo", has.Vo(symbolData), 
  #          "has.Ask", has.Ask(symbolData),
  #          "has.Bid", has.Bid(symbolData),
  #          "has.HLC", has.HLC(symbolData)))
        
  #p <- periodicity(symbolData)
  #unclass(p)
  
  #print(head(symbolData, n = 50))

  if(extended && Config_MissingDays) {
    cat(paste("\nMissing days :", getSymbol_MissingsDays(symbol)[,1], "\n")) 
    cat("------------------------------")

    fileName <- paste(STORAGE_ROOT, "/out/MissingDays_", symbol, ".txt", sep="")
    cat(paste("\nMissing days for ", symbol, "\n"),  file = fileName, append = FALSE)
    #cat(getSymbol_MissingsDays(symbol)[,1], file = fileName, append = TRUE)
    lapply(getSymbol_MissingsDays(symbol)[,1], function(x) cat(paste(toString(x), "\n"), file = fileName, append = TRUE))
  }
}

# Source: http://www.quantmod.com/examples/data/

###################################################################
# To minute OHLC bars
###################################################################
toMinuteBars <- function(symbol, # : String
                         kMinutes # : int
                        ) {

  cat(paste("\ntoMinuteBars for ", symbol, "minutes: ", kMinutes, "..."))
  symbolData = getSymbol_Data(symbol)

  ohlc <- to.period(symbolData[, 1:2], period = "minutes", k = kMinutes)
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")

  ohlc <- align.time(ohlc, 60)
  
  cat("\nDone.\n")
  ohlc
}

###################################################################
# To daily OHLC bars
# for US equities, you'd

#- shift the indexTZ timezone to US eastern time,
#- and then do x['T09:00/TT16:00'] to get regular market hours, and then
#- do to.daily on that data
#- shift the indexTZ timezone back to GMT

# http://blog.revolutionanalytics.com/2009/06/converting-time-zones.html **??
# http://www.r-bloggers.com/time-zones/
###################################################################
toDailyBars <- function(symbol # : String
                      ) {

  cat(paste("\ntoDailyBars for ", symbol, "..."))
  
  symbolData = getSymbol_Data(symbol)
  
  currentTZ <- indexTZ(symbolData)
  indexTZ(symbolData) <- "Europe/Germany"

  # http://stackoverflow.com/questions/11871572/subsetting-tricks-for-xts-in-r
  # http://stats.stackexchange.com/questions/12980/subset-data-by-month-in-r
  # http://stackoverflow.com/questions/15602187/how-to-use-the-index-of-an-xts-object-to-subset-another-xts-object
  
  symbolDataTmp <- symbolData['T09:00/T16:00']
    
  ohlc <- to.daily(symbolDataTmp[, 1:2])
  indexTZ(symbolData) <- currentTZ
  
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume")
  
  ohlc <- align.time(ohlc, 60)
  
  cat("\nDone.\n")
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
# Load a symbol for all data range
###################################################################
loadSymbol <- function(symbol) {
  
  dRanges = dateRanges()
  
  # todo : find a way to iterate over a data.frame that does not mess with the data
  # http://stackoverflow.com/questions/15059076/r-how-to-call-apply-like-function-on-each-row-of-dataframe-with-multiple-argum
  # http://stackoverflow.com/questions/16714020/loop-through-data-frame-and-variable-names
  # 
  dRangeCount <- nrow(dRanges)
  for(r in 1:dRangeCount) {
    loadSymbolForRange(symbol = symbol, from = dRanges$from[r], to = dRanges$to[r])
    
  }
#  prof1 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[1], to = dRanges$to[1]))
#  prof2 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[2], to = dRanges$to[2]))
#  prof3 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[3], to = dRanges$to[3]))
#  prof4 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[4], to = dRanges$to[4]))
#  prof5 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[5], to = dRanges$to[5]))
#  prof6 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[6], to = dRanges$to[6]))
#  prof7 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[7], to = dRanges$to[7]))
#  prof8 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[8], to = dRanges$to[8]))
#  prof9 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[9], to = dRanges$to[9]))
#  prof10 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[10], to = dRanges$to[10]))
#  prof11 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[11], to = dRanges$to[11]))
#  prof12 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[12], to = dRanges$to[12]))
#  prof13 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[13], to = dRanges$to[13]))
#  prof14 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[14], to = dRanges$to[14]))
#  prof15 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[15], to = dRanges$to[15]))
#  prof16 <- lineprof(loadSymbolForRange(symbol = symbol, from = dRanges$from[16], to = dRanges$to[16]))

  #shine(prof1)
  
#  lapply(dRanges, FUN = function (dr) { 
#      #print(class(dr[2]))
#      #print(dr[3])
#      #loadSymbolForRange(symbol = symbol, from = dr[2], to = dr[3])
#      loadSymbolForRange(symbol = symbol, from = dr$from, to = dr$to)
#  })
#  for(dr in dRanges) {
#    loadSymbolForRange(symbol = symbol, from = dr$from, to = dr$to)
#    
#  }  
  
  symbolEnv <- getSymbol_Env(symbol)
  
  if(Config_AggregateBars) {
    symbolEnv$minuteBars05 = toMinuteBars(symbol,  5)
    symbolEnv$minuteBars10 = toMinuteBars(symbol, 10)
    symbolEnv$minuteBars20 = toMinuteBars(symbol, 20)
    symbolEnv$minuteBars30 = toMinuteBars(symbol, 30)
    symbolEnv$minuteBars60 = toMinuteBars(symbol, 60)
    symbolEnv$dailyBars    = toDailyBars(symbol)
  }

}

###################################################################
# Expand the tick data archive for symbol
###################################################################
expandSymbolTickDataArchive <- function(symbol) {

  cmd <- paste("bash expandSymbolTickDataArchive.sh ", symbol)
  
  system(cmd)
  symbol
}

###################################################################
# Expand the tick data archive for symbol
###################################################################
purgeSymbolTickData <- function(symbol) {

  cmd <- paste("bash purgeSymbolTickData.sh ", symbol)
  
  system(cmd)
}

###################################################################
# Main program
###################################################################

Config_MissingDays <- TRUE
Config_AggregateBars <- FALSE #TRUE

#gcinfo(TRUE)

# http://stackoverflow.com/questions/11890600/time-zone-period-apply-in-xts-using-r
Sys.setenv(TZ="GMT")

#symbol <- "AEGN.AS" # all
#symbol <- "AIRP.PA" # all
#symbol <- "ALVG.DE" # all
#symbol <- "ALSO.PA" # all
#symbol <- "AXAF.PA" # all

#symbol <- "ORAN.PA"

#loadSymbol(symbol)

#cat("\n----------------------\n")
#cat("\nExtensive statistics.\n")

#printStats(symbol, TRUE)

#drawChart(toMinuteBars(symbol, 1440))

for(symbol in symbols) {
  
  expandSymbolTickDataArchive(symbol)

  cat("\n#####################################################\n")
  loadSymbol(symbol)
  
  purgeSymbolTickData(symbol)

  cat("\nExtensive statistics.\n")
  printStats(symbol, TRUE)
  
  #drawChart(toMinuteBars(symbol, 1440))
  
}


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
