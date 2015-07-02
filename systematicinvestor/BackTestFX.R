###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(file('sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')   

EURUSD = getSymbols.fxhistoricaldata('EURUSD', 'hour', auto.assign = F, download=T)
SPY = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)

#*****************************************************************
# Reference intraday period
#****************************************************************** 
plota(EURUSD['2012:03:06 10::2012:03:06 21'], type='candle', main='EURUSD on 2012:03:06 from 10 to 21')

#*****************************************************************
# Plot hourly and daily prices on the same chart
#******************************************************************     
# two Y axis plot   
dates= '2012:01:01::2012:01:11'
y = SPY[dates]
plota(y, type = 'candle', LeftMargin=3)

y = EURUSD[dates]
plota2Y(y, ylim = range(OHLC(y), na.rm=T), las=1, col='red', col.axis = 'red')
plota.ohlc(y, col=plota.candle.col(y))
plota.legend('SPY(rhs),EURUSD(lhs)', 'black,red', list(SPY[dates],EURUSD[dates]))

#*****************************************************************
# Universe: Currency Majors
# http://en.wikipedia.org/wiki/Currency_pair    
#****************************************************************** 
tickers = spl('EURUSD,USDJPY,GBPUSD,AUDUSD,USDCHF,USDCAD')

#*****************************************************************
# Daily Backtest
#****************************************************************** 
data <- new.env()
getSymbols.fxhistoricaldata(tickers, 'day', data, download=T)
bt.prep(data, align='remove.na', dates='1990::')

prices = data$prices   
n = len(tickers)  
models = list()

# Equal Weight
data$weight[] = NA
data$weight[] = ntop(prices, n)
models$equal.weight = bt.run.share(data, clean.signal=F)

# Timing by M. Faber
sma = bt.apply.matrix(prices, SMA, 200)
data$weight[] = NA
data$weight[] = ntop(prices, n) * (prices > sma)
models$timing = bt.run.share(data, clean.signal=F)

# Report
models = rev(models)
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models)

#*****************************************************************
# Intraday Backtest
#****************************************************************** 
data <- new.env()    
getSymbols.fxhistoricaldata(tickers, 'hour', data, download=T)  
bt.prep(data, align='remove.na', dates='1990::')

prices = data$prices   
n = len(tickers)  
models = list()

# Equal Weight
data$weight[] = NA
data$weight[] = ntop(prices, n)
models$equal.weight = bt.run.share(data, clean.signal=F)

# Timing by M. Faber
sma = bt.apply.matrix(prices, SMA, 200)
data$weight[] = NA
data$weight[] = ntop(prices, n) * (prices > sma)
models$timing = bt.run.share(data, clean.signal=F)

# Report
models = rev(models)
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models)  

