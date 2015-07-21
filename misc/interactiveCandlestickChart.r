library(quantmod)
library(googleVis)

prices = c("MSFT")
getSymbols(prices)

msft =data.frame(date = as.Date(index(MSFT)), open = 
                   as.numeric(MSFT$MSFT.Open),close = as.numeric(MSFT$MSFT.Close),
                 high = as.numeric(MSFT$MSFT.High),low = 
                   as.numeric(MSFT$MSFT.Low))

msft_n = msft[1:20,]

mplot = gvisCandlestickChart(msft_n,xvar = "date", low = "low", 
                             open = "open", close = "close", high = "high",options = list 
                             (legend = 'none', width = 900, title = "Microsoft stock Price"))
plot(mplot)
