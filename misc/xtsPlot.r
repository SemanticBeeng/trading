# Source: http://www.r-bloggers.com/plot-xts-with-moving-average-panel/
#install.packages("xtsExtra", repos="http://R-Forge.R-project.org")

require(quantmod)
require(RColorBrewer)
#require(xtsExtra)  #if you get an error, see first line and install from r-forge

#use Mebane Faber tickers VTI, VEU, IEF, VNQ, DBC 
#as discussed in http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461
tckrs <- c("VTI", "VEU", "IEF", "VNQ", "DBC")

getSymbols(tckrs, from = "2000-01-01")

prices <- get(tckrs[1])[,6]
for (i in 2:length(tckrs)) {
  prices <- na.omit(merge(prices, get(tckrs[i])[,6]))
}

ma.panel <- function (index, x, col, ...) {
  #draw line for price
  default.panel(index, x, col, ...)
  #label each panel with first 3 characters of column name
  mtext(substr(colnames(x), 1, 3), side = 3, cex = 0.8, line = -2.5, adj = 0.5, col = col)
  #now get n=200 moving average
  ma <- runMean(x, n = 200)
  #add the moving average line
  default.panel(index, ma, col="indianred1", ...)
  abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3,lwd=0.5)
  #abline(h=par("yaxp")[1], col="black")
}

plot.xts(prices, 
         las = 1,   #no rotate for y axis tickmark labels
         panel = ma.panel,  #do the moving average panel to overlay moving average lines
         auto.grid = FALSE, #get some extra contol of grid
         col = brewer.pal("Set1", n = 7)[c(2:5,7)],
         main = NA,
         blocks = list(start.time = "2007-12-01", end.time = "2009-06-01", col="lightblue"))
title(main = "ETFs (www.mebanefaber.com) and 200 day Moving Average", adj = 0.05, outer = TRUE, line = -2)
mtext("source: Yahoo! Finance", outer = TRUE, side = 1, adj = 0.94, font = 1, cex = 0.7, line = -1)


#not sure why you would want to do something like this
#but for example purposes
#we can change the layout radically with plot.xts
plot.xts(prices, 
         screens = c(1,2,4,3,5),
         layout.screens = matrix(c(1,1,2,2,3,3,4,5), nrow = 2, byrow = FALSE),  #to see how the matrix works print the matrix print(matrix(c(1,1,2,2,3,3,4,5), nrow = 2, byrow = FALSE))
         ylim = matrix(c(range(prices[,c(1,2,4)]),range(prices[,c(1,2,4)]),range(prices[,c(1,2,4)]),range(prices[,3]),range(prices[,5])),
                       byrow=TRUE,ncol=2),
         las = 1,   #no rotate for y axis tickmark labels
         panel = ma.panel,  #do the moving average panel to overlay moving average lines
         auto.grid = FALSE, #get some extra contol of grid
         col = brewer.pal("Set1", n = 7)[c(2:5,7)],
         main = NA,
         minor.ticks = FALSE,
         major.format = "%Y",
         blocks = list(start.time = "2007-12-01", end.time = "2009-06-01", col="lightblue"))
title(main = "ETFs (www.mebanefaber.com) and 200 day Moving Average", adj = 0.05, outer = TRUE, line = -2)
mtext("source: Yahoo! Finance", outer = TRUE, side = 1, adj = 0.94, font = 1, cex = 0.7, line = -1)


plot.xts(prices, 
         screens = c(1,2,4,3,5),
         layout.screens = matrix(c(1,1,2,2,3,3,4,5,4,5), nrow = 2, byrow = FALSE),  #to see how the matrix works print the matrix print(matrix(c(1,1,2,2,3,3,4,5,4,5), nrow = 2, byrow = FALSE))
         ylim = matrix(c(range(prices[,c(1,2,4)]),range(prices[,c(1,2,4)]),range(prices[,c(1,2,4)]),range(prices[,3]),range(prices[,5])),
                       byrow=TRUE,ncol=2),
         las = 1,   #no rotate for y axis tickmark labels
         panel = ma.panel,  #do the moving average panel to overlay moving average lines
         auto.grid = FALSE, #get some extra contol of grid
         col = brewer.pal("Set1", n = 7)[c(2:5,7)],
         main = NA,
         minor.ticks = FALSE,
         major.format = "%Y",
         blocks = list(start.time = "2007-12-01", end.time = "2009-06-01", col="lightblue"))
title(main = "ETFs (www.mebanefaber.com) and 200 day Moving Average", adj = 0.05, outer = TRUE, line = -2)
mtext("source: Yahoo! Finance", outer = TRUE, side = 1, adj = 0.94, font = 1, cex = 0.7, line = -1)

