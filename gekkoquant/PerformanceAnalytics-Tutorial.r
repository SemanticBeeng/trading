library("PerformanceAnalytics") 

data(managers)
head(managers)

dim(managers)
managers.length = dim(managers)[1]
colnames(managers)

manager.col = 1
peers.cols = c(2,3,4,5,6)
indexes.cols = c(7,8)
Rf.col = 10

#factors.cols = NA
trailing12.rows = ((managers.length - 11):managers.length)
trailing12.rows

trailing36.rows = ((managers.length - 35):managers.length)
trailing60.rows = ((managers.length - 59):managers.length)
#assume contiguous NAs - this may not be the way to do it na.contiguous()?

#frInception.rows = (length(managers[,1]) - length(managers[,1][!is.na(managers[,1])

charts.PerformanceSummary(managers[,c(manager.col,indexes.cols)], colorset=rich6equal)
                          
t(table.CalendarReturns(managers[,c(manager.col,indexes.cols)]))

table.Stats(managers[,c(manager.col,peers.cols)])

                                                                                 
chart.Boxplot(managers[ trailing36.rows, c(manager.col, peers.cols, indexes.cols)], main = "Trai")

layout(rbind(c(1,2),c(3,4)))
chart.Histogram(managers[,1,drop=F], main = "Plain", methods = NULL)
chart.Histogram(managers[,1,drop=F], main = "Density", breaks=40, methods = c("add.density", "add.normal"))

chart.Histogram(managers[,1,drop=F], main = "Skew and Kurt", methods = c("add.centered", "add.rug"))
chart.Histogram(managers[,1,drop=F], main = "Risk Measures", methods = c("add.risk"))                                                                                 

chart.RiskReturnScatter(managers[trailing36.rows,1:8], Rf=.03/12, main = "Trailing 36-Month Performance")

charts.RollingPerformance(managers[, c(manager.col, peers.cols, indexes.cols)], 
                          Rf=.03/12, colorset = c("red", rep("darkgray",5), "orange", "green"), lwd = 2)

chart.RelativePerformance(managers[ , manager.col, drop = FALSE], 
                          managers[ , c(peers.cols, 7)], 
                          colorset = tim8equal[-1], lwd = 2, legend.loc = "topleft")

chart.RelativePerformance(managers[ , c(manager.col, peers.cols) ], 
                          managers[, 8, drop=F], 
                          colorset = rainbow8equal, lwd = 2, legend.loc = "topleft")

table.CAPM(managers[trailing36.rows, c(manager.col, peers.cols)],
           managers[ trailing36.rows, 8, drop=FALSE],
           Rf = managers[ trailing36.rows, Rf.col, drop=FALSE])

                                                                                 