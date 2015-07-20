#Source: http://www.r-bloggers.com/plot-xts-is-wonderful/
#install.packages("xtsExtra", repos="http://R-Forge.R-project.org")

require(PerformanceAnalytics)
#require(xtsExtra)  #if you get error, please install xtsExtra from r-forge as shown in the top line
require(RColorBrewer)

#function add alpha or transparency to colors
addalpha <- function(cols,alpha=180) {
  rgbcomp <- col2rgb(cols)
  rgbcomp[4] <- alpha
  return(rgb(rgbcomp[1],rgbcomp[2],rgbcomp[3],rgbcomp[4],maxColorValue=255))
}

#replicate example provided in chart.TimeSeries with plot.xts

# These are start and end dates, formatted as xts ranges.
## http://www.nber.org-cycles.html
cycles.begin.dates<-c("1857-06",
                      "1860-10",
                      "1865-04",
                      "1869-06",
                      "1873-10",
                      "1882-03",
                      "1887-03",
                      "1890-07",
                      "1893-01",
                      "1895-12",
                      "1899-06",
                      "1902-09",
                      "1907-05",
                      "1910-01",
                      "1913-01",
                      "1918-08",
                      "1920-01",
                      "1923-05",
                      "1926-10",
                      "1929-08",
                      "1937-05",
                      "1945-02",
                      "1948-11",
                      "1953-07",
                      "1957-08",
                      "1960-04",
                      "1969-12",
                      "1973-11",
                      "1980-01",
                      "1981-07",
                      "1990-07",
                      "2001-03",
                      "2007-12"
)

cycles.end.dates<-c("1858-12",
                    "1861-06",
                    "1867-12",
                    "1870-12",
                    "1879-03",
                    "1885-05",
                    "1888-04",
                    "1891-05",
                    "1894-06",
                    "1897-06",
                    "1900-12",
                    "1904-08",
                    "1908-06",
                    "1912-01",
                    "1914-12",
                    "1919-03",
                    "1921-07",
                    "1924-07",
                    "1927-11",
                    "1933-03",
                    "1938-06",
                    "1945-10",
                    "1949-10",
                    "1954-05",
                    "1958-04",
                    "1961-02",
                    "1970-11",
                    "1975-03",
                    "1980-07",
                    "1982-11",
                    "1991-03",
                    "2001-11",
                    "2009-06"
)
# Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
risk.dates = c(
  "1987-10-19",
  "1994-02-01",
  "1997-07-01",
  "1998-08-17",
  "1998-09-23",
  "2000-07-01",
  "2011-09-11")
risk.labels = c(
  "Black Monday",
  "Bond Crash",
  "Asian Crisis",
  "Russian Crisis",
  "LTCM",
  "Tech Bubble",
  "Sept 11")
data(edhec)

R=edhec[,1:3,drop=FALSE]
Return.cumulative = cumprod(1+R) - 1

plot.xts(Return.cumulative, main="Default plot.xts Chart")  #using all the defaults
#png("chartTimeSeries.png",width=640,height=567,units="px")
plot.xts(Return.cumulative, screens=1, #screens=1 probably most appropriate for this application
         blocks = list(   #get blocks to plot from above set dates in list form
           start.time=paste(cycles.begin.dates,"-01",sep=""),
           end.time=paste(cycles.end.dates,"-01",sep=""),
           col = "lightblue"),
         events = list(  #get events to plot from above risk.date in list form
           time = risk.dates,
           label = risk.labels,
           col = "red"), 
         lwd = 2,
         legend.loc = "bottomright", auto.legend=TRUE,
         main="EDHEC Style Indexes")
#dev.off()
#png("chartTimeSeries with extra.png",width=640,height=567,units="px")
plot.xts(Return.cumulative,
         screens=c(1,2,2), #plot 1st series in 1st panel and 2nd and 3rd series in 2nd panel
         layout.screens=c(1,2,2),  #just as an example change the layout so 1st panel is 1/3 of area and 2nd is bottom 2/3
         blocks = list(  #set up blocks for recessions
           start.time=paste(cycles.begin.dates,"-01",sep=""),
           end.time=paste(cycles.end.dates,"-01",sep=""),
           col = "lightblue"),
         events = list(  #add some event lines
           time = risk.dates,
           label = risk.labels,
           offset = c(0.5,0.5,0.5,0.5,-0.5,0.5),  #collision control can also use yadj as in next line
           y.adj = c(0,0.5),  #with recycling will alternate position for each label
           col = "purple"),  #don't know why you would use purple but in case you do
         lwd = 2,
         legend.loc = "bottomright", auto.legend=TRUE,
         main="EDHEC Style Indexes")
#dev.off()

#for a little more advanced application we can start to do charts.PerformanceSummary style plot.xts 
cumulreturn.panel  <- function(...) {
  mtext("Cumulative Return", side=1, adj=1, line=-3)
  default.panel(...)
  abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
  abline(h=0, col="black")
}

es.panel <- function(index,x,...) {
  mtext("Expected Shortfall", side=1, adj=1, line=-3) 
  default.panel(index,x,...)
  #silly to do this but if we wanted just certain points like every 4 months we could do something like this
  #default.panel(index[seq(1,NROW(index),by=4)],coredata(x[seq(1,NROW(index),by=4)]),...)
  #abline(h=0, col="black")
  abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
  abline(h=par("yaxp")[1], col="black")
}

drawdown.panel <-  function(index,x,...) {  
  mtext("Drawdown", side=1, adj=1, line=-2) 
  default.panel(index,x,...)
  #silly to do this but if we wanted just certain points like every 4 months we could do something like this
  #default.panel(index[seq(1,NROW(index),by=4)],coredata(x[seq(1,NROW(index),by=4)]),...)
  #abline(h=0, col="black")
  abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
  abline(h=par("usr")[3], col="black")
}

#get some risk measurements to add for Performance Summary style plot
Risk.drawdown <- Drawdowns(R)

Risk.es <- rollapplyr(R,FUN="ES",width=36,p=0.95,na.pad=TRUE)
#take care of NA with 0 at beginning and interpolation at end
Risk.es <- apply(Risk.es,MARGIN=2,FUN=na.fill,fill=c(0,"extend"))
#something wrong with returned value from apply.rolling so indexes don't work properly
data.to.plot <- as.xts(cbind(coredata(Return.cumulative),Risk.es,coredata(Risk.drawdown)),order.by=index(edhec))

#png("chartsPerformanceSummary.png",width=640,height=600,units="px")
plot.xts(data.to.plot,
         lwd = c(2,1,1), #do this to show how arguments are recycled
         col = brewer.pal(n=9,"PuBu")[c(8,4,6)],
         auto.grid = FALSE, #usually auto.grid works just fine but turn off for example purposes
         las = 1,yax.loc = "right",  # yax.loc could also be flip or left in this case
         screens = c(1,1,1,2,2,2,3,3,3),  #3 series for each so first 3 in panel 1 second 3 in panel 2 and last 3 in panel 3
         layout.screens = c(1,1,2,3), #panel 1 take up first 2 of 4 so 50% and panels 2 and 3 each 25%
         bty = "n", 
         panel = c(cumulreturn.panel,es.panel,drawdown.panel), #c(first.panel,"auto"), #panel cycles through by panel rather than each series
         ylab = NA, major.format = "%b %Y", minor.ticks = FALSE,
         legend.loc = c("topleft",NA,NA), auto.legend = TRUE,
         legend.pars = list(bty = "n", horiz=TRUE),  #make legend box transparent
         cex.axis = 0.9, 
         main = NA)

title(main = "Performance Summary of EDHEC Indexes", adj = 0, outer = TRUE, line = -1.5)
#dev.off()

#just playing around with type="h"
#think will only work if all colors very different
plot.xts(Risk.drawdown["2000::2001",],type="h",screens=1,lwd=c(10,25,15),
         col=apply(as.matrix(brewer.pal(n=9,"PuBu")[c(8,4,6)]),MARGIN=1,FUN=addalpha,alpha=120))

