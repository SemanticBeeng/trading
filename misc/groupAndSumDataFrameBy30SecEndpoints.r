# Sum the thrird column by 30 second intervals
# Source http://r.789695.n4.nabble.com/Aggregating-time-series-to-every-30sec-td3500318.html

r.xts <- read.table(textConnection("
2011-05-05 09:29:31.929 0.5
2011-05-05 09:29:59.929 0.5
2011-05-05 09:30:04.929 0.3264757
2011-05-05 09:30:14.907 0.0934498
2011-05-05 09:30:19.917 0.8956367
2011-05-05 09:30:35.114 1.6632110
2011-05-05 09:30:45.193 1.1666715
2011-05-05 09:31:12.417 0.2861861"))

r.xts <- xts(r.xts$V3, as.POSIXct(paste(r.xts$V1,r.xts$V2)))
r.xts

endpoints(r.xts, 'secs', k=30)
period.sum(r.xts,endpoints(r.xts,'secs', k=30))

align.time(period.sum(r.xts,endpoints(r.xts,'secs',k=30)),30) 
