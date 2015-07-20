# Sum the thrird column by 30 second intervals
# Source http://r.789695.n4.nabble.com/Aggregating-time-series-to-every-30sec-td3500318.html

r.xts <- read.table(textConnection("
2011-05-05 09:29:31.929 0.5
2011-05-05 09:29:59.929 0.5
2011-05-05 09:30:04.929 0.1
2011-05-05 09:30:14.907 0.2
2011-05-05 09:30:19.917 0.3
2011-05-05 09:30:35.114 1.1
2011-05-05 09:30:45.193 1.2
2011-05-05 09:31:12.417 0.4"))

r.xts <- xts(r.xts$V3, as.POSIXct(paste(r.xts$V1,r.xts$V2))); r.xts

ep <- endpoints(r.xts, 'secs', k=30); ep

ps <- period.sum(r.xts, ep); ps

align.time(ps, 30) 

