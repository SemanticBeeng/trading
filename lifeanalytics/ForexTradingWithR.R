library(e1071)
library(nnet)
library(randomForest)
library(quantmod)
library(tseries)

#get data OHLC from csv file
raw<-read.delim2("EURUSD_day.csv",header=TRUE,sep=",")

#convert date
stripday<-strptime(raw$X.DATE ,format="%Y%m%d")
fxdata<-data.frame(stripday,raw)

fxdata$TIME<-NULL
fxdata$TICKER<-NULL
fxdata$DATE<-NULL
colnames(fxdata)<-c("Date","Open","Low","High","Close")

#write data to .csv
write.table(fxdata,"eurusd.csv",quote=FALSE,sep=",",row.names=FALSE)

##transform to an xts object
EURUSD<-as.xts(read.zoo("eurusd.csv",sep=",",format="%Y-%m-%d",header=T))

#setup Technical Indicators

myATR <- function(x) ATR(HLC(x))[,'atr']
mySMI <- function(x) SMI(HLC(x))[,'SMI']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
myBB <- function(x) BBands(HLC(x))[,'pctB']
myChaikinVol<-function(x)Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myMACD <- function(x) MACD(Cl(x))[,2]
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]
myEMA10 <- function(x) EMA(Cl(x),n=10)[,1]
myEMA20 <- function(x) EMA(Cl(x),n=20)[,1]
myEMA30 <- function(x) EMA(Cl(x),n=30)[,1]
myEMA50 <- function(x) EMA(Cl(x),n=50)[,1]
myEMA60 <- function(x) EMA(Cl(x),n=60)[,1]

data.model <- specifyModel(Delt(Cl(EURUSD)) ~
  myATR(EURUSD) + mySMI(EURUSD) + myADX(EURUSD) + myAroon(EURUSD)) +
  myBB(EURUSD) + myChaikinVol(EURUSD) + myCLV(EURUSD) + myEMA10(EURUSD) + 
  myEMA20(EURUSD) +myEMA30(EURUSD) + myEMA50(EURUSD) + myEMA60(EURUSD) +
  CMO(Cl(EURUSD)) + EMA(Delt(Cl(EURUSD))) +
  myVolat(EURUSD) + myMACD(EURUSD) + RSI(Cl(EURUSD)) +
  mySAR(EURUSD) + runMean(Cl(EURUSD)) + runSD(Cl(EURUSD)))


Tdata.train <- as.data.frame(modelData(data.model, data.window=c('2008-01-01','2010-01-01')))

Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2010-01-02','2010-11-01')))


# a very simple signal function
signals<-function(x) {
  
  if(x>=-0.005&&x<=0.005) {result<-"hold"} else
    if(x>0.005) {result<-"buy"} else
      if(x<-0.005) {result<-"sell"}
  
  result
}

#create class vector that holds TRAINING buy,sell,hold signals

class<-sapply(Tdata.train$Delt.Cl.EURUSD,signals)

#paste both to a new list that holds everything
traindata<-cbind(Tdata.train,class)

#remove Delt.Cl.EURUSD - not needed anymore. 
traindata$Delt.Cl.EURUSD<-NULL

#create class vector that  holds TESTING buy,sell,hold signals

class<-sapply(Tdata.eval$Delt.Cl.EURUSD,signals)

#paste to a new list that holds everything
testdata<-cbind(Tdata.eval,class)
testdata$Delt.Cl.EURUSD<-NULL

#get a summary of our traindata
summary(traindata)
