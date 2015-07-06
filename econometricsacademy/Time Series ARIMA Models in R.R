# Time Series ARIMA Models in R
# Copyright 2013 by Ani Katchova

# install.packages("tseries")
library(tseries)

mydata<- read.csv("timeseries_ppi.csv")
attach(mydata)

# Defining variables
Y <- ppi
d.Y <- diff(Y)
t <- yearqrt

# Descriptive statistics and plotting the data
summary(Y)
summary(d.Y)

plot(t,Y)
plot(d.Y)

# Dickey-Fuller test for variable
adf.test(Y, alternative="stationary", k=0)
adf.test(Y, alternative="explosive", k=0)

#summary(lm(dppi ~ lppi, na.action=na.omit))
#summary(lm(dppi ~ lppi + trend, na.action=na.omit))

# Augmented Dickey-Fuller test
adf.test(Y, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(d.Y, k=0)
adf.test(d.Y)


# ACF and PACF
acf(Y)
pacf(Y)

acf(d.Y)
pacf(d.Y)

# ARIMA(1,0,0) or AR(1)
arima(Y, order = c(1,0,0))

# ARIMA(2,0,0) or AR(2)
arima(Y, order = c(2,0,0))

# ARIMA(0,0,1) or MA(1)
arima(Y, order = c(0,0,1))

# ARIMA(1,0,1) or AR(1) MA(1)
arima(Y, order = c(1,0,1))

# ARIMA on differenced variable 
# ARIMA(1,1,0)
arima(d.Y, order = c(1,0,0))

# ARIMA(0,1,1)
arima(d.Y, order = c(0,0,1))

# ARIMA(1,1,1)
arima(d.Y, order = c(1,0,1))

# ARIMA(1,1,3)
arima(d.Y, order = c(1,0,3))

# ARIMA(2,1,3)
arima(d.Y, order = c(2,0,3))


# ARIMA(1,0,1) forecasting
mydata.arima101 <- arima(Y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=100)
plot (Y)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")

# ARIMA(1,1,1) forecasting
mydata.arima111 <- arima(d.Y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima111, n.ahead=100)
plot (d.Y)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")
