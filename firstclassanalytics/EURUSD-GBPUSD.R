# Introduction to R
# Copyright 2014 by Michael Grogan
# Tools -Install Packages

# Set working directory to where csv file is located
setwd("/datascience/projects/firstclassanalytics")

# Read the data
mydata<- read.csv("C:\\Users\\Michael Grogan\\Documents\\Current Documents\\Workspace\\Computer Programming\\R\\pairstradingeurgbp.csv")
attach(mydata)
# The following object is masked _by_ .GlobalEnv:
  
  X

# First Differences - lneurusd (dependent variable) and lngbpusd (independent variables)
library(tseries)

#‘tseries’ version: 0.10-32

#‘tseries’ is a package for time series analysis and computational finance.

#See ‘library(help="tseries")’ for details.

#Warning message:
#  package ‘tseries’ was built under R version 3.1.2 
adf.test(lneurusd)

Augmented Dickey-Fuller Test

data:  lneurusd
Dickey-Fuller = -2.4415, Lag order = 5, p-value = 0.3917
alternative hypothesis: stationary

adf.test(lngbpusd)

Augmented Dickey-Fuller Test

data:  lngbpusd
Dickey-Fuller = -2.2433, Lag order = 5, p-value = 0.4745
alternative hypothesis: stationary

adf.test(lneurusd_firstdifference)

Augmented Dickey-Fuller Test

data:  lneurusd_firstdifference
Dickey-Fuller = -4.8328, Lag order = 5, p-value = 0.01
alternative hypothesis: stationary

Warning message:
  In adf.test(lneurusd_firstdifference) :
  p-value smaller than printed p-value
adf.test(lngbpusd_firstdifference)

Augmented Dickey-Fuller Test

data:  lngbpusd_firstdifference
Dickey-Fuller = -5.4409, Lag order = 5, p-value = 0.01
alternative hypothesis: stationary

Warning message:
  In adf.test(lngbpusd_firstdifference) :
  p-value smaller than printed p-value

# OLS regression - lneurusd (dependent variable) and lngbpusd (independent variables)
olsreg <- lm(lneurusd ~ lngbpusd)
summary(olsreg)

Call:
  lm(formula = lneurusd ~ lngbpusd)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.051259 -0.015615 -0.000278  0.018327  0.049544 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.75102    0.02982  -25.18   <2e-16 ***
  lngbpusd     2.11010    0.06846   30.82   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.02334 on 177 degrees of freedom
Multiple R-squared:  0.843,	Adjusted R-squared:  0.8421 
F-statistic: 950.1 on 1 and 177 DF,  p-value: < 2.2e-16

# OLS is said to be super-consistent if cointegration holds.

# Plotting data
plot (lneurusd)
plot (lneurusd_firstdifference)
plot (lngbpusd)
plot (lngbpusd_firstdifference)

