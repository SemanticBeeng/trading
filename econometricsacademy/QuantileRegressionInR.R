# Quantile Regression in R
# Copyright 2013 by Ani Katchova

# install.packages("quantreg")
library(quantreg)

setwd("/datascience/projects/trading/econometricsacademy")
mydata<- read.csv("quantile_health.csv")
attach(mydata)

# Define variables
Y <- cbind(totexp)
X <- cbind(suppins, totchr, age, female, white)

# Descriptive statistics
summary(Y)
summary(X)

# OLS regression
olsreg <- lm(Y ~ X, data=mydata)
summary(olsreg)

# Quantile regression
quantreg25 <- rq(Y ~ X, data=mydata, tau=0.25)
summary(quantreg25)

quantreg50 <- rq(Y ~ X, data=mydata, tau=0.5)
summary(quantreg50)

quantreg75 <- rq(Y ~ X, data=mydata, tau=0.75)
summary(quantreg75)

# Simultaneous quantile regression
quantreg2575 <- rq(Y ~ X, data=mydata, tau=c(0.25, 0.75))
summary(quantreg2575)

# ANOVA test for coefficient differences
anova(quantreg25, quantreg75)

# Plotting data
quantreg.all <- rq(Y ~ X, tau = seq(0.05, 0.95, by = 0.05), data=mydata)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)
