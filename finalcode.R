#' ---
#' title: "BitcoinCash Code"
#' author: ""
#' date: "12/19/2018"
#' output: pdf_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
## ------------------------------------------------------------------------
library(dplyr)
library(forecast)
library(TSA)
bitcoin.data <- read.csv("/Users/tennyliu/Desktop/bitcoin_ts_removed.csv")
bitcoin.full <- read.csv("/Users/tennyliu/Desktop/bitcoin_ts.csv")

time.bitcoin.full <- as.ts(bitcoin.full$Close, start=c(17/11/1))
transformed.bitcoin.full <- log (time.bitcoin.full)

#time.bitcoin.data.full <- bitcoin.full %>% mutate(Date = as.Date(bitcoin.full$Date, format = '%m/%d/%Y')) 
#plot(time.bitcoin.data.full, type='l', main="Bitcoin Cash Price July 2017-Feb 2018")

time.bitcoin.data <- bitcoin.data %>% mutate(Date = as.Date(bitcoin.data$Date, format = '%m/%d/%Y')) 

plot(time.bitcoin.data, type='l', main="Bitcoin Cash Price July 2017-Feb 2018")

BoxCox.lambda(time.bitcoin.data)
transformed.time.bitcoin <-log(time.bitcoin.data$Close)
plot(transformed.time.bitcoin, type = 'l', main="Log-Transformed Bitcoin Time Series",ylab="Logged Closing Price", xlab="Time")
acf(transformed.time.bitcoin)
pacf(transformed.time.bitcoin)

#acf(transformed.time.bitcoint)
acf(diff(transformed.time.bitcoin,1), main="Series ACF of Taking First difference")
pacf(diff(transformed.time.bitcoin,1), main="Series PACF of Taking First difference")
eacf(diff(transformed.time.bitcoin,1))
plot(armasubsets(y=diff(transformed.time.bitcoin,1) , nar=8, nma=8, ar.method = "ols"))

#best fit based on AIC BIC
auto.arima(transformed.time.bitcoin, max.p=5, max.q=5)

#potential model ARIMA(2,1,2) accord. to eacf
model.1 <- arima(x=transformed.time.bitcoin, order=c(2,1,2), method="ML")

res.model.1 <- rstandard(model.1)
Box.test(res.model.1, lag=10, type = 'Ljung-Box', fitdf = 1)
plot(res.model.1,main="Residuals for ARIMA(2,1,2) Model")
abline(h=0)
runs(res.model.1)
qqnorm(res.model.1)
qqline(res.model.1)
hist(res.model.1,main="Residuals for ARIMA(2,1,2)")
shapiro.test(res.model.1)



#potential model ARIMA (0,1,2)  accord. to AIC/BIC 
model.2 <- arima(x=transformed.time.bitcoin, order=c(0,1,2), method="ML")

res.model.2 <- rstandard(model.2)
Box.test(res.model.2, lag=10, type = 'Ljung-Box', fitdf = 1)
plot(res.model.2,main="Residuals for ARIMA(0,1,2) Model")
abline(h=0)
runs(res.model.2)
qqnorm(res.model.2)
qqline(res.model.2)
hist(res.model.2,main="Residuals for ARIMA(0,1,2)")
shapiro.test(res.model.2)


#parameter estimation for model ARIMA(2,1,2)
arima(transformed.time.bitcoin, order=c(2,1,2), method='ML')#MLE
arima(transformed.time.bitcoin, order=c(2,1,2), method='CSS')#conditional least squares




#forecasting
plot(model.1, n.head=30, col='blue', type = 'l',xlab = 'Time', ylab= 'Logged closing price', main = 'Bitcoin Cash Prediction vs Actual Data')
lines(transformed.bitcoin.full, col='green', type='l')





#

