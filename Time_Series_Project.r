library(forecast)
library(tseries)
library(quantmod)
library(nnet)

data <- read.csv("MSFT.csv", header = T)

msft <- ts(data[,6],frequency = 1) #Frequency is set to 1 because it is daily
#6 refers to the column of data

msft.train <- window(msft, end=c(8000,1))
msft.test <- window(msft,start=c(8001,1))

#The following is a plot showing the training and testing ranges
plot(msft, main="Microsoft Stock 1986-2021", ylab="Price", xlab="Days")
lines(msft.train, col="blue")
lines(msft.test, col="green")
legend("bottomright", col=c("blue", "green"), lty=1, legend=c("Training", "Testing"))

#Then we preform the Augmented Dicky-Fuller Test, ADF to determine stationarity
adf.test(msft.train, alternative="stationary")

#We then differencing the time series
plot(diff(msft), type="l",main="MSFT 1986-2021",ylab="Price Differences",xlab="Days")

adf.test(diff(msft.train), alternative="stationary")

#Now we add the parameters for the ARIMA model
acf(diff(msft.train))
pacf(diff(msft.train))

#Now we need to find the BIC in order to use as our p in the ARIMA model

model1 <- Arima(msft.train,order=c(0,1,0), include.constant = T)
model1

tsdisplay(residuals(model1), lag.max = 45, main='(0,1,0) Model Residuals')

checkresiduals(model1)

modelauto <-auto.arima((msft.train), seasonal=FALSE)
modelauto

plot(forecast(model1, h=252), main="Arima(0,1,0) using Multi-Steps Forecast", ylab="Price", xlab="Date")
lines(msft.test, lty=3)

accuracy(forecast(model1,h=252), msft.test) [2,1:6]

model2 <- Arima(msft.test, model = model1)$fitted
plot(msft.train, main="ARIMA(0,1,0) using One-Step Forecast without Re-Estimation", ylab="Price", xlab="Date", ylim=c
     (min(msft), max(msft)))
lines(model2, col="green")
lines(msft.test, lty=3)
legend("bottomright", col = "green", lty = 1, legend = "Forecasted Price")

accuracy(model2, msft.test)[1,1:5]
