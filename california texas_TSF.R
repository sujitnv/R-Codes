readct <- read.csv("C:/Users/Sujit pc/Desktop/R/TimeSeriesForecasting/california texas.csv",header = TRUE,stringsAsFactors = FALSE)
dim(readct)
str(readct)
readct$MonthYear <- as.Date(readct$MonthYear,format = "%d-%m-%Y")
str(readct)
readctts <- ts(readct$Count.of.Orders,frequency = 12,start = c(2008,01,01),end = c(2013,11,01))
plot(readctts)
readcttslog <- log(readctts)
plot(readcttslog)
library(TTR)
calsma3<-SMA(readctts,n=8)
decompcal<-decompose(readctts)
plot(decompcal)
plot(calsma3)

install.packages("forecast")
library(forecast)
modelfit <- HoltWinters(readctts,beta = FALSE,gamma = FALSE)
modelfit <- HoltWinters(readctts)
modelfit
modelfit$SSE
modelfit$fitted
plot(modelfit$fitted)
plot(modelfit)
readcttsforecast0 <- forecast.HoltWinters(modelfit,h=25)
acf(readcttsforecast0$residuals, lag.max=20)
Box.test(readcttsforecast0$residuals, lag=20, type="Ljung-Box")
plot(readcttsforecast0)
ts.plot(readcttsdiff,readcttsforecast0$fitted)
readcttsforecast0$fitted
write.csv(readcttsforecast0$fitted,file ="forecasted_1.csv",row.names = FALSE)
getwd()

modelfitExpSmmothing <- HoltWinters(readctts,alpha = 0.8,beta = FALSE,gamma = FALSE)
modelfitExpSmmothing
modelfitExpSmmothing$SSE
modelfitExpSmmothing$fitted
plot(modelfitExpSmmothing$fitted)
plot(modelfitExpSmmothing)
readcttsforecast <- forecast.HoltWinters(modelfitExpSmmothing,h=25)
plot(readcttsforecast$residuals)
acf(readcttsforecast$residuals, lag.max=20)
Box.test(readcttsforecast$residuals, lag=20, type="Ljung-Box")
ts.plot(readcttsdiff,readcttsforecast$fitted)

readcttsdiff<-diff(readctts,differences = 2)
plot(readcttsdiff)
acf(readcttsdiff, lag.max=20)
acf(readcttsdiff, lag.max=20, plot=FALSE)
pacf(readcttsdiff, lag.max=20)
pacf(readcttsdiff, lag.max=20,plot = FALSE)
readcttsarima <- arima(readcttsdiff, order=c(3,2,2))
readcttsarima
library("forecast")
readctttsforecasts <- forecast.Arima(readcttsarima, h=5)
readctttsforecasts
plot(readctttsforecasts)
plot(readctttsforecasts$residuals)
acf(readctttsforecasts$residuals, lag.max=20)
Box.test(readctttsforecasts$residuals, lag=20, type="Ljung-Box")
ts.plot(readcttsdiff,readctttsforecasts$fitted)
plot(readctttsforecasts)

calitexastsds1arima1<-auto.arima(readcttsdiff,ic="bic")
calitexastsds1arima1
readcttsarima1 <- arima(readcttsdiff, order=c(2,0,0))
readcttsarima1
library("forecast")
readctttsforecasts1 <- forecast.Arima(readcttsarima1, h=5)
readctttsforecasts1
plot(readctttsforecasts1)
plot(readctttsforecasts1$residuals)
acf(readctttsforecasts1$residuals, lag.max=20)
Box.test(readctttsforecasts1$residuals, lag=20, type="Ljung-Box")
ts.plot(readcttsdiff,readctttsforecasts1$fitted)

readcttsarima2 <- arima(readcttsdiff, order=c(2,2,2))
readcttsarima2
library("forecast")
readctttsforecasts2 <- forecast.Arima(readcttsarima2, h=5)
readctttsforecasts2
plot(readctttsforecasts2)
plot(readctttsforecasts2$residuals)
acf(readctttsforecasts2$residuals, lag.max=20)
Box.test(readctttsforecasts2$residuals, lag=20, type="Ljung-Box")
ts.plot(readcttsdiff,readctttsforecasts2$fitted)



write.csv(readcttsforecast,file ="forecastedgF.csv",row.names = FALSE)
getwd()
readcttsforecast
View(readcttsforecast)
plot(readcttsforecast)
plot(readcttsforecast$residuals)
hist(readcttsforecast$residuals)
plot(readcttsforecast$fitted)
accuracy(readcttsforecast,testdatats)
#plot(readcttsforecast,ylim=c(0,150))
#line(treadct(1:120))
hist(readcttsforecast,probability = TRUE,ylim = c(0,20),breaks = 1.5,ts07em0448/9)





testdata <- read.csv("C:/Users/Archents/Desktop/TSF/New folder/TSFPartition/california texas_Test.csv",header = TRUE,stringsAsFactors = FALSE)
dim(testdata)
str(testdata)
testdata$MonthYear <- as.Date(testdata$MonthYear,format="%d-%m-%Y")
str(testdata)
testdatats <- ts(testdata$Count.of.Orders,frequency = 12,start = c(2013,12,01),end = c(2015,12,01))
testdataforecast <- forecast(modelfit,testdatats)
