skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot(skirtsseries)
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot(skirtsseriesdiff1)
skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot(skirtsseriesdiff2)

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot(kingtimeseriesdiff1)
acf(kingtimeseriesdiff1, lag.max=20)
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE)
pacf(kingtimeseriesdiff1, lag.max=20)
pacf(kingtimeseriesdiff1, lag.max=20,plot = FALSE)
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot(volcanodustseries)
acf(volcanodustseries, lag.max=20)
acf(volcanodustseries, lag.max=20, plot=FALSE)
pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1))
kingstimeseriesarima
library("forecast")
kingstimeseriesforecasts <- forecast.Arima(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot(kingstimeseriesforecasts)
plot(kingstimeseriesforecasts$residuals)
acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")


volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima
library(forecast)
volcanodustseriesforecast<-forecast.Arima(volcanodustseriesarima,h=31)
volcanodustseriesforecast
plot(volcanodustseriesforecast)
plot(volcanodustseriesforecast$residuals)
acf(volcanodustseriesforecast$residuals, lag.max=20)
Box.test(volcanodustseriesforecast$residuals, lag=20, type="Ljung-Box")

volcanodustseriesarima1 <- arima(volcanodustseries, order=c(2,0,3))
volcanodustseriesarima1
library(forecast)
volcanodustseriesforecast1<-forecast.Arima(volcanodustseriesarima1,h=31)
volcanodustseriesforecast1
plot(volcanodustseriesforecast1)
plot(volcanodustseriesforecast1$residuals)
acf(volcanodustseriesforecast1$residuals, lag.max = 30)
Box.test(volcanodustseriesforecast1$residuals,lag=30,type = "Ljung-Box")

auto.arima(volcanodustseries,ic="bic")
