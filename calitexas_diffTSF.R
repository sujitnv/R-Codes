OrigData <- read.csv("C:/Users/Sujit pc/Desktop/R/TimeSeriesForecasting/california texas.csv",header = TRUE,stringsAsFactors = FALSE)
str(OrigData)
OrigData$MonthYear <- as.Date(asd$MonthYear,format="%d/%m/YYYY")
OrigDatats <- ts(OrigData$Count.of.Orders,frequency = 12,start = c(2008,01,01),end = c(2012,12,01))
plot(OrigDatats)
Origfit <- HoltWinters(OrigDatats)
Origfitfc <- forecast(Origfit,h=24)
plot(Origfitfc)
write.csv(Origfitfc,"forecast_1.csv",row.names = FALSE)
getwd()
Origfitfc$fitted
plot(residuals(Origfitfc))
acf(readcttsforecast0$residuals, lag.max=20)
Box.test(readcttsforecast0$residuals, lag=20, type="Ljung-Box")
ts.plot(OrigDatats,Origfitfc$fitted)


Origfit1 <- HoltWinters(OrigDatats,beta = FALSE,gamma = FALSE)
origfitfc1<-forecast(Origfit1,h=24)
plot(origfitfc1)
write.csv(origfitfc1,"forecast_2.csv",row.names = FALSE)
getwd()
origfitfc1$fitted
plot(residuals(origfitfc1))
acf(origfitfc1$residuals, lag.max=20)
Box.test(origfitfc1$residuals, lag=20, type="Ljung-Box")
ts.plot(OrigDatats,origfitfc1$fitted)

Origfit2 <- HoltWinters(OrigDatats,alpha = 0.8,beta = FALSE,gamma = FALSE)
origfitfc2<-forecast(Origfit2,h=24)
plot(origfitfc2)
write.csv(origfitfc2,"forecast_3.csv",row.names = FALSE)
getwd()
origfitfc2$fitted
plot(residuals(origfitfc2))
acf(origfitfc2$residuals, lag.max=20)
Box.test(origfitfc2$residuals, lag=20, type="Ljung-Box")
ts.plot(OrigDatats,origfitfc2$fitted)


OrigDatatsdiff<-diff(OrigDatats,differences = 3)
plot(OrigDatatsdiff)
acf(OrigDatatsdiff, lag.max=20)
acf(OrigDatatsdiff, lag.max=20, plot=FALSE)
pacf(OrigDatatsdiff, lag.max=20)
pacf(OrigDatatsdiff, lag.max=20,plot = FALSE)
origdataarima1 <- arima(OrigDatatsdiff, order=c(5,3,2))
origdataarima1
library(forecast)
origfitfc3 <- forecast.Arima(origdataarima1, h=24)
write.csv(origfitfc3,"forecast_4_2.csv",row.names = FALSE)
getwd()
origfitfc3
plot(origfitfc3)
plot(origfitfc3$residuals)
acf(origfitfc3$residuals, lag.max=20)
Box.test(origfitfc3$residuals, lag=20, type="Ljung-Box")
ts.plot(OrigDatats,origfitfc3$fitted)

dataAutoArima<-auto.arima(OrigDatats,ic="bic")
OrigDatatsdiff1<-diff(OrigDatats,differences = 1)
origdataarima2 <- arima(OrigDatatsdiff1, order=c(0,1,0))
origdataarima2
origfitfc4 <- forecast.Arima(origdataarima2, h=24)
write.csv(origfitfc4,"forecast_5.csv",row.names = FALSE)
origfitfc4
plot(origfitfc4)
plot(origfitfc4$residuals)
acf(origfitfc4$residuals, lag.max=20)
Box.test(origfitfc4$residuals, lag=20, type="Ljung-Box")
ts.plot(OrigDatats,origfitfc4$fitted)

origdataarima3 <- arima(OrigDatatsdiff, order=c(2,2,2))
origdataarima3
origfitfc5 <- forecast.Arima(origdataarima3, h=24)
write.csv(origfitfc5,"forecast_6.csv",row.names = FALSE)
origfitfc5
plot(origfitfc5)
plot(origfitfc5$residuals)
acf(origfitfc5$residuals, lag.max=20)
Box.test(origfitfc5$residuals, lag=20, type="Ljung-Box")
ts.plot(OrigDatats,origfitfc5$fitted)
