housingdata <- read.csv("C:/Users/Sujit pc/Desktop/R/LinearRegression/housingggg.csv",header = TRUE,stringsAsFactors = FALSE)
head(housingdata)
tail(housingdata)
plot(housingdata)
names(housingdata)
str(housingdata)
summary(housingdata)
hist(housingdata$price)
qqnorm(housingdata$price)
qqline(housingdata$price)
cor(housingdata$lotsize,housingdata$price)
hist(housingdata$lotsize)
boxplot(housingdata$price)
install.packages("Hmisc")
library(Hmisc)
describe(housingdata$lotsize)
indexes <- sample(1:nrow(housingdata),size = 0.8*nrow(housingdata),replace = FALSE)
indexes
train <- housingdata[indexes,]
dim(train)
test <- housingdata[-indexes,]
dim(test)
write.csv(test,file = "newhousingtestdata.csv",row.names = FALSE)
getwd()
setwd()
fittttt <- lm(price~.,data = train)
summary(fittttt)
vif(fittttt)
outlierTest(fittttt)
train <- train[-c(233),]
mean(train$medv)
fit222 <- lm(price~prefarea+garagepl+airco+gashw+fullbase+recroom+driveway,data = train)
summary(fit222)
plot(fit222)
abline(fit222)

summary(fit222)
coef(fit222)
vif(fit222)
mean(fit222$residuals)
hist(fit222$residuals,xlab = "residuals",ylab = "histogram of residuals")
qqnorm(fit222$residuals,main = "normal probability plot",pch=19)
qqline(fit222$residuals)
plot(fit222$fitted.values,fit222$residuals,pch=19)
abline(h=0)
plot(train$lotsize,fit222$residuals,main = "residuals vs predictors",xlab = "lower status",ylab = "residuals",pch=19)
abline(h=0)
plot(train$bathrms,fit222$residuals,main = "residuals vs predictors",xlab = "lower status",ylab = "residuals",pch=19)
abline(h=0)

plot(train$stories,fit222$residuals,main = "residuals vs predictors",xlab = "lower status",ylab = "residuals",pch=19)
abline(h=0)

plot(train$driveway,fit222$residuals,main = "residuals vs predictors",xlab = "lower status",ylab = "residuals",pch=19)
abline(h=0)

plot(train$fullbase,fit222$residuals,main = "residuals vs predictors",xlab = "lower status",ylab = "residuals",pch=19)
abline(h=0)


plot(fit222$residuals,fit222$fitted.values,xlab = "residuaals",ylab = "fitted values",pch=19)
abline(h=0)


datatest <- predict.lm(fit222,test)
datatest
head(datatest)
head(test$price)
SSE <- sum((test$price - datatest) ^ 2)
SST <- sum((test$price - mean(test$price)) ^ 2)
1 - SSE/SST
write.csv(datatest,file = "newhousingtesteddata.csv",row.names = FALSE)
getwd()
plot(datatest)
qqnorm(datatest)
qqline(datatest)
hist(datatest)

modelfithousingdata <- lm(price~lotsize+bathrms+stories+driveway+recroom+fullbase+gashw+airco+garagepl+prefarea,data = train)
summary(modelfithousingdata)
vif(modelfithousingdata)
outlierTest(modelfithousingdata)
housingdata <- housingdata[-c(239),]
modelfithousingdata11 <- lm(price~lotsize+bathrms+stories+airco+garagepl+prefarea,data = train)
summary(modelfithousingdata11)
vif(modelfithousingdata11)

mean(modelfithousingdata$residuals)
hist(modelfithousingdata$residuals,xlab = "residuals",ylab = "histogram of residuals")
qqnorm(modelfithousingdata$residuals,main = "normal probability plot",pch=16)
qqline(modelfithousingdata$residuals)
plot(modelfithousingdata$fitted.values,modelfit$residuals,pch=19)
abline(h=0)
modelfithousingdata$fitted.values

datatest <- predict.lm(modelfithousingdata,test)
datatest
head(datatest)
head(test$price)
SSE <- sum((test$price - datatest) ^ 2)
SST <- sum((test$price - mean(test$price)) ^ 2)
1 - SSE/SST
write.csv(datatest,file = "qqqhousingtesteddata.csv",row.names = FALSE)
getwd()
coefficients(modelfithousingdata)


cor(housingdata$lotsize,housingdata$price)
cor(housingdata$bedrooms,housingdata$price)
cor(housingdata$bathrms,housingdata$price)
cor(housingdata$stories,housingdata$price)
cor(housingdata$driveway,housingdata$price)
cor(housingdata$recroom,housingdata$price)
cor(housingdata$fullbase,housingdata$price)
cor(housingdata$gashw,housingdata$price)
cor(housingdata$airco,housingdata$price)
cor(housingdata$garagepl,housingdata$price)
cor(housingdata$prefarea,housingdata$price)



modelfithousingdata <- lm(price~lotsize+bathrms+stories+driveway+recroom+fullbase+gashw+airco+garagepl+prefarea,data = train)
summary(modelfithousingdata)
vif(modelfithousingdata)
outlierTest(modelfithousingdata)
housingdata <- housingdata[-c(239),]

modelfithousingdataSVN <- lm(price~airco+bathrms+driveway+fullbase+garagepl+lotsize+prefarea+stories,data = train)
summary(modelfithousingdataSVN)
vif(modelfithousingdataSVN)

mean(modelfithousingdataSVN$residuals)
hist(modelfithousingdataSVN$residuals,xlab = "residuals",ylab = "histogram of residuals")
qqnorm(modelfithousingdataSVN$residuals,main = "normal probability plot",pch=16)
qqline(modelfithousingdataSVN$residuals)

datatestSVN <- predict.lm(modelfithousingdataSVN,test)
datatestSVN
head(datatest)
head(test$price)
SSE <- sum((test$price - datatestSVN) ^ 2)
SST <- sum((test$price - mean(test$price)) ^ 2)
1 - SSE/SST
write.csv(datatestSVN,file = "svnhousingtesteddata.csv",row.names = FALSE)
cor(housingdata)
