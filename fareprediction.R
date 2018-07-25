faredata <- read.csv("C:/Users/Sujit pc/Desktop/R/Fare.csv",header = TRUE,stringsAsFactors = FALSE)
View(faredata)
faredata <- faredata[-c(15)]
sapply(faredata, function(x) sum(is.na(x)))
sum(is.na(faredata))
hist(faredata$FARE)
cor(faredata)

View(faredata)

for(i in 1:nrow(faredata))
{
  if(faredata$VACATION[i]=='No')
  {
    faredata$VACATION[i] <- 0
  }
  else
  {
    faredata$VACATION[i] <- 1
  }
}

for(i in 1:nrow(faredata))
{
  if(faredata$SW[i]=='No')
  {
    faredata$SW[i] <- 0
  }
  else
  {
    faredata$SW[i] <- 1
  }
}

for(i in 1:nrow(faredata))
{
  if(faredata$SLOT[i]=='Free')
  {
    faredata$SLOT[i] <- 0
  }
  else
  {
    faredata$SLOT[i] <- 1
  }
  if(faredata$GATE[i]=='Free')
  {
    faredata$GATE[i] <- 0
  }
  else
    faredata$GATE[i] <- 1
}

View(faredata)

faredata$S_INCOME <- sub("\\$", "", faredata$S_INCOME)
faredata$E_INCOME <- sub("\\$", "", faredata$E_INCOME)
faredata$S_INCOME <- sub("\\,", "", faredata$S_INCOME)
faredata$E_INCOME <- sub("\\,", "", faredata$E_INCOME)

str(faredata)

faredata$VACATION <- as.numeric(faredata$VACATION)
faredata$SW <- as.numeric(faredata$SW)
faredata$S_INCOME <- as.numeric(faredata$S_INCOME)
faredata$E_INCOME <- as.numeric(faredata$E_INCOME)
faredata$SLOT <- as.numeric(faredata$SLOT)
faredata$GATE <- as.numeric(faredata$GATE)

str(faredata)
cor(faredata)
library(corrplot)
corrplot(corr = cor(faredata),method = "number")


qqplot(faredata$FARE,log10(faredata$S_POP))
qqplot(faredata$FARE,log10(faredata$E_POP))
qqplot(faredata$FARE,log10(faredata$PAX))

qqplot(faredata$FARE,faredata$DISTANCE)
qqplot(faredata$FARE,faredata$COUPON)
qqplot(faredata$FARE,faredata$NEW)
qqplot(faredata$FARE,faredata$VACATION)
qqplot(faredata$FARE,faredata$SW)
qqplot(faredata$FARE,faredata$HI)
qqplot(faredata$FARE,faredata$S_INCOME)
qqplot(faredata$FARE,faredata$E_INCOME)
qqplot(faredata$FARE,faredata$S_POP)
qqplot(faredata$FARE,faredata$E_POP)
qqplot(faredata$FARE,faredata$SLOT)
qqplot(faredata$FARE,faredata$GATE)
qqplot(faredata$FARE,faredata$PAX)

faredata$S_POP <- log10(faredata$S_POP)
faredata$E_POP <- log10(faredata$E_POP)
faredata$PAX <- log10(faredata$PAX)

indexes <- sample(1:nrow(faredata),size = 0.8*nrow(faredata),replace = FALSE)
train <- faredata[indexes,]
dim(train)
test <- faredata[-indexes,]
dim(test)

fit <- lm(FARE~.,data=train)
summary(fit)
vif(fit)
outlierTest(fit)
train <- train[-c(332)]

fit1 <- lm(FARE~VACATION+SW+HI+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,data=train)
summary(fit1)
outlierTest(fit1)
vif(fit1)
hist(fit1$residuals)
plot(fit1$residuals,train$S_INCOME)
plot(fit1$residuals,train$E_INCOME)
plot(fit1$residuals,train$S_POP)
plot(fit1$residuals,train$E_POP)
plot(fit1$residuals,train$DISTANCE)
plot(fit1$residuals,train$PAX)

predicted <- predict(fit1,test)
summary(predicted)
write.csv(predicted,"predict1.csv")
getwd()
write.csv(test,"test.csv")
SSE <- sum((test$FARE - predicted) ^ 2)
SST <- sum((test$FARE - mean(test$FARE)) ^ 2)
1 - SSE/SST


plot(fit1)
lmtest::bptest(fit1)
car::ncvTest(fit1)


boxfare <- caret::BoxCoxTrans(faredata$FARE)
print(boxfare)
faredata <- cbind(faredata,fare_new=predict(boxfare,faredata$FARE))
head(faredata)
View(faredata)
indexes <- sample(1:nrow(faredata),size = 0.8*nrow(faredata),replace = FALSE)
train <- faredata[indexes,]
dim(train)
test <- faredata[-indexes,]
dim(test)
fit2 <- lm(fare_new~VACATION+SW+HI+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,data=train)
summary(fit2)

#http://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/

plot(fit2)
lmtest::bptest(fit2)
car::ncvTest(fit2)
predicted <- predict(fit2,test)
summary(predicted)
write.csv(predicted,"predict1.csv")
getwd()
write.csv(test,"test1.csv")
