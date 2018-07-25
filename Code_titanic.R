training.data.raw <-read.csv("C:/Users/Sujit pc/Desktop/R/LogisticRegression/Titanic_Train.csv",header = TRUE,na.strings = c(""))
sapply(training.data.raw, function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
library(Amelia)
missmap(training.data.raw,main="Missing Values Vs Observed")
data<- subset(training.data.raw,select = c(2,3,5,6,7,8,10,12))
data$Age[is.na(data$Age)]<-mean(data$Age,na.rm = T)
View(data)
is.factor(data$Embarked)
contrasts(data$Embarked)
data<-data[!is.na(data$Embarked),]
rownames(data)<-NULL
View(data)
traindata<-data[1:800,]
testdata<-data[801:889,]
modellog<-glm(Survived~.,family = binomial(link='logit'),data = traindata)
summary(modellog)
anova(modellog,test = "Chisq")
install.packages("pscl")
library(pscl)
pR2(modellog)
fitted.results<-predict(modellog,newdata = subset(testdata,select=c(2,3,4,5,6,7,8)),type = 'response')
write.csv(fitted.results,"forecasted.csv",row.names = FALSE)
getwd()
fitted.results<-ifelse(fitted.results>0.5,1,0)
misClassificError<-mean(fitted.results !=testdata$Survived)
1-misClassificError
install.packages("ROCR")
library(ROCR)
p <- predict(modellog, newdata=subset(testdata,select=c(2,3,4,5,6,7,8)), type="response")
pr<-prediction(p,testdata$Survived)
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
auc<-performance(pr,measure = "auc")
auc<-auc@y.values[[1]]
auc




modellog1<-glm(Survived~Pclass+Sex+Age+Embarked,family = binomial(link='logit'),data = traindata)
summary(modellog1)
anova(modellog1,test = "Chisq")
install.packages("pscl")
library(pscl)
pR2(modellog1)
fitted.results1<-predict(modellog1,newdata = subset(testdata,select=c(2,3,4,5,6,7,8)),type = 'response')
fitted.results1<-ifelse(fitted.results1>0.5,1,0)
write.csv(fitted.results1,"forecasted1.csv",row.names = FALSE)
getwd()
misClassificError1<-mean(fitted.results1 !=testdata$Survived)
1-misClassificError1
install.packages("ROCR")
library(ROCR)
p1 <- predict(modellog1, newdata=subset(testdata,select=c(2,3,4,5,6,7,8)), type="response")
pr1<-prediction(p1,testdata$Survived)
prf1<-performance(pr1,measure = "tpr",x.measure = "fpr")
plot(prf1)
auc1<-performance(pr1,measure = "auc")
auc1<-auc1@y.values[[1]]
auc1
