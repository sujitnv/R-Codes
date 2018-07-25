library(datasets)
View(attitude)
str(attitude)
attitudeData<-attitude
View(attitudeData)
write.csv(attitudeData,'Attitude.csv',row.names = FALSE)
getwd()
summary(attitude)
data<-attitude[,c(3,4)]
View(data)
plot(data)
km1<-kmeans(data,2,nstart=100)
km1
plot(data,col=(km1$cluster +1))
km2<-kmeans(data,3,nstart=100)
km1
plot(data,col=(km2$cluster +1))
mydata<-data
wss<-(nrow(mydata)-1)*sum(apply(mydata,2,var))
for(i in 2:15) wss[i]<-sum(kmeans(mydata,centers = i)$withinss)
plot(1:15,wss)

km3<-kmeans(data,6,nstart=100)
km3
plot(data,col=(km3$cluster +1))

hc <- hclust(dist(data))
hc
plot(hc)
d <- dist(as.matrix(data))
d

clusterData <- read.csv("C:/Users/Sujit pc/Desktop/IFT-433 Big Data/clusterdata.csv",stringsAsFactors = FALSE,header = TRUE)
View(clusterData)
d <- dist(as.matrix(clusterData))
d
hc <- hclust(dist(clusterData))
plot(hc)
