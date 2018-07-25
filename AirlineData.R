airlinedata<-read.csv("C:/Users/Sujit pc/Desktop/R/AirlinesData.csv",stringsAsFactors = FALSE,header = TRUE)
dim(airlinedata)
#plot(airlinedata$Balance,airlinedata$Qual_miles)
newairlinedata <- na.omit(airlinedata)
dim(newairlinedata)
b <- scale(newairlinedata)
c <- dist(b,method = "euclidean")
d <- hclust(c,method = "ward.D2")

e <- cutree(d,k=4)
write.csv(e,"eeee2.csv")
getwd()
plot(e)
f <- rect.hclust(d, k=4, border="red")


View(e)

wss<-(nrow(newairlinedata)-1)*sum(apply(newairlinedata,2,var))
for(i in 2:20)wss[i]<-sum(kmeans(newairlinedata,centers = i)$withinss)
plot(1:20,wss,type = "b",xlab = "Number of Clusters",ylab = "within groups sum of squares")

fit<-kmeans(newairlinedata,4)
fit
newairlinedata1<-data.frame(newairlinedata,fit$cluster)
View(newairlinedata1)
write.csv(newairlinedata1,"kmeanscluster.csv")
getwd()


#http://www.statmethods.net/advstats/cluster.html