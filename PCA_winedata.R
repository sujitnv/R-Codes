winedata <- read.csv("C:/Users/Sujit pc/Desktop/R/WineData.csv",stringsAsFactors = FALSE,header = TRUE)
View(winedata)
a <- cor(winedata)
library(car)
library(corrplot)
install.packages("corrplot")
corrplot(a,method = "circle")
plot(cor(winedata))
dim(winedata)
winedata[!complete.cases(winedata),]
newdata<-na.omit(winedata)
dim(newdata)
winedata$Type<-as.factor(winedata$Type)
View(winedata)
winedata.pca <- prcomp(winedata,center = TRUE,scale. = TRUE)
z <- winedata.pca$rotation
dim(z)
clusterdata<-z[,c(1:2)]

View(clusterdata)
write.csv(z,"asd.csv")
summary(winedata.pca)
std <- winedata.pca$sdev
var <-std^2

totvar <- var/sum(var)
dim(totvar)
plot(totvar,type = "b")

getwd()
#winedata$Type="A,B,C" <- rbind(1,2,3)
View(winedata.pca)
winedata.pca <- as.data.frame(winedata.pca)
plot(winedata.pca,type="l")
a <- as.data.frame(winedata.pca)
typeof(winedata.pca)
cor(winedata.pca,winedata)
df <- data.frame(matrix(unlist(winedata.pca), nrow=180, ncol = 3))

winedata.pca1 <- princomp(winedata)
summary(winedata.pca1)
loadingvalues <- winedata.pca1$loadings[]
loadingvalues
x=loadingvalues[,1]
x


wss <- (nrow(winedata)-1)*sum(apply(winedata,2,var))
for(i in 2:20)wss[i] <- sum(kmeans(winedata,centers = i)$withinss)
plot(1:20,wss,type = "b")
winedataKcluster <- kmeans(winedata,5)
summary(winedataKcluster)
winedataKcluster
newwinedatak <- data.frame(winedata,winedataKcluster$cluster)
write.csv(newwinedatak,"winedataRcluster.csv")
getwd()
