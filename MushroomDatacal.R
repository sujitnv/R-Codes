mushroomdata <- read.table("C:/Users/Sujit pc/Desktop/R/mushroom.txt",sep = ',')
View(mushroomdata)
mushroomdata1 <- mushroomdata[c(1:100),]
View(mushroomdata1)
library(ggplot2)
plot(mushroomdata$V2)
install.packages("ineq")
library(ineq)
c <- ineq(mushroomdata$V2,type="Gini")
d <- ineq(mushroomdata$V3,type="Gini")
e <- ineq(mushroomdata$V4,type="Gini")
f <- ineq(mushroomdata$V5,type="Gini")
g <- ineq(mushroomdata$V6,type="Gini")
h <- ineq(mushroomdata$V7,type="Gini")
a <- c(c,d,e,f,g,h)
a
plot(a)
install.packages("plyr")
library(plyr)
library(rpart)
library(car)

entropy <- function(target){
  freq <- data.frame(prop.table(table(target)))
  freq$entropy <- -log2(freq$Freq)*freq$Freq
  sum(freq$entropy)
}
aa <- entropy(mushroomdata$V2)
bb <- entropy(mushroomdata$V3)
cc <- entropy(mushroomdata$V4)
dd <- entropy(mushroomdata$V5)
ee <- entropy(mushroomdata$V6)
ff <- entropy(mushroomdata$V7)
entrpyyyy <- c(aa,bb,cc,dd,ee,ff)
entrpyyyy
v <- c('V2','V3','V4','V5','V6','V7')
table(v,a,entrpyyyy)
library(tree)
treeV2 <- tree(mushroomdata$V1~mushroomdata$V2)
plot(treeV2$y)
treeV3 <- tree(mushroomdata$V1~mushroomdata$V3)
treeV4 <- tree(mushroomdata$V1~mushroomdata$V4)
treeV5 <- tree(mushroomdata$V1~mushroomdata$V5)
plot(treeV5)
summary(treeV5)
a <- entropy(mushroomdata$V5)
1-a

IG_cat<-function(data,feature,target){
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),] 
  #use ddply to compute e and p for each value of the feature
  dd_data<-ddply(data, feature, here(summarise), e=entropy(get(target)), N=length(get(target)))
  #compute entropy for the parent
  e0<-entropy(data[,target])
  #calculate p for each value of feature
  dd_data$p<-dd_data$N/nrow(data)
  #compute IG
  IG<-e0-sum(dd_data$p*dd_data$e)
  
  return(IG)
}

IG_cat(mushroomdata,mushroomdata$V2,mushroomdata$V2)
tree
printcp(tree)


