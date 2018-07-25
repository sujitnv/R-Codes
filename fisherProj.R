data <- read.table("C:/Users/Sujit pc/Desktop/R/iris.txt",stringsAsFactors = FALSE)
View(data)
plot(iris$Sepal.Length)
plot(iris$Sepal.Width)
plot(iris$Petal.Length)
plot(iris$Petal.Width)
plot(iris$Species)
library(ggplot2)

qplot(Petal.Width, Sepal.Width, data=iris, colour=Species, size=I(4))
qplot(Sepal.Length, Petal.Length, data=iris, colour=Species, size=I(4))
datairis <- iris
datairis$Species[datairis$Species=="virginica"] <- 'versicolor'
View(datairis)
traindata <- sample(1:nrow(datairis),size = 0.8*nrow(datairis),replace = FALSE)
train <- datairis[traindata,]
View(train)
test <- datairis[-traindata,]
View(test)

library(MASS)
fischer1 <- lda(Species~.,data=train)
fischer1

datairis11 <- iris[c(iris$Species!='setosa'),]
View(datairis11)
traindata11 <- sample(1:nrow(datairis11),size = 0.8*nrow(datairis11),replace = FALSE)
train11 <- datairis11[traindata11,]
fischer2 <- lda(Species~.,data=train11)
fischer2
iris.values <- predict(fischer2, test[,-5])
iris.values
table(pred=iris.values$class,true=test$Species)
plot(iris.values$class,test$Species) 
qplot(iris.values$class, data=test, colour=Species, size=I(4))
