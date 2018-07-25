require(MASS)
data("iris")
head(iris,3)
r <- lda(formula=Species~.,data=iris,prior=c(1,1,1)/3)
r
r$prior
r$counts
r$means
r$scaling
r$svd
prop=r$svd^2/sum(r$svd^2)
prop

train <- sample(1:150,75)
View(train)
train
r3 <- lda(Species~.,iris,prior=c(1,1,1)/3,subset=train)
r3
plda <- predict(object = r,newdata = iris[-train,])
plda

#http://www.r-bloggers.com/computing-and-visualizing-lda-in-r/
