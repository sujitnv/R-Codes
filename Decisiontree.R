data(iris)
names(iris)

table(iris$Species)

install.packages("ggplot2")
library(ggplot2)

qplot(Sepal.Length, Petal.Length, data=iris, colour=Species, size=I(4))
qplot(Petal.Width, Sepal.Width, data=iris, colour=Species, size=I(4))

install.packages("tree")
library(tree)

tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
summary(tree1)

plot(tree1)
text(tree1)
cv.tree(tree1)
tree11 <- prune.misclass(tree1,best=3)
summary(tree11)
cv.tree(tree11)
tree2 <- tree(Species~Petal.Length+Sepal.Length,data=iris)
plot(tree2)
text(tree2)

plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree11,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)


tree1 <- tree(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris)
summary(tree1)

plot(tree1)
text(tree1)

