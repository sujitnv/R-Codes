data("iris")
View(iris)
irisdata<-iris[,1:4]
irisdata
write.csv(iris,"irisdataorig.csv",row.names = FALSE)
getwd()
log.ir<-log(iris[,1:4])
plot(log.ir)
plot(iris)
ir.species<-iris[,5]
ir.species
ir.pca<-prcomp(log.ir,center = TRUE,scale. = TRUE)
ir.pca
plot(ir.pca)
summary(ir.pca)
predict(ir.pca,newdata = tail(log.ir,2))
install.packages("devtools")
library(devtools)
install_github("ggbiplot","vqv")
library(ggbiplot)
g<-ggbiplot(ir.pca,obs.scale = 1,var.scale = 1,groups = ir.species,ellipse = TRUE,circle = TRUE)
g
g<-g+scale_colour_discrete(name='')
g
g<-g+theme(legend.direction='horizontal',legend.position='top')
print(g)
biplot(ir.pca)
