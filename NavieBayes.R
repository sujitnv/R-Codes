install.packages("mlbench")
library(mlbench)
data("HouseVotes84")
View(HouseVotes84)
plot(as.factor(HouseVotes84[,2]))
plot(as.factor(HouseVotes84[HouseVotes84$Class=='republican',2]))
plot(as.factor(HouseVotes84[HouseVotes84$Class=='democrat',2]))
na_by_col_class <- function(col,cls){return(sum(is.na(HouseVotes84[,col])&HouseVotes84$Class==cls))}
na_by_col_class(3,'democrat')
na_by_col_class(2,'republican')

p_y_col_class <- function(col,cls){
  sum_y <- sum(HouseVotes84[,col]=='y' & HouseVotes84$Class==cls,na.rm =TRUE)
  sum_n <- sum(HouseVotes84[,col]=='n' & HouseVotes84$Class==cls,na.rm = TRUE)
  return(sum_y/(sum_y+sum_n))
}
p_y_col_class(2,'democrat')
p_y_col_class(2,'republican')

for(i in 2:ncol(HouseVotes84))
{
  if(sum(is.na(HouseVotes84[,i])>0)){
    c1 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=='democrat',arr.ind = TRUE)
    c2 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=='republican',arr.ind = TRUE)
    HouseVotes84[c1,i] <- ifelse(runif(na_by_col_class(i,'democrat'))< p_y_col_class(i,'democrat'),'y','n')
    HouseVotes84[c2,i] <- ifelse(runif(na_by_col_class(i,'republican'))<p_y_col_class(i,'republican'),'y','n')
  }
}

HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)
traincolNum <- grep("train",names(HouseVotes84))
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-traincolNum]
tesHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-traincolNum]
install.packages("e1071")
library(e1071)
nb_model <- naiveBayes(Class~.,data=trainHouseVotes84,laplace=0)
nb_model
nb_test_predict <- predict(nb_model,tesHouseVotes84[,-1])
table(pred=nb_test_predict,true=tesHouseVotes84$Class)
mean(nb_test_predict==tesHouseVotes84$Class)

nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for (i in 1:n){
    HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<train_fraction,1,0)
    trainColNum <- grep("train",names(HouseVotes84))
    trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
    testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]
    nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
    nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])
    fraction_correct[i] <- mean(nb_test_predict==testHouseVotes84$Class)
  }
  return(fraction_correct)
}
frac_coreect_pred <- nb_multiple_runs(0.6,20)
frac_coreect_pred

library(caret)
model <- train(Class~.,data=trainHouseVotes84,lambda=1.0,method="nb")
print(model)
predictmodel <- predict(model,tesHouseVotes84[,-1])
predictmodel
table(pred=predictmodel,true=tesHouseVotes84$Class)
mean(predictmodel==tesHouseVotes84$Class)
#"https://eight2late.wordpress.com/2015/11/06/a-gentle-int roduction-to-naive-bayes-classification-using-r/