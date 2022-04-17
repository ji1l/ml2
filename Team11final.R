#Team 11

#### Data Preparation

#loading libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(gbm)

#visualizing the data 
#df <- train 
#barplot(sort(table(df$Type)), main = "City Type")
#barplot(sort(table(df$City)), main = "Restaurant Numbers", cex.names = 0.5)

#setup data 
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

n.train <- nrow(train)
test$revenue <- 0
train$revenue <- log(train$revenue) #if you visualize the revenue variable - it is highly skewed - take the log to normalize
data <- rbind(train, test)
data <-data[,-c(1,3)]

#convert date to num days
today<-Sys.Date()
opened<-as.Date(data$Open.Date,'%m/%d/%Y')
format(opened, "%Y-%m-%d")
data$Open.Date<-difftime(today, opened)
data$Open.Date <- as.numeric(substr(data$Open.Date,1,4))


#convert to factors
data$City.Group<- as.factor(data$City.Group)
data$Type <- as.factor(data$Type)

#defining test and train 
train <- data[1:n.train,]
test <- data[-c(1:n.train),]

#creating folds
set.seed(123)
folds <- createFolds(1:nrow(train),k=10)


#####   randomForest  ######

#set seed 
set.seed(123)

#define sizes of trees we want to test 
ntree <- c(3000,6000,9000) # parameter

#create empty matrix to store rmse values that will be created in nested for loop; defining column names
rmse_rf <- matrix(0,10,3)
colnames(rmse_rf) <- c('ntree = 3000','ntree = 6000','ntree = 9000')

#nested for loop to test for which number of trees to use
for(j in 1:3){
  for(i in 1:10){
    fold_test <- train[folds[[i]],]
    fold_train <- train[-folds[[i]],]
    rf_model <- randomForest(data=fold_train,revenue~.,ntree=ntree[j])
    pred_rf <- predict(rf_model,newdata=fold_test)
    rmse_rf[i,j] <- sqrt(sum((pred_rf - log(fold_test[,41]))^2))
  }
}

#print the rmses
map_rf <- apply(rmse_rf,2,mean)
map_rf 

#the model we will choose is the one with the smalled rmse value
which.min(map_rf) # ntree  = 6000 opt

rf_model_best <- randomForest(data=train,revenue~.,ntree = 6000,importance = T)
importance(rf_model_best)

# performance
pred_rf_best <- predict(rf_model_best,newdata=train)
summary(lm(pred_rf_best~train$revenue))$r.squared # r.squared - 0.908501
plot(pred_rf_best~train$revenue, pch = 16)

# predict  final result 
final_pred_rf <- predict(rf_model_best,newdata=test)

#looking at predicted revenues
exp(final_pred_rf)


#### boosting ####

#reset seed
set.seed(123)

rmse_boost <- matrix(0,10,3)
colnames(rmse_boost) <- c('ntree = 3000','ntree = 6000','ntree = 9000')

for (j in 1:3) {
  for (i in 1:10) {
    
    fold_test <- train[folds[[i]],]
    fold_train <- train[-folds[[i]],]
    
    boost.model<-gbm(revenue~.,
                      data = fold_train,
                      distribution= "gaussian",
                      n.trees = ntree[j],
                      #using default interaction and shrinkage values
                      verbose = F)
    
    yhat<-predict(boost.model,
                  newdata= fold_test,
                  n.trees= ntree[j])
    
    rmse_boost[i,j]<-sqrt(mean((yhat-log(fold_test[,41])^2)))
  }
}

#print rmses
map_boost <- apply(rmse_boost,2,mean)
map_boost
which.min(map_boost) #ntree = 6000

#creating best model 
boost_model_best <- gbm(revenue~.,
                      data = train,
                      distribution= "gaussian",
                      n.trees = 6000,
                      verbose = F)

#performance
pred_boost_best <- predict(boost_model_best, newdata=train)
summary(lm(pred_boost_best~train$revenue))$r.squared # r.squared - 0.9131
plot(pred_boost_best, train$revenue, pch = 16)

# predict  final result 
final_pred_boosting <- predict(boost_model_best,newdata=test)
exp(final_pred_boosting) #looking at results - must take exp bc earlier we took the log of revenue




#####   svm model #####

#similar to above - setting up parameter to look at in nested for loop; empty matrix to store rmse values
gamma <- c(0.1,0.5,1,10) # parameter 
rmse_svm <- matrix(0,10,4)
colnames(rmse_svm) <- c('gamma=0.1','gamma=0.5','gamma=1','gamma=10')

#  default kernel  = 'radial'
for(j in 1:4){
  for(i in 1:10){
    fold_test <- train[folds[[i]],]
    fold_train <- train[-folds[[i]],]
    svm_model <- svm(data=fold_train,revenue~.,kernel='radial',gamma = gamma[j])
    pred_svm <- predict(svm_model,newdata=fold_test)
    rmse_svm[i,j] <- sqrt(sum((pred_svm - log(fold_test[,41]))^2))
  }
}

#checking out rmses
map_svm <- apply(rmse_svm,2,mean)
map_svm

#finding which gamma value to use to create best model 
which.min(map_svm) #gamma = 1 optimal


#best model
svm_model_best <- svm(data=train,revenue~.,kernel='radial', gamma = 0.1)
pred_svm_best <- predict(svm_model_best,newdata=train)
summary(lm(pred_svm_best~train$revenue))$r.squared # r.squared - 0.7424
plot(pred_svm_best,train$revenue,pch= 16)

# predict  final result 
final_pred_svm <- predict(svm_model_best,newdata=test)
exp(final_pred_svm)


#### submission ####
submit<-as.data.frame(cbind(seq(0, length(final_pred_rf) - 1, by=1), exp(final_pred_rf), exp(final_pred_boosting), exp(final_pred_svm)))
colnames(submit)<-c("Id","RandomForest_Prediction", "Boosting Prediction","SVM_Prediction")
write.csv(submit,"tp02_team11.csv",row.names=FALSE,quote=FALSE)
