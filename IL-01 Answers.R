#IL01 Q01
library(ISLR2)
#sum(is.na(Hitters$Salary)) #Q01-1
#dim(Hitters) #Q1-2
NewHitters <- na.omit(Hitters) 
#dim(NewHitters) #Q1-3
#print(sum(is.na(Hitters$Salary))) #Q1-4

#IL01 Q02
library(leaps)
set.seed(5082)
regfit.full <-regsubsets(Salary ~., NewHitters)
#print(summary(regfit.full)) #Q2

#IL01 Q03
regfit.full <-regsubsets(Salary ~., nvmax=19,NewHitters)
regfit.summary<-summary(regfit.full)
#print(regfit.summary$rsq) #Q3

#IL01 Q04
#which.max(regfit.summary$adjr2) #Q4

#IL01 Q05
#print(coef(regfit.full, 11)) #Q5
regfit.fwd<-regsubsets(Salary~., data=NewHitters, nvmax=19, method="forward")

#IL01 Q06
#print(summary(regfit.fwd)) #Q06-1
regfit.bwd<-regsubsets(Salary~., data=NewHitters, nvmax=19, method="backward")
#print(summary(regfit.bwd)) #Q06-2
#print(coef(regfit.full, 7)) #Q06-3
#print(coef(regfit.fwd, 7)) #Q06-4
#print(coef(regfit.bwd, 7)) #Q06-5

#IL01 Q07
set.seed(1)
train<-sample(c(TRUE, FALSE), nrow(NewHitters), replace=TRUE)
test <-(!train)
#print(sum(train)) #Q07-1
#print (nrow(NewHitters)) #Q07-2
regfit.best<-regsubsets(Salary ~., data=NewHitters[train, ], nvmax=19)
#print(summary(regfit.best)$adjr2) #Q07-3
#which.max(summary(regfit.best)$adjr2) #Q07-4

#IL01 Q08
predict.regsubsets = function(object,newdata,id,...){
  form <- as.formula(object$call[[2]]) # Extract the formula used when we called regsubsets()
  mat <- model.matrix(form,newdata)    # Build the model matrix from newdata using the extract formula 
  coefi <- coef(object,id=id)          # Extract the coefficients of the ith model
  xvars <- names(coefi)                # Pull out predictor names used in the ith model
  mat[,xvars]%*%coefi                 # Calculate predicted values using matrix multiplication
}
yhat=predict(regfit.best, NewHitters[test,],7)
#print(yhat) #Q08-1
mse<-mean((NewHitters$Salary[test]-yhat)^2)
#print(mse) #Q08-2

#IL01 Q09
mses<-rep(NA, 19)
for (i in 1:19){
  yhat<-predict(regfit.best, NewHitters[test,],i)
  mses[i]<-mean((NewHitters$Salary[test]-yhat)^2)
}
#print(mses) #Q09-1
#print(which.min(mses)) #Q09-2
#coef(regfit.best,7) #Q09-3

#IL01 Q10
k<-10
n<-nrow(NewHitters)
set.seed(1)
folds<-sample(rep(1:k, length=n))
cv.errors<-matrix(data=NA, nrow=k, ncol=19, dimnames=list(NULL, paste(1:19)))
for (j in 1:k) {
  #train the best subset model using training data (not in jth fold)
  best.fit <- regsubsets(Salary ~ ., data=NewHitters[folds != j, ], nvmax=19) 
  for (i in 1:19) {
    #calculate predicted values using test data in the jth fold
    pred <- predict(best.fit, NewHitters[folds==j, ],id=i)
    #store MSE result for each fold in the cv.errors matrix
    cv.errors[j,i]<- mean((NewHitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors<-apply(X=cv.errors,MARGIN=2, mean) #MARGIN=2 indicates column
#mean.cv.errors #Q10-1
#which.min(mean.cv.errors) #Q10-2
#coef(regfit.full, 10) #Q10-3

#IL01 Q11
library(glmnet)
x<-model.matrix(Salary~., NewHitters)[,-1]
#print (x) #Q11-1
y<-NewHitters$Salary
#print (y) #Q11-2

#IL01 Q12
power.value<-seq(from=10, to=-2, length=100) 
#print(power.value) #Q12-1
grid<-10^power.value
#print(grid) #Q12-2
ridge.mod<-glmnet(x, y, alpha=0, lambda = grid)
#dim(coef(ridge.mod)) #Q12-3

#IL01 Q13
#ridge.mod$lambda[50] #Q13-1
#coef(ridge.mod)[,50] #Q13-2
#sqrt(sum(coef(ridge.mod)[-1,50]^2)) #Q13-3
#ridge.mod$lambda[60] #Q13-4
#coef(ridge.mod)[,60] #Q13-5
#sqrt(sum(coef(ridge.mod)[-1,60]^2)) #Q13-6

#IL01 Q14
set.seed(1)
train <- sample(1:nrow(x), size=nrow(x)/2)
test<-(-train)
ridge.mod<-glmnet(x[train, ], y[train], alpha =0, lambda = grid, thresh=1e-12)
ridge.pred<-predict(ridge.mod, s=4, newx=x[test,])
mse.4<-mean((ridge.pred-y[test])^2) 
#print (mse.4) #Q14-1
mse.null<-mean((mean(y[train])-y[test])^2) 
#print(mse.null) #Q14-2
#1- (mse.4/mse.null) #Q14-3
lm.pred<-predict(ridge.mod, s=0, newx=x[test,], exact=T, x=x[train, ], y=y[train])
#mean((lm.pred-y[test])^2) #Q14-4
#predict(ridge.mod, s=4, type="coefficients") #Q14-5

#IL01 Q15
set.seed(1)
cv.out<-cv.glmnet(x[train, ], y[train], alpha=0)
bestlam<-cv.out$lambda.min
#print(bestlam) #Q15-1
ridge.pred<-predict(ridge.mod, s=bestlam, newx=x[test,])
#mean((ridge.pred-y[test])^2) #Q15-2
out <-glmnet(x,y,alpha=0)
#predict(out, type="coefficients", s=bestlam) #Q15-3

#IL01 Q16
set.seed(1)
lasso.mod<-glmnet(x[train, ], y[train], alpha=1, lambda=grid)
#summary(lasso.mod) #Q16-1
cv.out<-cv.glmnet(x[train, ], y[train], alpha=1)
bestlam <- cv.out$lambda.min
#print(bestlam) #Q16-2
lasso.pred<-predict(lasso.mod, s=bestlam,newx=x[test,])
#mean((lasso.pred-y[test])^2) #Q16-3
out<-glmnet(x,y, alpha=1, lambda=grid)
lasso.coef<-predict(out, type="coefficients", s=bestlam)
#print(lasso.coef) #Q16-4

#IL01 Q17
library(pls)
set.seed(1)
pcr.fit<-pcr(Salary~., data=NewHitters, scale=TRUE, validation="CV")
#summary(pcr.fit) #Q17-1\
set.seed(1)
pcr.fit<-pcr(Salary~., data=NewHitters, subset = train, scale=TRUE, validation="CV")
pcr.pred<-predict(pcr.fit, x[test, ], ncomp=5)
#mean((pcr.pred-y[test])^2) #Q17-2
pcr.fit<-pcr(y~x, scale=TRUE, ncomp=5)
#summary(pcr.fit) #Q17-3

#IL01 Q18
set.seed(1)
pls.fit<-plsr(Salary~., data=NewHitters, subset = train, scale=TRUE, validation="CV")
summary(pls.fit) #Q18-1
pls.pred<-predict(pls.fit, x[test, ], ncomp=1)
mean((pls.pred-y[test])^2) #Q18-2
pls.fit<-plsr(y~x, scale=TRUE, ncomp=1)
summary(pls.fit) #Q18-3