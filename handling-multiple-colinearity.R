library(mlbench
        )
data("BostonHousing")
str(BostonHousing)

#Predict the variation in medv house price
#the medv is y, target variable is a continuous numerical entity
#Regression Problem 

require(pls)
set.seed(1000)

#PCR Regression

pcr_model <- pcr(medv~.,data=BostonHousing,scale=TRUE,validation="CV" )
summary(pcr_model)
validationplot(pcr_model)
validationplot(pcr_model,val.type = "R2")
#Predicted values of Y vs Actual Y
predplot(pcr_model)


#Partial Least Square Regresssion 
library(caret)

myfolds <- createMultiFolds(BostonHousing$medv, k=5, times = 10)
control <- trainControl('repeatedcv',index=myfolds, selectionFunction = "oneSE" )


#Train the PLS model 
mod1 <- train(medv ~ . , data = BostonHousing,
              method="pls",
              metric = "R2",
              tuneLength = 20,
              trControl = control, 
              preProc= c("zv", "center", "scale"))
plot(mod1)

##Try another dataset 
library(plsdepot)
data(vehicles)
head(vehicles)
names(vehicles)

cars = vehicles [,c(1:12,14:16,13)]
pls1 = plsreg1(cars[,1:15],cars[,16, drop=FALSE], comps = 5)
pls1$R2


#Using predictors as they are 

#Ridge Regression 

data("BostonHousing")

library('tidyverse')
library('broom')
library('glmnet')
#Ridge regression uses lambda values to minimize the error between 
#The predicted and the actual. Lambda = 0 represents OLs and very high 
#values produce the mean of Y and so we define the optimum lambda. 



lambdas = 10 ^ seq(3, -2, by = -.1)
lambdas
y = BostonHousing$medv
x = BostonHousing %>%select(crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,b,lstat)%>% data.matrix()
#glmnet takes in a vector input and matrix of predictors

fit <- glmnet(x,y,alpha = 0, lambda = lambdas)
#Ridge regression involves tuning a hyperparameter, lambda,
#glmnet() runs the model many rimes for different values of lambda.

#What is the optimal lambda value 
cv_fit = cv.glmnet(x,y,alpha=0, lambda = lambdas)
#cv.glmnet uses cross validation to work out how well
#each model generalizes 

plot(cv_fit)
optlambda <- cv_fit$lambda.min
optlambda


#Log value of lambda best minimizes the error 

#predicting values and computing an R2 values for the data we trained on 
y_predicted <- predict(fit, s = optlambda, newx= x )


#Sum of squares total and error 
sst <- sum(y^2)
sse <- sum((y_predicted -y)^2)

rsq <- 1 - sse / sst
rsq


#LASSO REGRESSION 

data("BostonHousing")
#Lasso finds the optimum lamda values 
y = BostonHousing$medv
x = BostonHousing %>%select(crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,b,lstat)%>% data.matrix()
# Lasso regression uses lambda values to minimize error bw predicted 
# & actual Y. Lambda=0 represents OLS and very high values produce the mean of Y
# need to identify the optimum lambda

library(tidyverse)
library(broom)
library(glmnet)

y = BostonHousing$medv
x = BostonHousing %>% select(crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,b,lstat) %>% data.matrix()
#  glmnet requires a vector input and matrix of predictors

lambdas = 10^seq(3, -2, by = -.1)#specify a range of lambda

fit <- glmnet(x, y, alpha = 1, lambda = lambdas) #alpha=1 in lasso
#  regression involves tuning a hyperparameter, lambda, 
#glmnet() runs the model many times for different values of lambda.

# which is the most optimal lambda value?

cv_fit <- cv.glmnet(x, y, alpha = 1, lambda = lambdas)
#cv.glmnet() uses cross-validation to work out 
#how well each model generalises

plot(cv_fit)
#lowest point in the curve indicates the optimal lambda

optlambda <- cv_fit$lambda.min
optlambda
#log value of lambda that best minimised the error

#predicting values and computing an R2 value for the data we trained on

y_predicted <- predict(fit, s = optlambda, newx = x)

# Sum of Squares Total and Error
sst <- sum(y^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

#The optimal model has accounted for 96% of the variance in the training data


