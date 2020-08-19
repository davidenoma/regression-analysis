#Outliers affects our results 
#So we need to deal with outliers in order to have robust estimates 
#to deal with outliers 

##Robust REgression [least trimmed squares regression technique]

library(robustbase)
library(robustbase)

data("BostonHousing")
str(BostonHousing)

#Fit OLS regresssion model 
reg1 <- lm(medv~., data = BostonHousing)
summary(reg1)
#model explains 73 percent of the response variable 

plot(reg1)

#test the presence of outliers 
library(car)
outlierTest(reg1) #Identify outlier data points 

ltsFit = ltsReg(medv~.,data = BostonHousing)
  summary(ltsFit)
#This explains the variation better

  
  #Heterodascicity- non constant residual variances 
reg1 <- lm(medv~., data=BostonHousing)
summary(reg1)

#TEsting for non constant residual variables 
require(car)
ncvTest(reg1)

#Try Box Cos Transformation 
library(caret)
library(e1071)
distm = BoxCoxTrans(BostonHousing$medv)
print(distm)

BostonHousing = cbind(BostonHousing, m_new = predict(distm,BostonHousing$medv)) 

head(BostonHousing)

reg_bc = lm(m_new~., data = BostonHousing)
ncvTest(reg_bc)

#The Box cox transform did not work and so we use another approach
library(lmvar)
library(datasets)  
