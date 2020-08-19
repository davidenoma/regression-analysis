install.packages('MASS')
library(MASS)

fit =lm(Sepal.Length~., data=iris)

#To examine all models
step(lm(mpg~wt + drat + disp + qsec,data=mtcars),direction = "both")
step(lm(mpg~wt + drat + disp + qsec,data=mtcars),direction = "backward")

#INclude performance of the null model as well 

null = lm(Sepal.Length~1, data = iris) 
#Mean value of Y predicts new Ys
full = lm(Sepal.Length~., data = iris) #include all Xs

step(null, scope = list(lower=null,upper = full), direction = "both")

library('relaimpo')
fit =lm(formula = Sepal.Length~ Petal.Length + Sepal.Width +Petal.Width, data=iris)
#Bootstrap Measures of Relative importance 
#Drawing randomly with replacement from a set of data points
boot <- boot.relimp(fit, b = 1000, type = c("lmg","last","first","pratt"),
                    rank=TRUE,diff = TRUE , rela= TRUE)
booteval.relimp(boot)
plot(booteval.relimp(boot, sort=TRUE))

data(BostonHousing)
fit1 = lm(medv~.,data = BostonHousing)
summary(fit1)

fit2 = lm(medv~lstat, data=BostonHousing)
summary(fit2)

library(leaps)
regFit = regsubsets(medv~.,data=BostonHousing)
summary(regFit)

reg.summary = summary(regFit)

reg.summary$rsq

plot(regFit, scale="adjr2", main="Adjusted R ^ 2")

#Evaluating the accuracy of a regression model-A machine learning approach
data(BostonHousing)
str(BostonHousing)

summary(BostonHousing)

library('caret')
set.seed(99)
Train = createDataPartition(BostonHousing$medv,p=0.75,list = FALSE)
#75 25 SPLIT
training = BostonHousing[Train,]
testing = BostonHousing[-Train,]
#the ~. is to include all the predictor variables 
fit1 = train(medv~., data=training, method="lm")

summary(fit1)
#See the performance on the training data 

fit2 = train(medv~.,data=training,method = "lm", metric="RMSE")
print(fit2)

p1 = predict(fit2,newdata = testing)#predict on the basis of 25 % test data
p = as.data.frame(p1)
test = as.data.frame(testing$medv)
y = cbind(test,p)
colnames(y)=c("medv","Pred")

head(y)

cor.test(y$medv,y$Pred)


library(Metrics)
rmse(y$medv, y$Pred)
#Rmse also checks for overfitting 

#KFOLD CROSS VALIDATING
#vERY Robust as every data gets to be training and test 

train_control <- trainControl(method = "cv",number = 10)
fit3= train(medv~., data=BostonHousing,trControl=train_control, method="lm",metric="RMSE")

summary(fit3)
print(fit3)


#3 10 fold cross validation and data split 
set.seed(999)

Train = createDataPartition(BostonHousing$medv,p=0.75,list=FALSE)

training = BostonHousing[Train,]
testing = BostonHousing[-Train,]

train_control <- trainControl(method="cv",number = 10)

fit4 = train(medv~., data = training, trControl = train_control,method="lm")
print(fit4)

#Predict on unseen data 
p1 = predict(fit4, newdata=testing)
p = as.data.frame(testing$medv)

y= cbind(test,p)
colnames(y) = c("medv","Pred")
head(y)
cor.test(y$medv,y$Pred)


rmse(y$medv,y$Pred)



#LASSO REGRESSION FOR VARIABLE SELECTION 
data(BostonHousing)
library(caret)
train_control <- trainControl(method = "cv",number=10)

lasso <- train(medv~., BostonHousing,
               method="lasso",
               preProc = c('scale','center'),
               trControl = train_control)
lasso

#Get Coefficient 
predict.enet(lasso$finalModel,type='coefficients', s=lasso$bestTune$fraction)


