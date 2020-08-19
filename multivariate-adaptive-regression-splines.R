##Multivariate ADative regession splines 

#Non parametric regression technique
setwd("C:/Users/kora/Documents/statistics/Regression Analysis for Statistics and Machine Learning in R [FCO]/R-Scripts-Regression-Analysis")
eco = read.csv('biocap.csv')
head(eco)
require('caret')
set.seed(123)
Train = createDataPartition(eco$BiocapacityT, p = 0.75, list = FALSE)
training = eco[Train,]
testing = eco[-Train,]
library(earth)
modfit = earth(BiocapacityT~.,data = training)
modfit
#Generalized cross validation is a form of regularization as well
evimp(modfit)
p = predict(modfit, newdata = testing)
head(p)
p = as.data.frame(p)
final = cbind(testing$BiocapacityT , p)
head(final)
cor(testing$BiocapacityT,p)
fitControl = trainControl(method = "cv", number = 10)

modfit2 = train(BiocapacityT ~. , data=training, method="earth",
                trControl=fitControl)
summary(modfit2)

p = predict(modfit2, newdata = testing)
head(p)
p = as.data.frame(p)
final = cbind(testing$BiocapacityT, p)
head(final)
cor(testing$BiocapacityT,p)
varImp(modfit2)
