#Linear regression, we model Y as a function of X 
 library(help = "datasets")
 data("Orange")
head(Orange) 
plot(Orange$age, Orange$circumference)
#Model the variation in circumference as a
#a function of age

#H0: there is no link between age and circumference

fit = lm(circumference ~ age, data=Orange)

summary(fit)
##p < 0.05 so we reject h0 

library(ggplot2)
ggplot(Orange, aes(x=age, y=circumference)) +
  geom_point(color="#2980B9",size = 4) +
  geom_smooth(method = lm, color = "#2C3E50")
fit = lm(circumference~ age, data = Orange)
summary(fit)

#Predict the circumference of a tree aged 1500
#From the statistics we cannot take the intercept because the value
#of X cannot be zero 
age = 1500
cirm = 0.106 * age
cirm

#Calculating Confidence intervals in R 
head(ToothGrowth)

##Confidence interval of mean 
n = length(ToothGrowth$len)
s = sd(ToothGrowth$len)
standardError = s / sqrt(n)

zvalue = qnorm(0.975)
zvalue


#Margin of error 
moe = zvalue * standardError

xbar = mean(ToothGrowth$len)
xbar + c(-moe,moe)

##T-based confidence interval for mean 

##Use in cases n < 30 

tval = qt(0.975, df=n-1)

tval
#Close to the Z value 

moe = tval * SE #Margin of error 

t.test(ToothGrowth$len) #95% ci for mean 
t.test(ToothGrowth$len,conf.level = 0.9)


#Confidence interval provides a measure of precision of the 
#linear regression coefficient 

library(ggplot2)
#Statistical model is estimating parameter from a dataset. 
fit = lm(circumference ~ age , data = Orange)
summary(fit)
ggplot(Orange, aes(x=age,y=circumference)) +
  geom_point(color="#2990B9",size = 4) + 
  geom_smooth(method=lm, color = "#2C3E50")
#Gray bands give an estimate of the confidence interval of the 
#regression line. 

new.data = data.frame(age=1500)
predict(fit,newdata = new.data, interval = "conf")
#Predicts the age, upper and lower limit. 

confint(fit)
 
#We can force intercept to be zero if we are sure that it is 
#not significant 
fit0 = lm(circumference ~ age + 0, data = Orange)

summary(fit0)



#RElationship between Anova and linear regression 
fit1 = lm(Sepal.Length ~ Petal.Length, data=iris)
summary(fit1)
anova(fit1)


#Multiple Linear Regression 
#One Response variable and two or more predictors
data(iris)
head(iris)
fit1 = lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
summary(fit1)
fit2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
summary(fit2)

#Addition of categorical variables 
#Accounting for interaction of predictor variables.
data(iris)
library(ggplot2)

qplot(Sepal.Length, Petal.Length, data = iris)
fitlm = lm(Petal.Length ~ Sepal.Length, data=iris)
summary(fitlm)
summary(iris)
qplot(Sepal.Length, Petal.Length, data=iris, color = Species)

x = lm(Petal.Length ~ Sepal.Length+Species, data = iris)
summary(x)

#Is there a significant variation in petal length across species 

fit1 = lm(Petal.Length ~ Sepal.Length* Species, data = iris)
anova(fit1)
#anova show that the Species affects the Petal.Length at 
#a significanct level i.e. predictor affecss the response 
summary(iris$Species)
summary(fit1)

#Check for conditions met for OLS regression
fit = lm(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

fir = lm(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

#QQplot shows that the errors follow a normal distribution

library('lmtest')
#Test for Autocorrelated/non-independence of errors 

#H0 is that there is no autocorrelation

dwtest(fit)
#WE fail to reject the H0

library(car)
#H0 hypothesis of constant error variance i.e. no hetereoscedasticity

ncvTest(fit)


#Identify outliers that have too much influence of model 
#influential datapoints 
cutoff = 4 /((nrow(iris) - length(fit$coefficients)-2))
par(mfrow=c(1,1))
plot(fit, which = 4,cook.levels = cutoff)
