  data(iris)
head(iris)  


fit1= lm(Sepal.Length~ Petal.Width, data=iris)
summary(fit1)

par(mfrow = c(2,2))
plot(fit1)

qqnorm(residuals(fit1))
qqline(residuals(fit1))


#The residuals do not fit the normal distribution so that in
#dictates a problem.

hist(iris$Sepal.Width)

#Try a transf0rmation of the response variable 
iris$Sepal.Width.sq <- sqrt(iris$Sepal.Width) #Sq root
iris$Sepal.Width.cub <- (iris$Sepal.Width) ^ (1/3) #Cube
iris$Sepal.Width.ln <- log(iris$Sepal.Width) #log

#we are attempting to get a bell curve distribution 

hist(iris$Sepal.Width.sq)
hist(iris$Sepal.Width.cub)
hist(iris$Sepal.Width.ln)

#Linear regresssion between square root of Y and X

fit2 <- lm(iris$Sepal.Width.sq ~ iris$Petal.Length)
summary(fit2)


#The square transformation has a better normal qqplot of the 
#

qqnorm(residuals(fit2))
qqline(residuals(fit2))


#We can transform both the response and predictor 
## That is log log transform 

fit3 <- lm(log(iris$Sepal.Width) ~ log(iris$Petal.Width))
summary(fit3)

plot(fit3)

#performing the back transform 
c = coef(fit3)
a = c[1]
a
b = c[2]
b
backtrans = exp(a + b*log(iris$Petal.Width))
head(backtrans)

#Box cox trnsformation
#They reduct the non normality of the errors in a linear model
library(MASS)
bc <- boxcox(Sepal.Width ~ Petal.Width, data=iris)
#log likelihood function governs the selection of the lambda 
#power transformation 
#We select lambda to carry out transformation 


trans <- bc$x[which.max(bc$y)]
trans
#This if the lambda(hyperparameter) that is going to maximize y
fit4 <- lm(Sepal.Width^trans ~ Petal.Width, data = iris )
summary(fit4)


