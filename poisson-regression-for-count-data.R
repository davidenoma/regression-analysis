getwd()
#set wd to dataset location 

#Poisson Regression for count data 
#DAta mean and variance to be roughly similar
c = read.csv('canopycvr1.csv')
attach(c)
head(c)
mean(c$cover)
var(c$cover)


#mean and variance of the response
#are roughly similar so we can apply poisson regression 

fit = glm(cover~elev, data=c, family = poisson(link = log))
summary(fit)
cf = coef(fit)
cf
#elev is the slope  b
#i.e for a unit increase in elevation the increase in cover 
#is e ^ b
exp(cf[2]) #With every increase in elevation, the cover increases
#by a factor of 1.0002 

#inverse of the link function 

1 - exp(cf[2])

#Model selection 

fit2 = glm(cover~elev+tci, data=c, family = poisson)
summary(fit2)

#compare models 
#We can see from above that addin tci has not improved the model. 

anova(fit,fit2,test="Chisq")


#Categorical qualitatove varianble
fit3 = glm(cover~disturb * elev, data=c, family=poisson)
summary(fit3)
#iNT EHE CASE OF OVER DISPERSED DATWA WE USE NEGATIVE BINOMIAL

library(MASS)
fit4 = glm.nb(cover~elev,data=c)
summary(fit4)


#GOODNESS OF FIT TESTING
#Does the model fit2(poisson distribution) fit the data 
#Does the model fit (negative binomial) fit the data 

1 - pchisq(summary(fit2)$deviance, 
           summary(fit2)$df.residual)

1 - pchisq(summary(fit4)$deviance, 
           summary(fit4)$df.residual)

#if p-value is less than 0.05 it does not fit the data
#if it is greater then we accept the null that it firs the data


