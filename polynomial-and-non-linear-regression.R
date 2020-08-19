#Poisson and non-linear regression 
#That is for variables that don't have a linear relationship

q = seq(0,100,1)
p = 0.6
y = 500 + p*(q - 10) ^3
plot(q,y,type='l',col='red',main='Non Linear Relationship',lwd=5)

data("chickweight")
head(ChickWeight)

cw1 <- subset(ChickWeight,Diet=='1')
head(cw1)
cw1
plot(weight~Time,data=cw1)

fit1 = lm(weight ~Time, data = cw1)
summary(fit1)
AIC(fit1)

fit2 = lm(weight ~Time + I(Time*Time), data = cw1)
summary(fit2)
AIC(fit2)

fit3 = lm(weight ~Time + I(Time*Time) + I(Time * Time * Time), data = cw1)
summary(fit3)
AIC(fit3)

fit2a = lm(weight ~ poly(Time ,2), data= cw1)
summary(fit2a)
AIC(fit2a)

fit3a = lm(weight~poly(Time,3), data=cw1)
summary(fit3a)
AIC(fit3a)


#Non Linear Regression 
#This is where Linear models cannot be used. 
library(nls2)
data("Loblolly")
str(Loblolly)
head(Loblolly)
x = Loblolly$age
y = Loblolly$height

plot(Loblolly$age,Loblolly$height)
m <- nls(y ~ a + b * I(x^z), start = list(a=1,b=1,z=1) )

lines(x,fitted(m), lty=2, col="red",lwd=2)
qqnorm(residuals(m))
qqline(residuals(m))

RSS = sum(residuals(m) ^2)#Resisdual Sum of squares
TSS = sum(y - mean(y) ^2)#Total sum of square
R.square = 1 - (RSS/TSS)
R.square


#Generalized Additive Models 

#Generalized additive models are for non linear interaction 
#between the variables. 
setwd("C:/Users/kora/Documents/statistics/Regression Analysis for Statistics and Machine Learning in R [FCO]/R-Scripts-Regression-Analysis")
eco = read.csv('biocap.csv')
head(eco)
library(car)
scatterplotMatrix(eco, diagonal="histogram", smooth = FALSE)
library(gam)
library(mgcv)

mod_lm = gam(BiocapacityT ~Population+HDI+Grazing.Footprint+Carbon.Footprint+
               Cropland + Forest.Land + Urban.Land +GDP, data=eco)

summary(mod_lm)


#Fit an actual generalized additive model using the 
#same cubic spline as our basis 

mod_lm2 = gam(BiocapacityT ~ s(Grazing.Footprint) + s(Cropland) + s(Forest.Land),
              data = eco)
summary(mod_lm2)
x = lm (BiocapacityT ~Grazing.Footprint + Cropland 
        +Forest.Land, data=eco)
summary(x)

#problems with gam is concurvity. 
#To check for colinearity 
concurvity(mod_lm2)

mod_gam3 <- gam(BiocapacityT ~te(Grazing.Footprint,Cropland,Forest.Land), data=eco)
summary(mod_gam3)

vis.gam(mod_gam3, type='response',plot.type = 'persp',
        phi=30, theta=30, n.grid = 500, border = NA)

library(caret)
b = train(BiocapacityT ~. ,
          data = eco , 
          method = "gam",
          trControl = trainControl(method = "LOOCV",number = 1,
          repeats = , tuneGrid = data.frame(method = "GCV.Cp",select = FALSE)))

#Boosted GAM 
#Boosted Generalized additive models 
library(caret)
library(mboost)
eco = read.csv("biocap.csv")
head(eco)
#Gamboost Fitting
set.seed(123)

Train = createDataPartition(eco$BiocapacityT, p = 0.75, list = FALSE)
#SPLIT DATA in 75 25 ratio

training = eco[Train,]
testing = eco[-Train,]

fitControl = trainControl(method = 'cv', number = 10)

Grid <- expand.grid(.mstop=seq(100,1000,100),.prune =c(5))

formula = BiocapacityT ~ Population+HDI+Grazing.Footprint+Carbon.Footprint+
  Cropland+Forest.Land+Urban.Land+GDP


fit1 = train(formula, data=training, method = 'gamboost', trControl=fitControl,tuneGrid=Grid,metric='RMSE',maximize=FALSE)
print(fit1)
fit1
varImp(fit1,scale=TRUE)

v = varImp(fit1, scale = TRUE)
plot(v)

p1 = predict(fit1, newdata = testing)
p = as.data.frame(p1)
test = as.data.frame(testing$BiocapacityT) #Actual respoinse valuse
y = cbind(test,p)
colnames(y) = c("BiocapacitT","Pred")
head(y) #ACtual and predicted 
cor.test(y$BiocapacitT,y$Pred)
#Correlation test 

library(Metrics)
rmse(y$BiocapacitT,y$Pred) #Root mean square error
