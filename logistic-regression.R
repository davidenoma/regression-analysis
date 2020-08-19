#There are situations when we have a non-normal error structure 

#GLMs are used when we have non normal error structure. 

#Resisuals are not normal 

#e.g. Proportions, count data, binary response 

#Logistic Regression 
library(ggplot2)
ggplot(mtcars, aes(x=wt, y =am))+
    geom_point() + stat_smooth(method = "glm", method.args = list(family="binomial"),se = FALSE)
head(mtcars)

fitglm = glm(am~hp+wt, data=mtcars,family = binomial(link = 'logit'))
#to get the odds of success 
exp(coef(fitglm))

summary(fitglm)
#The null deviance shows how well the response variable is 
#predicted by a model that includes only the intercept (grand mean).
#DF number of observations-1

#The residual deviance shows how well the response variable is 
#predicted by a model that includes both predictor vars (DF declines by 2 more)

#residual deviance for a well-fitting model 
#should be approximately equal to its degrees of freedom

#-----------------------------------

# how well does the model fit the data
#Hosmer and Lemeshow goodness of fit (GOF) test
library(ResourceSelection)
hoslem.test(mtcars$am,fitted(fitglm))
#the p=value is greater than 0.05 so we fail to reject the hypothesis 
#that there is no difference  i.e the model fits well 

newdata = data.frame(hp=120,wt=2.8)
#Preict values of uknown datat
predict(fitglm, newdata, ttype = "response")


#Overdispersion

library(arm)
x = predict(fitglm)
y = resid(fitglm)

binnedplot(x,y)


library(AICcmodavg)
data("beetle")
head(beetle
     )
qplot(Dose,Mortality_rate,data=beetle)
b$survive=b$Number_tested-b$Number_killed

fitglm2= glm(cbind(Number_killed,survive)~Dose,data=b,family=binomial)
#logistic transformation converts proportions to logits

summary(fitglm2)
#robust fit  residual deviance/redsidual df are almost 1:1

#check overdispersion
x=predict(fitglm2)
y=resid(fitglm2)

binnedplot(x,y)



##Logistic regression with Binary response variable 
library(caret)
setwd("C:/Users/kora/Documents/statistics/Regression Analysis for Statistics and Machine Learning in R [FCO]/R-Scripts-Regression-Analysis")
voice2 = read.csv('voice.csv')
head(voice2)
set.seed(99)
Train = createDataPartition(voice2$label, p = 0.75, list = FALSE)
#SPLIT DATA OIN5 25 ratio

summary(voice2)

#Training Data using 10 fold cross validation
train_control <- trainControl(method="cv",number = 10)

training = voice2[Train,]
testing = voice2[-Train,]
mod_fit = train(label~., data = training, trControl = train_control,
                method = "glm", family = "binomial")

#Estimates from Logistic regression characterize the relationship between 
#The predictor and the response on a log-odds scale 

summary(mod_fit)

#We fit another model and in this case we fit only the significant details
#of the first model fit. 
mod_fit2  = train(label~Q25+Q75+sp.ent+sfm+mode+meanfun+minfun,
                  data = training, trControl = train_control,
                  method = "glm", family = "binomial")


summary(mod_fit2)
#estimate results means that a unit increase in Q25 causes 
#reduces the log odds by 55 but this does not make much sense. 

exp(coef(mod_fit2$finalModel))
    

#A unit increse in mode will increse the chance of being a male 
#by 260 
p1 = predict(mod_fit2, newdata=testing)
p2 = predict(mod_fit2, newdata=testing, type="prob")

head(p1)
head(p2)
#Compare with the labels in the actual data set
accuracy <- table(p1,testing[,"label"])
accuracy
sum(diag(accuracy))/sum(accuracy)

#see importance of the different predictors 
#this is from the caret package
varImp(mod_fit2)

#Since there is no exact equivalent to the R2 of the linear regression,
#the McFaden R2 index can be used to assess the model fit

mod_fit2a=glm(label~Q25+Q75+sp.ent+sfm+meanfun+minfun+mode,data=training,family="binomial")
library(pscl)
pR2(mod_fit2a)


##MULTINOMIAL LOGISTIC REGRESSION 
library(caret)
glass = read.csv('glassClass.csv')
head(glass)

#Y-var type that indicates the glass type (1-7)
#numbers ate the categories so we can set them as factors.
glass$Type = as.factor(glass$Type)
table(glass$Type)


set.seed(99)
Train = createDataPartition(glass$Type, p =0.75, list=FALSE)


training = glass[Train,]
testing = glass[-Train,]

#Neural network based modeling for the neural network
library(nnet)
gModel <- multinom(Type~.,data = training, maxit=500,trace = T)
#Variable importance
varImp(gModel)

#Now we predict the glass class 

p1=predict(gModel,type="class", newdata=testing)
p2=predict(gModel,type="probs", newdata=testing)
head(p1)
head(p2)

#Tesing Accuracy 
#From the caret package.
postResample(testing$Type,p1)
