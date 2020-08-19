#Classification and regression Trees

#they do not apply a global model like OLS 
#USEFUL FOR HIGH NON LINEARITY AND COMPLEX RELATIONSHIP 
#BETWEEN DEPENDENT AND INDEPENDENT VARIABLES 

library(caret)
library(rpart
        )
eco = read.csv("biocap.csv")
names(eco)
set.seed(10)
inTraining = createDataPartition(eco$BiocapacityT, p=0.75,list = FALSE)
training = eco[inTraining,]
testing = eco[-inTraining,]

fit = rpart(BiocapacityT ~., data=training, control=rpart.control(minsplit = 6))
fit
printcp(fit)
plotcp(fit)
summary(fit)
par  (mfrow=c(1,2))
rsq.rpart(fit) #Visualize cross validation results 

#plot tree 
plot(fit, uniform=TRUE, main="REgression Tree for Biocapacity")
text(fit, use.n = TRUE, all=TRUE,cex=.8 )

#Pruning the tree prevents overfitting of the dat 

pfit <- prune(fit,cp=0.016)
#cp is the best performance that we are getting

plot(pfit, uniform = TRUE, main= "Pruned regression tree foe mileage")
text(pfit,use.n = TRUE, all=TRUE, cex=.8)

#How well does this model generalize 
p= predict (fit, newdata = testing)
head(p)       
p = as.data.frame(p)
final = cbind(testing$BiocapacityT,p)
head(final)
cor(testing$BiocapacityT,p)
library(Metrics)
rmse(testing$BiocapacityT,p)

##Conditional Inference trees

######### Conditional inference tree- R #######
######## does away with the need of tree pruning

setwd("F:\\RegressionModelling_R\\Lectures\\section7")
library(caret)

library(party)

eco=read.csv("biocap.csv")

names(eco)

set.seed(10)

inTraining =createDataPartition(eco$BiocapacityT, p = .75, list = FALSE) #create a 75% data partition

training = eco[ inTraining,] #75% data for model training
testing= eco[-inTraining,] #25% for model testing

fitc= train(BiocapacityT ~ ., data=training, method='ctree',tuneGrid=expand.grid(mincriterion=0.95))

ctreeVarImp = varImp(fitc) #variable importance

ctreeVarImp

plot(ctreeVarImp)
##### use party package- visualize the tree 

library(party)
fit2=ctree(BiocapacityT ~ ., data=training)

plot(fit2, main="Conditional Inference Tree for Biocapacity")

print(fit2)

#######################################################################
############ how will this generalize?

p=predict(fitc, newdata =testing) #testing was the 25% original data not used in model building

head(p)

p=as.data.frame(p)

final=cbind(testing$BiocapacityT,p) #how do predicted values (p) compare with actual test data

head(final)

cor(testing$BiocapacityT,p)

library(Metrics)

rmse(testing$BiocapacityT,p)
