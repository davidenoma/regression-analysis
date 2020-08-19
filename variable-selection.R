#Variable Selection 


#Understanding variable importance of regression models 

library(relaimpo)

#percent contribution of the differenct predictors 

fit1 = lm(medv~.,data=BostonHousing)
calc.relimp(fit1, type=c("lmg"), rela = TRUE)

#R^2 partitioned 

library(hier.part) #Variance Partitioning 

#Determine the amount of Y variance explained by each #of the l2 predictors


x = BostonHousing[,1:12]

H = hier.part(BostonHousing$medv, x , fam="gaussian", gof = "Rsqu")
par(mfrow=c(1,1))
H$I.perc
#this shows that rm contributes most to the model.

library(caret)
varImp(fit1, scale=FALSE)
#This is a black box method


