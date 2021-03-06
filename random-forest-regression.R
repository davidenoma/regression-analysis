#Random Forest Regression 
#####################################################################
################ Random Forest (RF)#############################

setwd("F:\\RegressionModelling_R\\Lectures\\section7")
library(caret)
library(randomForest)
library(Metrics)

eco=read.csv("biocap.csv")

names(eco)

set.seed(10)

inTraining =createDataPartition(eco$BiocapacityT, p = .75, list = FALSE) #create a 75% data partition
training = eco[ inTraining,] #75% data for model training
testing= eco[-inTraining,] #25% for model testing

trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE, verboseIter = FALSE)

modfit = train(BiocapacityT ~ ., data = training, method = "rf", prox = FALSE, trControl = trControl,importance=TRUE)

p=predict(modfit, newdata =testing) #testing was the 25% original data not used in model building

head(p)

p=as.data.frame(p)

final=cbind(testing$BiocapacityT,p) #how do predicted values (p) compare with actual test data

head(final)

cor(testing$BiocapacityT,p)

library(Metrics)

rmse(testing$BiocapacityT,p)

varImp(modfit)
########### pkg randomForest###############################

library(randomForest)

erf = randomForest(BiocapacityT ~ ., eco)
VI_F=importance(erf)

VI_F

partialPlot(erf, eco, Population  , BiocapacityT)
#rf model, dataframe, x var, y var
#marginal response plots
# direction of relationship bw Biocapacity and Forest land 
