library('mlbench')
library(help = mlbench)
data(BostonHousing)
str(BostonHousing)

summary(BostonHousing)
head(BostonHousing)

library(ggplot2)
library(car)
library(caret)
library(corrplot)

#Tackling multiple co-linearity. The presence of highly correlated 
#predictors 

#Dropping response variable Y 
mat_a = subset(BostonHousing, select = -c(medv))

#We only want numeric variables for compuating correlation
numeric <- mat_a[sapply(mat_a,is.numeric)]

#Calculating correlation betw`een two variables
descrCor <- cor(numeric)
print(descrCor)

corrplot(descrCor)

require(caret)
#Remove highly correlated variables 
highlyCorrelated = findCorrelation(descrCor, cutoff = 0.7)

#Identify variable names
highlyCorCol = colnames(numeric)[highlyCorrelated]
highlyCorCol


#Remove highly correlated variables from the original dataset 
#Create a new dataset.
data3 = BostonHousing[,-which(colnames(BostonHousing) %in% highlyCorCol)]
dim(data3)

#Variance inflation factor(VIF) can handle co-linearity as well 
fit = lm(medv~., data = BostonHousing )
summary(fit)

vif(fit)

##Check 
df = cbind(BostonHousing$medv, data3)
df = as.data.frame(df)
fit2 = lm(medv~.,data=df)
vif(fit2)

