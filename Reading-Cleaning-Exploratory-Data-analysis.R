setwd("C:\\Users\\kora\\Documents\\statistics\\Regression Analysis for Statistics and Machine Learning in R [FCO]\\R-Scripts-Regression-Analysis")

#Randomly distributed NAs
data("airquality")
??airquality
str(airquality)
airquality

aq = na.omit(airquality)
head(aq)

aq2 = airquality[complete.cases(airquality),]
head(aq2)

#Replace NAs with 0 
aqty = airquality 
aqty[is.na(aqty)] <- 0
head(aqty)
summary(aqty)
str(aqty)

#Replacing missing values with average 

meanOzone = mean(airquality$Ozone,na.rm = T)
#ifelse function replaces the values based on the test function.
aqty.fix = ifelse(is.na(airquality$Ozone),meanOzone,airquality$Ozone)
summary(aqty.fix)

summary(airquality$Ozone)
#Visualize patterns of NAs
library(mice)
aqty2 = airquality
md.pattern(aqty2)

library(VIM)
mp = aggr(aqty2, col=c('navyblue','yellow'),
          numbers = TRUE, sortVars=TRUE, 
          labels=names(aqty2),cex.axis=.7,
          gap=3, ylab=c('Missing data','Pattern
                        '))
#Our results show that 72.5 of observations have no missing values 
##22.9 have missing values in Ozone 

#Impute
#Imputation techniques 
#500 uterations of predictive mean mapping for imputing 
im_aqty = mice(aqty2, m=5, maxit=50, method = 'pmm', seed = 500)
summary(im_aqty)
#Values imputed in ozone
head(im_aqty$imp$Ozone)

#Get back the completed data 
completedData = complete(im_aqty,1)
head(completedData)

##Set working directory to local location of datasets 
setwd("C:\\Users\\kora\\Documents\\statistics\\Regression Analysis for Statistics and Machine Learning in R [FCO]\\Exercise Files\\CODES\\Data")
eco = read.csv("countries_ecologicalF.csv")
head(eco)
str(eco)
#we have to remove the $ and comma sign from the GDP per capita column
eco$GDP = as.numeric(gsub("[\\$,]","",eco$GDP.per.Capita))
head(eco)
str(eco)
ncol(eco)

#Since there are many columns, we just want to select a few of them 
mdf = names(eco) %in% c("Population..millions","GDP","Grazing.Footprint","HDI",
                        "Carbon.Footprint","Cropland","Forest.Land","Urban.Land",
                        "Total.Biocapacity")
newdata = eco[mdf]
head(newdata)

#To exclude them 
x = eco[!mdf]


#To rename column names 
names(newdata)[names(newdata) == "Population..millions"] = "population"
names(newdata)[names(newdata) == "Total.Biocapacity"] = "BiocapacityT"
head(newdata)
newdata = na.omit(newdata
                  )
head(newdata)
write.csv(newdata, "biocap.csv")
