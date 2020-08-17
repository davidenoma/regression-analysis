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
  geom(color="")