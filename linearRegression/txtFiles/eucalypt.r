## Foloowing is the R code is to help establish relationship between density and Janka hardness,
## which are the two variables from the data collected from 36 Australlian eucalypt hardwoods
## The Janka Hardness test measures the force required to push a steel ball into the wood.
eucalypt <- read.table("eucalypt.txt",header = T)
attach(eucalypt) # database is attached to R search path to access it directly by the name
names(eucalypt)

head(eucalypt)
tail(eucalypt)
plot(denst,hard,xlab = "Wood Density", ylab  = "Hardness")
a <- lm(hard~denst) # lm is used to fit linear models
plot(denst,hard,xlab = "Wood Density", ylab  = "Hardness")
abline(a) # this function just adds a straight line to the plot
summary(a) # this function produce resullt summaries of the results of various model fitting functions.
confint(a) # this function computes confidence intervals for onwe or more parameters in a fitted model.
anova(a) # computes analysis of variance or deviance tables for or more fitted model objects.

r = rstandard(a)
lf <- fitted(a)
plot(denst,r,xlab = "Wood Density", ylab  = "Standerdized Residual")
abline(h = 0)
plot(lf,r,xlab = "fitted values", ylab  = "Hardness")# fitted values also called predicted values
abline(h=0)
# to check the normality of the residual(error in the result)
qqnorm(r,xlab = "N(0,1) Quantities", ylab = "standerdized residual") 
abline(0,1)

h <- hatvalues(a)
cd <- cooks.distance(a)
plot(h,cd,xlab = "hat values",ylab = "Cook's Distance") # the plot shows that there is one influentaial point.


## Now lets examine if the quadratic term of the expalnatory variable will improve the model fit.
dens2 <- denst^2
b <- lm(hard ~ denst+dens2)
summary(b)
c <- lm(hard~dens2)
summary(c)
anova(b,c) # it shows that b and c are not significantly differnt so we will choose model c becuase it is simpler
AIC(a);AIC(b);AIC(c)
AIC(a) - AIC(b)
AIC(b) - AIC(c)
step(b)
377.05-369.06
369.06-367.49
confint(c) # c is y = 90.699+0.607^2.


# use model c to predict
plot(denst, hard,xlab = "wood density",ylab = "Hardness")
x <- seq(from= 24, to = 70, by = 1)
x2 <- x^2
y <- predict(c,list(dens2 = x2), interval = "confidence")
matlines(x,y,lty = c(1,2,2), col = "black")

# now ask for predicted hardness if the wood density was 57
x2<- 57^2
predict(c,list(dens2= x2), interval = "confidence")
r <- rstandard(c)
If < fitted(c)
plot(denst,r,xlab = "wood density",ylab = "standared residual")
abline(h = 0)
plot(If,r,xlab = "Fitted values",ylab = "standerdized residual")
abline(h=0)
qqnorm(r,xlab = "N(0,1) Quantities", ylab = "standerdized residual")
abline(0,1)
h <- hatvalues(c)
cd <- cooks.distance(c)
plot(h,cd,xlab = "hat values",ylab = "Cook's Distance") # the plot shows that there is one influentaial point.

## Transforming the response.

library(lmtest) # 0.05 is used as bundary for determining if a model is dependent or not on the explanatory variables.
bptest(hard~dens2) # to test whether the variance from model c is dependednt on the vaues of the explanatory variables
library(MASS)
par(mfrow=c(1,2))
boxcox(hard~denst+dens2) # to look for an otimal transformation of response varibale
boxcox(hard~denst+dens2,lambda = seq(-0.4,0.8,0.1)) # from the box plot analysis the most abviouis transformation would be for sigma = 0 is in the 95% confident interval
a2 <- lm(log(hard)~denst+dens2)
summary(a2)
anova(a2)
plot(denst,log(hard),xlab = "Wood Density",ylab = "Hardness", axes = F)
axis(1)
axis(2,at=log(seq(500,3500,1000)),seq(500,3500.1000))
box()
x <- seq(from=24,to = 70,by=1)
x2 = x^2
y <- predict(a2,list(denst = x, dens2= x2), interval = "confidence")
matlines(x,y,lty = c(1,2,2),col = "black")

# predicted median hardness and confidence interval for a wood density of 57
xp <- 57 
xp2<-xp^2
pred <- predict(a2,list(denst = xp, dens2 = xp2), interval = "confidence")
exp(pred)
r <- rstandard(a2)
If < fitted(a2)
plot(denst,r,xlab = "wood density",ylab = "standared residual")
abline(h = 0)
plot(If,r,xlab = "Fitted values",ylab = "standerdized residual")
abline(h=0)
qqnorm(r,xlab = "N(0,1) Quantities", ylab = "standerdized residual")
abline(0,1)
h <- hatvalues(a2)
cd <- cooks.distance(a2)
plot(h,cd,xlab = "hat values",ylab = "Cook's Distance") 
bptest(log(hard)~denst+dens2) # Test if the variance is still increasing

# Note: The model with the logrithm transformation of the reponse variable fits the data the best.
