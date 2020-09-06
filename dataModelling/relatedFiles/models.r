library(ggplot2)
library(forecast)
library(tseries)

library(rpart)
library(classifly)
library(rpart.plot)

# The following command can be used to clear the working memory of R
rm(list=ls())

### Linear Model 1 #####
items <- read.csv('pdata.csv',sep = ",",header=T) # load the data
names(items)
#attach(items)
a <- lm(y~x-1, data = items) # create a linear regression model
plot(items$x,items$y,xlab = "givenX", ylab = "predictedY", main = "model 1")
abline(a, col='red', lwd = 2) # plot the line for the best fit
summary(a)



### MODEL 2 ####
x <- items$x
x2 <- x^2
x3 <- x^3

b <- lm(y~x+x2+x3,data = items)
fitModel <- fitted(b) # to extract the fitted values from the object b returened by lm.
plot(x,y) # ????i was giving it data instead of specifying x and y.
lines(x,fitModel, col = "yellow",lwd=3.5)
abline(a, col='red', lwd = 2)
legend(0, 200, c("model 1", "model 2"), col = c("red","yellow"),
       text.col = "black",lty = 1, lwd = 2,
       bg = "gray90")
summary(b)




### PART 2: TIMESERIES ####
## Section 1: is just about visualizing the data against the bike attribute and explaing it##


data <- read.csv('day.csv',sep = ",",header=T) # load the data
data <- data[,-1] # #Remove explanatory "instant" coloumn, which is just the id for each exapmle.
head(data)

boxplot(data$bikes~data$season,  col = c("red","yellow", "orange", "brown"), xlab = "Season", ylab = "Number of Bikes")
boxplot(data$bikes~data$yr,  col = c("orange", "brown"), xlab = "Year", ylab = "Number of Bikes")
boxplot(data$bikes~data$mnth,  col = c("orange", "brown","orange", "brown","red", "green","grey", "black","pink","orange", "brown","red"), xlab = "Month", ylab = "Number of Bikes")
boxplot(data$bikes~data$holiday,  col = c("orange", "brown"), xlab = "Holiday", ylab = "Number of Bikes")
cor(data$bikes,data$holiday)
boxplot(data$bikes~data$weekday,  col = c("orange", "brown","red", "green","grey", "black","pink"), xlab = "WeekDay", ylab = "Number of Bikes")
cor(data$bikes,data$weekday)
boxplot(data$bikes~data$workingday,  col = c("red", "green"), xlab = "WorkingDay(0) Holiday(1)", ylab = "Number of Bikes")
cor(data$bikes,data$workingday)
boxplot(data$bikes~data$weathersit,  col = c("orange", "brown", "green","grey"), xlab = "Weather", ylab = "Number of Bikes")
cor(data$bikes,data$weathersit)

plot(data$temp,data$bikes, xlab = "normalized temperature", ylab ="number of bikes", main = "Temperature vs Bikes" ) # normalized temperature
plot(data$atemp,data$bikes, xlab = "another temperature", ylab ="number of bikes", main = "aTemperature vs Bikes" ) # normalized temperature
plot(data$hum,data$bikes, xlab = "Humidity", ylab ="number of bikes", main = "Bikes over Humidity" ) # Humidity
plot(data$windspeed,data$bikes, xlab = "normalized wind speed", ylab ="number of bikes", main = "Bikes over Wind" ) # normalized temperature
cor(data$windspeed,data$bikes)


## Section 2: Task 2 clean the date and remove the outliers ##
count_ts = ts(data[, c('bikes')]) # to create time series object
head(count_ts)
data$clean_bikes = tsclean(count_ts) # Remove the outliers and add a new cloumn for clean_bikes
head(data)

##Section 2: Task 3 Build  linear model for predicting count##
dataClean <- data[,-c(1,13,14)] # remove instant, dteday, casual, registered from the dat.
head(dataClean)


accessModelQuality <- function(dataClean){ ##To access the quality of the model.##
  dataSplit <- sample.int(n=nrow(dataClean), size = floor(.70*nrow(dataClean)), replace = FALSE) # split the data
  trainData <- dataClean[dataSplit,] # get the training data
  testData <- dataClean[-dataSplit,] # get the testing data
  bikes.lm <- lm(clean_bikes ~ . , trainData)
  #summary(bikes.lm)
  #season <- lm(clean_bikes~trainData$season, trainData)
  #summary(season)
  #anova(season,bikes.lm)
  #head(trainData)
  #dataFactor <- trainData[,-c(3,6)]
  #head(dataFactor)
  #factors.lm <- lm(clean_bikes~.,dataFactor)
  #summary(factors.lm)
  #anova(factors.lm,bikes.lm)
  y <- testData$clean_bikes # test data
  yhat <- predict(bikes.lm, testData) # prediction using all test data
  rsqr <- 1 - sum((y - yhat)^2) / sum((y - mean(y))^2) # R-squared measure
  return (rsqr)
}



