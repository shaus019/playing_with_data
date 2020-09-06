library(ggplot2)
library(forecast)
library(tseries)

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
head(data)
#Remove explanatory "instant" coloumn, which is just the id for each exapmle.
data <- data[,-1]
head(data)
min(data$bikes)
plot(data$temp,data$bikes, xlab = "number of bikes", ylab ="normalized temperature" ) # normalized temperature
cor(data$temp,data$bikes)
barplot(data$bikes,data$holiday, xlab = "number of bikes", ylab ="holiday" ) # 0 and 1 weather day is holiday or not
cor(data$holiday,data$bikes) #  -0.06834772
plot(data$season,data$bikes, xlab = "season", ylab ="number of bikes" ) # season (1:winter, 2:spring, 3:summer, 4:fall)
cor(data$season,data$bikes) # 0.4061004
plot(data$workingday,data$bikes, xlab = "workingday", ylab ="number of bikes" ) # if day is neither weekend nor holiday is 1, otherwise is 0.
plot(data$yr,data$bikes, xlab = "year", ylab ="number of bikes" )     # (0: 2011, 1:2012) 
plot(data$mnth,data$bikes, xlab = "month", ylab ="number of bikes" ) #( 1 to 12) 
plot(data$weekday, data$bikes)# day of the week 
plot(data$weathersit, data$bikes)# 1 clear 2 cloudy 3 light snow, 4 heavy rain
head(data$weathersit)
tail(data$weathersit)
data$dteday = as.Date(data$dteday,format="%d/%m/%Y")  # Convert date column to proper Date class
ggplot(data, aes(dteday, bikes)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") + xlab("")
ggplot(data, aes(season, bikes)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") + xlab("")
