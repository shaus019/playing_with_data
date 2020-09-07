rm(list=ls())
library(rpart)
library(classifly)
library(rpart.plot)

# load the required data, clean and remove the unnecessary variables and outliers
data <- read.csv('day.csv',sep = ",",header=T) # load the data
count_ts = ts(data[, c('bikes')]) # to create time series object
head(count_ts)
data$clean_bikes = tsclean(count_ts) # Remove the outliers and add a new cloumn for clean_bikes
head(data)
dataClean <- data[,-c(1,2,14,15,16)] # remove instant, dteday, casual, registered, bikes from the data.
head(dataClean)

r2 <- matrix(ncol = 20, nrow = 50)
for (x in 1:20) {
  for (v in 1:50) {
    dataSplit <- sample.int(n=nrow(dataClean), size = floor(.70*nrow(dataClean)), replace = FALSE) # split the data
    trainData <- dataClean[dataSplit,] # get the training data
    testData <- dataClean[-dataSplit,] # get the testing data
    rp <- rpart(clean_bikes ~ ., data = trainData,
                control=rpart.control(maxdepth=x,
                                      minsplit=2,
                                      minbucket=2,
                                      cp=0))
    yhat <- predict(rp,
                    newdata=testData,
                    type="vector")
    y <- testData$clean_bikes # test data
    yhat <- predict(rp, testData) # prediction using all test data
    rsqr <- 1 - sum((y - yhat)^2) / sum((y - mean(y))^2) # R-squared measure
    r2[v,x] <- rsqr
    
    
  }
  
}


print(r2)
r3<- sample(0,20,replace= TRUE)
for (r in 1:20) {
 r3[r] <-  mean(r2[,r])
  
}

print(r3)
plot(1:20,r3, type = 'l',xlab = "Tree Depth", ylab = "R^2", col = "orange")
