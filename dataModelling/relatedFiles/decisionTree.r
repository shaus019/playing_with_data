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

dt.control <- rpart.control(maxdepth=30) # 0 is root node
credit.model <- rpart(dataClean$clean_bikes ~ ., data = dataClean, control = dt.control)

plot(credit.model, branch=0, compress=TRUE, main="Bikes Number  Decision Tree")
text(credit.model, pretty=0, use.n=TRUE, fancy=FALSE, all=FALSE, cex=0.75, fwidth=0.25, xpd=TRUE)

# Simple visualisation of tree:
#
# prp(credit.model)
#
summary(credit.model)

#########################################################
# How many go to the right of the top node
##########################################################
right.tree <- which((credit$checking_status=='>=200') | 
                      (credit$checking_status == 'no checking'))
#
# How many to the right are good?
summary(credit[right.tree,"class"])
#