##########################################################################
# sp500.r
##########################################################################
# Load time series and stock data libraries
##########################################################################
# NOTE: Just run the install.packages ONCE!
############################################
# install.packages("DMwR")
#############################################
library(xts)
library(DMwR)
library(quantmod)
#
# Read the csv file with sp500 data and convert to a time series object
########################################################################
sp500 <- as.xts(read.zoo("sp500.csv",header=T))
# Finally, plot a candleChart for the last 3 months of the dataset

chartSeries(last(sp500,"1 months"), theme='white')

## 1. Describe what the candles are showing?
#shows Last assoasiated with how long the dta will last
chartSeries(last(sp500,"1 months"), theme='white')
head(sp500)
## 2. The daily average price can be approximated by:(high+close+low)/3
## code for the daily average price and plot at the end.
high <- sp500[,2] # select the second column for high.
close <- sp500[,4] # select the 4th column for the close.
low <- sp500[,3] # select the 3rd column for low.
head(close)
average <- (high+close+low)/3 # Average for the data.

sp500$average <- c(average) # Add the average column to the data.
plot(average, main = "Plot for the Average", ylab = "Average",col = "red") # Plot the result

## 3: The daily return is defines as return(t) = (c(t) - c(t -1))/c(t-1)
# 3.1: Calculate the return for the last 12 months and Compare it with daily return candle chart.


previousDay <- close[-1] # get c(t-1), which is the close for the previous date.

dailyReturns <- as.numeric(close)# make it numeric values.
# Calculate the daily return
dailyReturns[2:length((dailyReturns))] <- 
  (dailyReturns[2:length((dailyReturns))] - dailyReturns[1:length((dailyReturns) - 1)])/dailyReturns[1:length((dailyReturns) - 1)]
head(dailyReturns[2:length((dailyReturns))])
head(dailyReturns)
sp500$return <- dailyReturns # Adding a column to the data for our dailyreturn
plot(last(sp500$return, "12 months")) # plot the dailyreturn for the last 12 months.
chartSeries(last(sp500,"12 months"), theme='white') # Candlechart for the last 12 months.
###########################################################################