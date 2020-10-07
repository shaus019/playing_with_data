# 1. Visualize and discuus the different ROI and risks assossiated with Stocks 
# in addistion , load, visualise and give a simple interpretation for corr(1).tab
invest <-read.table("invest.tab")
head(invest)
plot(invest)
View(invest)
plot(invest$ROI,invest$Risk, xlab = "ROI",ylab = "Risk",main = "Plot for Risk over ROI",col=invest$Type,pch=invest$Type)
# Add a legend 1 stock 2 for bond three for cash
legend(5,0.35,legend=unique(c("Stock","Bond","Cash")),col = unique(invest$Type),pch = unique(invest$Type))


correlation <- read.table("corr(1).tab") # load the correaltion data
head(correlation)
View(correlation)
convert<-as.matrix(correlation) # convert the data into a matrix
library(MASS)
library(corrplot)
corrplot(convert)
View(convert)

# 5. Create another objective to minimze the correaltion for the investemnt blend.
selected <- which(convert >= minAMOUNT)
removeDuplicates <- unique(selected)
corr <- sum(correlation)  # Just the sum of risk
