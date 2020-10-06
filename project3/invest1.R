invest <-read.table("invest.tab")
head(invest)
plot(invest)
View(invest)

#Use gg plot here agin to clour it on the basis of and jsut talk about how they re cahanging#
plot(invest$ROI,invest$Risk, xlab = "ROI",ylab = "Risk",main = "Plot for Risk over ROI",col=invest$Type,pch=invest$Type)
# Add a legend 1 for stock 2 for bond three for cash
legend(5,0.35,legend=unique(c("Stock","Bond","Cash")),col = unique(invest$Type),pch = unique(invest$Type))


correlation <- read.table("corr(1).tab") # load the correaltion data
head(correlation)
boxplot(correlation)
library(MASS)
library(corrplot)

correlationPlot <- cor(correlation)
corrplot(correlationPlot)
