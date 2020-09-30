invest <-read.table("invest.tab")
head(invest)
plot(invest)
plot(invest$ROI,invest$Risk, xlab = "ROI",ylab = "Risk",main = "Plot for Risk over ROI",col="Blue")

correlation <- read.table("corr(1).tab") # load the correaltion data
head(correlation)
boxplot(correlation)
library(MASS)
library(corrplot)

correlationPlot <- cor(correlation)
corrplot(correlationPlot)
