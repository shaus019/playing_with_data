invest <-read.table("invest.tab")
head(invest)
plot(invest)
plot(invest$ROI,invest$Risk, xlab = "ROI",ylab = "Risk",main = "Plot for Risk over ROI",col="Blue")
