#
# Electricity patterns
# Time series, one day, equal spacing, 30 mins per sample
# Starts at midnight
#
p <- read.csv('power.csv',header=FALSE)

# Since first column is the place, let's change it to the row name and remove it
#
rownames(p) <- p[,1] # Set rownames to the household ids
p <- p[,2:ncol(p)] # Remove the first column since this the holdhold ids
colnames(p) <- seq(from=1,to=48,by=1)  # Rename columns to be the sample "time"

# We now have a data frame with rows labelled by site.
# Note that when we need to plot an individual time series we 
# turn it into a numeric vector.

par(mfrow=c(1,2))
par(mar=c(5,5,1,1))
boxplot(p,xlab="Time",ylab="Electricity Usage", main="All Households")
#
# Plot the first household data
plot(as.numeric(p[5,]),type="l",xlab="Time",ylab="Electricity Usage",main="One Household")
#
par(mfrow=c(1,1)) # turn display back to single figure

