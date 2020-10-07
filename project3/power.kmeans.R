############################################
# Creating explanatory variables and kmeans
############################################

###########################################################################
# simple.fn
#--------------------------------------------------------------------------
# Example simple function to be called by apply
# IN: x - the row of data for one household
#     fn - the simple function (mean, min, etc.) to apply to the vector x
#   col.range - the columns to select from x to apply fn. Defaults to all columns
# OUT: the result of applying fn to x for those columns
#########################################################################
simple.fn <- function(x,fn,cols=1:length(x))
{
    fn(x[cols])
}
###########################################################################
# build.table
#--------------------------------------------------------------------------
# Build the table of explanatory variables using the 
# original electricity data (p)
# IN: p - the original electricity dataset
# OUT: The constructed table of data to use with kmeans
# NOTE: Each row of p is a household
#       The columns of p are the 48 readings in time for each household
#############################################################
build.table <- function(p)
{
	tab <- apply(p,1,simple.fn,mean)  # mean over all time periods
	tab <- cbind(tab,apply(p,1,simple.fn,var))
	tab <- cbind(tab,apply(p,1,simple.fn,sum))	
	tab <- cbind(tab,apply(p,1,simple.fn,max,1:12)) # Max for first 12 time periods (6 hours from 12:30am)
	tab <- cbind(tab,apply(p,1,simple.fn,sum,12:19))
	tab <- cbind(tab,apply(p,1,simple.fn,min))
	
	colnames(tab) <- c("mean","var","sum","max12","sumoverperiod","min")  # labels for each column
	
	as.data.frame(tab)   # Return the final table
}
table <- build.table(p)
####################################################################
# do.cluster
#--------------------------------------------------------------------
# IN: power.table  - the table of explanatory variables from build.table
#     num.clusters - number of clusters for kmeans
# OP: Performs kmeans clustering for num.clusters
# OUT: The result of the clustering algorithm
# NOTE: Assumes that the power.table has rows for each household, and
#       columns for the explanatory variables
#########################################################################
do.cluster <- function(power.table,num.clusters=6)
{	
	kmeans(scale(power.table),centers=num.clusters)
}
sixclusters <- do.cluster(table, num.clusters = 6)
summary(sixclusters)

bindedTable <- cbind(p,sixclusters$cluster)
colnames(bindedTable)[49] <- "cluster"#add a cloumn for cluster of each house hold.
par(mfrow=c(3,3))
par(mar=c(3,5,1,1))
c1 <- which(bindedTable$cluster==1)
c2 <- which(bindedTable$cluster==2)
c3 <- which(bindedTable$cluster==3)
c4 <- which(bindedTable$cluster==4)
c5 <- which(bindedTable$cluster==5)
c6 <- which(bindedTable$cluster==6)
boxplot(bindedTable[c1,],xlab = "time", ylab = "Usage",main="cluster1")
boxplot(bindedTable[c2,],xlab = "time", ylab = "Usage",main="cluster2")
boxplot(bindedTable[c3,],xlab = "time", ylab = "Usage",main="cluster3")
boxplot(bindedTable[c4,],xlab = "time", ylab = "Usage",main="cluster4")
boxplot(bindedTable[c5,],xlab = "time", ylab = "Usage",main="cluster5")
boxplot(bindedTable[c6,],xlab = "time", ylab = "Usage",main="cluster6")

par(mfrow=c(1,1)) # to exit the plots on the same

# plot showing th mean usage for each time stamp for each cluster.
par(mfrow=c(3,3))
par(mar=c(3,5,1,1))
plot(colMeans((bindedTable[c1,])),type = "l", xlab = "Time", ylab = "Usage", main = "Cluster 1 Mean", col="red") 
plot(colMeans((bindedTable[c2,])),type = "l", xlab = "Time", ylab = "Usage", main = "Cluster 2 Mean", col="red")
plot(colMeans((bindedTable[c3,])),type = "l", xlab = "Time", ylab = "Usage", main = "Cluster 3 Mean", col="red")
plot(colMeans((bindedTable[c4,])),type = "l", xlab = "Time", ylab = "Usage", main = "Cluster 4 Mean", col="red")
plot(colMeans((bindedTable[c5,])),type = "l", xlab = "Time", ylab = "Usage", main = "Cluster 5 Mean", col="red")
plot(colMeans((bindedTable[c6,])),type = "l", xlab = "Time", ylab = "Usage", main = "Cluster 6 Mean", col="red")

par(mfrow=c(1,1)) # to exit the plots on the same

# Select two explanatory variables and plot them clouring each point by cluster
## load the library first
plot(table$mean,table$min, xlab = "Mean", ylab = "Minimum",col =bindedTable$cluster,pch=bindedTable$cluster)
# Add a legend
legend(0.1,0.35,legend=unique(bindedTable$cluster),col = unique(bindedTable$cluster),pch = unique(bindedTable$cluster))

       