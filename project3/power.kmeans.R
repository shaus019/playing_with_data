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
	
	colnames(tab) <- c("mean","var","sum","max12")  # labels for each column
	
	as.data.frame(tab)   # Return the final table
}
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




