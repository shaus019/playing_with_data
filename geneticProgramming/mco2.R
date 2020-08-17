# The following command can be used to clear the working memory of R
#rm(list=ls())
###################
####################################################################
# Example of multi-criteria optimisation using the mco package
# Backpack example, but using real values for each item selection
# (Solves the same problem as mco.R but uses a representation
# that has a single value for each item).
####################################################################
library(mco)
library(scatterplot3d)
# ##################################################################
# Using nsga2 as a real-valued optimiser for a discrete problem
####################################################################
# Since we are doing the bag problem, we will set values 0 - 1 AND
# just assume that a value > 0.5 means an item is picked, while <= 0.5 means it is left off
# the bag list.
#############################################
items <- read.csv('items.csv',header=T)
head(items)
#################################################################
# Since we will be carrying the bag, we need a weight limit.	
weightlimit <- 20
numberitems = nrow(items)

################################################################
# Fitness evaluation functions for the backpack problem
################################################################
#
# MAXIMISE survival points
# In: x - will be a vector of values with limits between
#     0 and 1.  Just need to find which > 0.5 to determine
#     what is in the bag.
# OUT: -Survival points 
# NOTE: pack weight will be a separate function, both for
#       minimising and as a constraint
#
################################################################
survivalPoints <- function(x) 
{
	xpicked = which(x > 0.5)
    pack_survivalpoints <- sum(items$survivalpoints[xpicked])
    return(-pack_survivalpoints)
}
#########################################################
# Minimise packWeight
##########################################################
packWeight <- function(x)
{
	
	xpicked = which(x > 0.5)
    pack_weight <- sum(items$weight[xpicked])
	return(pack_weight)
}

#########################################################
# Maximize packRobustness
##########################################################
packRobust <- function(x)
{
  
  xpicked = which(x > 0.5)
  pack_robust <- sum(items$robust[xpicked])
  return(-pack_robust)
}
###########################################################
# Constraint:  packWeight must be < weightLimit
# All constraints are ok if they are >= 0
# ##########################################################
weightConstraint <- function(x)
{
	return(weightlimit - packWeight(x))
}
minSPW <- function(x)
{
	return(c(survivalPoints(x),packWeight(x),packRobust(x)))
}
# NOTE THAT we have an input equal to the number of items
# since we are using a real value for each item in the bag.
# Note also that the lower/upper bounds are now 0 - 1.
#
for (gen in 1:20)
{
bag <- nsga2(minSPW,idim=numberitems,odim=3,popsize=48,generations=gen,
			lower.bounds = rep(0.0,numberitems), 
			upper.bounds = rep(1.0,numberitems),
			constraints=weightConstraint,cdim=1)
## You can just pass two of the three instead of passing the bag to see it easily.
plot(bag,xlab="SurvivalPoints",ylab="packWeight",
         main="Objective Space")

Sys.sleep(5)
}
##########################################################
# Accessing the elements of the bag
##########################################################
#
# Getting back the solution with the biggest survival value
#
minsurvival <- min(bag$value[,1])
mininds <- which(bag$value[,1]==minsurvival)
baganswer <- bag$par[mininds[1],]
items[which(baganswer > 0.5),]









