###########################################
# ASSN 3
# invest.R
###########################################
#
# Build a multi-objective constrained model
# for an investment portfolio
###########################################
#install.packages("mco")
library(mco)

# The 30 investments that will be used to select
# a portfolio are in "invest.tab"
#
invest <- read.table("invest.tab")
#View(invest)
# Determine number of options from the invest table
#
numberOptions <- nrow(invest)
print(numberOptions)
#
# The format of invest is:
# Each row is an option
# Columns are : ROI (return on investment), Risk, Type
# 			    where Type = 1 (Stock), 2 (bond), 3 (cash)
#
##########################################
# CONSTRAINTS
##########################################
# The sum of the portfolios must be between
# 0.95 and 1.0
# Constraints are satisfied by being >= 0
#############################################
portfolioSUM <- function(x)
{
	selected <- which(x >= minAMOUNT)
	sumx = sum(x[selected])
	if ((sumx >=0.95) && (sumx <=1.0)) return(1) # ok
	return(-1) # Fails constraint
}
#############################################################
# Each option must either not be selected
# OR must be between an amount
#   minAMOUNT <= amount <= maxAMOUNT
#
# We will assume that anything < minAMOUNT
# is equivalent to zero (and therefore not included)
#############################################################
minAMOUNT = 0.05
maxAMOUNT = 0.2
#############################################################
# Set the lower and upper bounds for
# each investment option
#############################################################

# The lower bound is 0.
lower = rep(0,numberOptions)

# The upper bound is the maximum amount of an
# option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################################
# Constraint - each selected option must be  >= minAMOUNT
#              and <= maxAMOUNT
# We ignore options with value < minAMOUNT because they
# aren't part of the final selection of investments.
###########################################################################
portfolioRANGE <- function(x)
{
	selected <- which(x >= minAMOUNT)  # Only count if at least minimum
	over <- which(x[selected] > maxAMOUNT)
	if (length(over) > 0) return(-1)
	return(1)
}

##################################################################
# Constraints on the minimum and maximum number of selected
# stocks/bonds/cash
##################################################################
minNumber = 8
maxNumber = 12

portfolioNUM <- function(x)
{
	numselected <- length(which(x >= minAMOUNT))
	if (numselected < minNumber) return(-1)
	if (numselected > maxNumber) return(-1)
	return(numselected)
}

############################################################
# Functions to be minimised/maximised
############################################################
###############################
# Return on Investion (ROI)
###############################
# This wants to be MAXIMISED
# Only include options that are greater than the minAMOUNT
#############################################################
ROI <- function(x)
{
	selected <- which(x >= minAMOUNT)
	roi <- sum(invest$ROI[selected]*x[selected])
	return(-roi) # Since nsgaII minimises we take the negative
}
#################################
# Risk (RISK)
#################################
# This is to be MINIMISED
# Only include options that are greater than the minAMOUNT
##############################################################
RISK <- function(x)
{
	selected <- which(x >= minAMOUNT)
	risk <- sum(invest$Risk[selected]*x[selected])  # Just the sum of risk
	return(risk) # we want to minimise the risk
}

#################################
# CORRELATION
#################################
# This is to be minimized.(means if some investemnts in the portfolio are increasing other should be decreasing)
# This reduces the risk of the portfolio as a whole losing money and results in more stable returns.
# Only include options that are greater than the minAMOUNT
##############################################################
CORRELATION <- function(x)
{
  selected <- which(x >= minAMOUNT)
  removeDuplicates <- duplicated(selected)
  corr <- sum(invest$Risk[selected]*x[selected])  # Just the sum of risk
  return(corr) # we want to minimise the risk
}
##################################################
# Here are the functions that are to be minimised
# Note ROI is actually maximised, while RISK is
# minimised.
###################################################
funs <- function(x)
{
	return(c(ROI(x),RISK(x)))
  #return(c(CORRELATION(x)))
}
######################################################
# Here are the constraints
# Since nsga2 assumes a single constraint function, we
# call each constraint in turn, and return the results
# of all the constraints as a concatenated list
######################################################
constraintFNS <- function(x)
{
	psum = portfolioSUM(x)
	prange = portfolioRANGE(x)
	pnum = portfolioNUM(x)
	return(c(prange,pnum,psum))
}
# Set the lower and upper bounds for each investment option
# The lower bound is 0.
lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################
# CALL nsga2 to find the pareto optimal solutions
###########################################################
portfolio <- nsga2(funs,
                   idim=numberOptions, #inputs for each option,
                   odim=2, #2 outputs (ROI,RISK)
                   popsize=52,generations=500,
				           lower.bounds=lower,
				           upper.bounds=upper,
					         constraints = constraintFNS,
					         cdim=3) # 3 constraints

print(portfolio$value)
View(portfolio$value)
######## Plot the pareto front using default plotting
###########################################################
plot(portfolio,xlab="-ROI (%)",ylab="RISK",main="Objective Space")
#hist(portfolio)
View(portfolio$par) # for each portfolio you look at investemnts which are greater than minAmount
## 2: That is where you should plot the histogram
## showing the number of investments for each solution on the pareto front
## for the histogram have an empty array and start an index 1
## Then do a nested loop through portfolio for finding all the values that are greater than minimum amount
## and add them to the array and then plot it using breaks


## 3 :Examine and present the blend of stocks, bonds and cash for a low risk,
## moderate risk and high risk investment blend(just pick one from each general category).
# the first column here is the roi and the 2nd is the risk
# use the min function to find the minimum risk and then compare in with that number in the invest
lowRisk <- min(portfolio$value[,2]) # low risk
lowRiskIndex <- which(portfolio$value[,2] == lowRisk, arr.ind=TRUE) ## to see where the lowRisk value is located
print(lowRiskIndex)
toReplaceLowRisk<-portfolio$par[lowRiskIndex,]
print(portfolio$value[1,2])


highRisk <- max(portfolio$value[,2]) # high risk
highRiskIndex<-which(portfolio$value[,2] == highRisk, arr.ind=TRUE) ## to see where the highRisk value is located
print(highRiskIndex)
toReplaceHighRisk<-portfolio$par[highRiskIndex,]
print(toReplaceHighRisk)
print(portfolio$value[2,2])

moderateRisk <- median(portfolio$value[,2]) # moderate risk
print(moderateRisk)
#print(which(portfolio$value[,2] == abs(moderateRisk), arr.ind=TRUE)) ## wont's see this as the value is just close to the moderate value.
moderateRiskIndex<-which(abs(portfolio$value[,2] - moderateRisk) == min(abs(portfolio$value[,2] - moderateRisk)))
print(moderateRiskIndex)
moderateRiskIndex<-portfolio$par[39,]
print(moderateRiskIndex)
print(portfolio$value[44,2])
toReplaceModerateRisk<-portfolio$par[44,]
print(toReplaceModerateRisk)

## replace all with less than 0.05 with 0, as they don't count
replace1 <- replace(toReplaceLowRisk,toReplaceLowRisk<minAMOUNT,0)
print(replace1)# we can see we got quite a few values for cash which has low risk.
replace2<- replace(toReplaceHighRisk,toReplaceHighRisk<minAMOUNT,0)
print(replace2)# We can see that there are quite few values for stocks as the risk and roi is high for them.
replace3<- replace(moderateRiskIndex,moderateRiskIndex<minAMOUNT,0)
print(replace3)# it makes sence as we have some cash,some bond and some stocks bcz its moderate.


## 4:Produce the plot given in the assignment