## DATA QUALITY STRUCTURE ##
##############################################################################################
# Example to produce plot of:
# Relative distance in Feature Space (x) versus Relative Distance in Response space (y)
##############################################################################################
#
# Assume we are given a table with a single response variable
# Want to make a table with the (x) and (y) values as 2 columns
# 
# IN: d - the dataframe or matrix 
#     response.var - the number of the column used as the response variable.  Defaults to last column 
# OP: Calculates the normalised distance between each pair of data items in explanatory space
#     and the distance between their response variables. 
# OUT: Data frame with 2 columns, the distance in feature space (x), and distance in response space (y)
########################################################################################################
dist.table <- function(d, response.var = ncol(d))  
{
    d <- scale(d) # scale data
	d.dist <- dist(d[,-response.var])  # distance all X values	
	d.resp <- dist(d[,response.var])
	
	d.dist <- (d.dist-min(d.dist))/(max(d.dist)-min(d.dist))
	d.resp <- (d.resp-min(d.resp))/(max(d.resp)-min(d.resp))
	
	data.frame(cbind(d.dist,d.resp))
}
#
# Example with simple linear response, random X1 and X2 but no noise for response
#
X1 <- runif(100)
X2 <- runif(100)
Y <- X1 + X2
#
ex1 <- data.frame(cbind(X1,X2,Y))
d <- dist.table(ex1, response.var = 3)
plot(x=d$d.dist, y = d$d.resp,xlab="Normalised Distance in Feature Space",
                              ylab="Normalised Distance in Response Space",cex=0.5)

##2: Dataset with no relationship between the explanatory and response ##

X1 <- runif(100)
X2 <- runif(100)
Y1 <- runif(100)
 
ex1 <- data.frame(cbind(X1,X2,Y1))
d <- dist.table(ex1, response.var = 3)
plot(x=d$d.dist, y = d$d.resp,xlab="Normalised Distance in Feature Space",
     ylab="Normalised Distance in Response Space",cex=0.5)
## 3: Produce figure with two plots using Boston housing dataset and ##
library("MASS")
data("Boston", package = "MASS")
dataBoston<-Boston
medvCol <- which(colnames(dataBoston)=="medv")
print(medvCol)
db <- dist.table(dataBoston,response.var = medvCol)
plot(x=db$d.dist, y = db$d.resp,xlab="Normalised Distance in Feature Space",
     ylab="Normalised Distance in Response Space",cex=0.5)
