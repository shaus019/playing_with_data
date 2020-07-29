## DEGREE DISTRIBUTION ##

data <- read.csv("network.csv",header=TRUE, sep=",")
head(data)

#2: What is the maximum and minimum degree for this network?
max(data$k)


# Tha maximum degree is 183.
min(data$k)
# The minimum degree is 5.
# 3: Produce a single figure showing two plots: the histogram of the degree, 
# and a line plot sorted by degree. Comment on what this tells you about the network.
par(mfrow = c(1, 2))
hist(data$k)
plot(sort(x = data$k), y = NULL, type = "l")
#plot(data)
#hist(data$k, x= Null ,sort(data$k))


# 3: Calculate P(k) the probability of observing a node with degree k.
# identify the total number of outcomes that can accur
totalFrequency <- table(data$k) # Count the frequency of nodes k
totalOfFrequency <- sum(totalFrequency) # that is the the total number of outcomes
probability <- totalFrequency/totalOfFrequency # probability by dividing the frequency by possible outcomes
print(probability)
uniqueValues <- unique(sort(data$k)) # unique values sorted by degree
print(uniqueValues)
par(mfrow = c(1, 1))
plot(x = uniqueValues, y = probability, log = "xy") # plot for the probability using log scale for x and y.

## 5: why the network is likely to be scale free?
## Because nodes with higher probabability are more likely to see new edges,
# compared to the one with lower probability.
# It means that when the network grows the underlining structure or properties stays the same.
