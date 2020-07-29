## VISUALIZATION AND VISUALIZATION
# Tell R about the packages you are using.
library("lattice")
library("tsne")
data = read.csv("countrystats.csv", header=T, sep=",") #Read the data from the file.
head(data)
#change the row name to be the country name
row.names(data)<-data[,1]
row.names(data)
#Delete the country name coloumn, which is the firsr coulumn.
data <- data[,-1]
head(data)
rowNames <-row.names(data)
# 1.Breifly state the meaning of each variable for the data.
# PopDensity = is the measurement of population. In other words how dense an area is term of it population. 
# IncomeperCapital = That is the average income earned per person in a given area.
# Purchasing parity = is the measurement of prices in differnt countries.
# changeGDP = is to measure or make an assessment on how the economy of a country is doing.

# 2. Which country look similar to New Zealand? Which country is the strongges or the weekest?

# Ans : NewZealand is the same as icelenad Nethereland etc.
#levelplot(as.matrix(data), scales=list(x=list(rot=90)))


# 3. Scale the data and create a distance matrix
dataDist <-dist(scale(data))
# Hierarchical clustered dendrogram with average algorithmative method.
dataDend <-hclust(dataDist,method="average")
plot(dataDend)

# Hierarchical clustered dendrogram with complete algorithmative method.
dataCom <-hclust(dataDist,method="complete")
plot(dataCom)
# Cut the dendrogram into 50 clusters
clusters <- cutree(dataCom, k = 50)
print(clusters)


## Using dimentionaly reduction method t-SNE to create a 2 dimentional plot of the data.
tsnee <- tsne(data, initial_config = NULL, k = 2,
     max_iter = 50, min_cost = 0, epoch_callback = NULL, whiten = TRUE,
     epoch=100)
plot(tsnee)
text(x = tsnee[,1],y = tsnee[,2],labels=row.names(data))
plot(tsnee,xlim = c(-6,0),ylim =c(-5,2))

#text(x = tsnee[,1],y = tsnee[,2],labels=row.names(data))
country <-which(row.names(data)=="NewZealand")
print(country)
