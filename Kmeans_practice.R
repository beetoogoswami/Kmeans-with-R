rm(list = ls())

data=read.csv("Cereals.csv", sep=",", header = TRUE)
data=data[,-c(1)]

data1=scale(data)

sum(is.na(data1))
data1=knnImputation(data1,k=10)


data_kmeans=kmeans(data1,6)
data_kmeans$cluster
data_kmeans$withinss  # within the cluster
data_kmeans$tot.withinss
sum(data_kmeans$tot.withinss) # 
data_kmeans$betweenss # between the clusters

data_kmeans$cluster
summary(data_kmeans)

# calculate the mean

aggregate(data1,by=list(data_kmeans$cluster), FUN=mean)


data_cluteres=data.frame(data,cluster=data_kmeans$cluster )
data1=data.frame(data1,cluster=data_kmeans$cluster )

wss=0

for (i in 1:15){
  wss[i]=kmeans(data1,centers = i)$withinss
}
wss

plot(wss)

plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


test_datapoint <- data1[sample(1:nrow(data1),1),]
closest.cluster <- function(x) {
  cluster.dist <- apply(data_kmeans$centers, 1, function(y) sqrt(sum((x-y)^2)))
  print(cluster.dist)
  return(which.min(cluster.dist)[1])
}



# Predicting which cluster the new data point belongs to based on the distance.
closest.cluster(test_datapoint)

# Checking the cluster stability
# Building the clusters on all data
fit1 <- kmeans(data1, 10)
fit1
# Getting the cluster numbers
x <- fit1$cluster
# Building the clusters on 90 % of data
fit2 <- kmeans(data1[1:74,], 10)
# Getting the cluster numbers
y <- fit2$cluster
unique(y)
# Loading the required libraries
install.packages('dtwclust')
library(dtwclust)
library(flexclust)
# Checking whether the same data points are falling into same cluster 
# when we cluster on all the data or 90% of data.
randIndex(x[1:74], y)
install.packages("mclust")
library(mclust)
# Checking whether the same data points are falling into same cluster 
# when we cluster on all the data or 90% of data.
adjustedRandIndex(x[1:74], y)

