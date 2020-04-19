rm(list=ls())

attach(mtcars)

cars=mtcars

cars=scale(cars)

cars_kmeans=kmeans(cars,4)

cars_kmeans$cluster

cars_cluster=data.frame(cars,cluster=cars_kmeans$cluster)

plot(cars_cluster)
cars_kmeans$withinss
cars_kmeans$betweenss

wss=0

for (i in 1:15){
  wss[i]=kmeans(cars,centers = i)$withinss
}
wss

plot(wss)

plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 