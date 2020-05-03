
#Kmeans using attitude dataset

library(datasets)

str(attitude)

?attitude

dim(attitude)

View(attitude)

summary(attitude)

# Subset the attitude data
dat = attitude[,c(3,4)]
dat

# Plot subset data
plot(dat, main = "% of favourable responses to
     Learning and Privilege", pch =20, cex =1.3)


# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 2, nstart=100)
km1


km1$cluster
km1$withinss


# Plot results
plot(dat, col =(km1$cluster+3) , main="K-Means result with 2 clusters", pch=20, cex=2)


# Check for the optimal number of clusters given the data

mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)


plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)



# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

attributes(km2)

# Plot results
plot(dat, col =(km2$cluster+3) , main="K-Means result with 6 clusters", pch=20, cex=2)





