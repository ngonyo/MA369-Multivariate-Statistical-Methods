# Illustration of K-means clustering 
# We will use the kmeans function and simulated data.
  
# Simulate data on 2 features with two true clusters
set.seed(2)
x <- matrix(rnorm(50*2), byrow = T, ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3 #shifting origin to 3
x[1:25, 2] <- x[1:25, 2] - 4 #shifting origin to -4

# Note: the first and the last 25 observartions have different means
# In particular: 
# First 25: N(3, 1) for X1 and N(-4, 1) for X2
# Last 25: N(0, 1) for both X1 and X2
  
# Plot the data
plot(x, col = ifelse(1:50 <= 25, "blue", "orange"),
     xlim = range(x[, 1]), ylim = range(x[, 2]))

# K-means with K = 2 centroid
?kmeans
km.out <- kmeans(x, centers = 2, nstart = 20)
## nstart should be a large number (number of sets) to avoid
# unexpected local maxima

# Look at what is stored in result
km.out
names(km.out)

# The cluster assignments
km.out$cluster
table(km.out$cluster)
sum(km.out$cluster[1:25])

# Note: K-means perfectly separates the observations into two clusters
  
# Plot the results
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=2", 
     xlab = "", ylab = "", pch = 20, cex = 2)

# Look at the various SS
km.out$withinss # Within Cluster SS
km.out$tot.withinss # Total Cluster SS = sum(Within Cluster SS)
km.out$totss # Total Cluster SS
km.out$betweenss # Between Cluster SS
sum(diag(var(x))*49) # Total SS (= (n-1)S^2)

# K-means with K = 3 centroid
set.seed(4)
km.out = kmeans(x, centers = 3, nstart = 20)
km.out
sum(km.out$cluster[1:25])

# Plot the results
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=3",
     xlab = "", ylab = "", pch = 20, cex = 2)

# Note: With K = 3, the second cluster is split into 2 clusters.
# Which K to choose?
###################################################################