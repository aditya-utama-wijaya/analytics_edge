airlines = read.csv('2-AirlinesCluster.csv')

# which variable has (on average) the smallest value?
# which variable has (on average) the largest value?
summary(airlines)

# we will normalize our data before we run the clustering algorithms
# if we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale
# the variables that are on a larger scale will contribute much more to the distance calculation, and thus will dominate the clustering

# normalize the data
library('caret')
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

# in the normalized data, which variable has the largest maximum value?
# in the normalized data, which variable has the smallest minimum value?
summary(airlinesNorm)

# compute the distances between data points on the normalized data
distances = dist(airlinesNorm, method = 'euclidean')

# run the hierarchical clustering algorithm on the normalized data
hierClust = hclust(distances, method = 'ward.D')

# plot the dendrogram of the hierarchical clustering process
plot(hierClust)
# there is a long time that the line crosses 2 clusters, 3 clusters, or 7 clusters

# suppose the airline decides to proceed with 5 clusters
# divide the data points into 5 clusters
clusterGroups = cutree(hierClust, k = 5)

# how many data points are in cluster 1?
table(clusterGroups)

# compare the aerage values in each of the variables for the 5 clusters (the centroid of the clusters)
# compute the average values of the unnormalized data so that it is easier to interpret
spl = split(airlines[1:7], clusterGroups)
lapply(spl, colMeans)

# how would you describe the customers in cluster 1?
# => infrequent but loyal customers
# how would you describe the customers in cluster 2?
# => customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions
# how would you describe the customers in cluster 3?
# => customers who have accumulated a large amount of miles, mostly through non-flight transactions
# how would you describe the customers in cluster 4?
# => cluster 4 customers have the smallest value in DaysSinceEnroll, but they are already accumulating a reasonable number of miles
# => relatively new customers who seem to be accumulating miles, mostly through non-flight transactions
# how would you describe the customers in cluster 5?
# => cluster 5 customers hae lower than average values in all variables
# => relatively new customers who don't use the airline very often

# run the k-means clustering algorithm on the normalized data
set.seed(88)
kmeansClust = kmeans(airlinesNorm, centers = 5, iter.max = 1000)

# how many clusters have more than 1000 observations?
table(kmeansClust$cluster)

# compare the cluster centroids to each other
kmeansClust$centers

# cluster ordering is not meaningful in either k-means clustering or hierarchical clustering
# so while there may be a cluster produced by the k-means algorithm that is similar to cluster 1 produced by the hierarchical method, it will not necessarily be shown first
