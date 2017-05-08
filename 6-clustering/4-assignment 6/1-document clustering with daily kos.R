dailykos = read.csv('1-dailykos.csv')

# compute the distances
kosDist = dist(dailykos, method = 'euclidean')

# use hclust to build the model
kosHierClust = hclust(kosDist, method = 'ward.D')

# what seem like to be good choices for the number of clusters?
plot(kosHierClust)
# 2 or 3

# let's pick 7 clusters, this number is reasonable according to the dendrogram, and also seems reasonable for the application
# split the data into 7 clusters
hierGroups = cutree(kosHierClust, k = 7)

# create 7 new datasets, each containing the observations from one of the clusters
hierCluster = split(dailykos, hierGroups)

# how many observations are in cluster 3?
nrow(hierCluster[[3]])

# which cluster has the most observations?
# which cluster has the fewest observations?
table(hierGroups)

# what is the most frequent word in cluster 1, in terms of average value?
tail(sort(colMeans(hierCluster[[1]])))
# this computers the mean frequency values of each of the words, then outputs the 6 words that occur the most frequently
# the colMeans function computes the column (word) means
# the sort function orders the words in increasing order of the mean values
# the tail function outputs the last 6 words listed, which are the ones with the largest column means

# which words best describe cluster 2?
tail(sort(colMeans(hierCluster[[2]])))

# run k-means clustering
set.seed(1000)
kmeansCluster = kmeans(dailykos, centers = 7)

# subset the data into 7 clusters (7 new datasets)
k = split(dailykos, kmeansCluster$cluster)

# how many obserations in cluster 3?
nrow(k[[3]])

# which cluster has the most observations?
# which cluster has the fewest number of observations?
table(kmeansCluster$cluster)

# output the 6 most frequent words in each cluster
tail(sort(colMeans(k[[1]])))
tail(sort(colMeans(k[[2]])))
tail(sort(colMeans(k[[3]])))
tail(sort(colMeans(k[[4]])))
tail(sort(colMeans(k[[5]])))
tail(sort(colMeans(k[[6]])))
tail(sort(colMeans(k[[7]])))

# compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering
table(hierGroups, kmeansCluster$cluster)

# which hierarchical cluster best corresponds to k-means cluster 2?
# 116 (80.6%) of the observations in k-means cluster 2 also fall in hierarchical cluster 7

# which hierarchical cluster best corresponds to k-means cluster 3?
# 171 (61.7%) of the observations in k-means cluster 3 also fall in hierarchical cluster 5

# which hierarchical cluster best corresponds to k-means cluster 7?
# no more than 123 (39.9%) of the observations in k-means cluster 7 fall in any hierarchical cluster

# which hierarchical cluster best corresponds to k-means cluster 6?
# 320 (97.3%) of observations in k-means cluster 6 fall in hierarchical cluster 2