healthy = read.csv('3-healthy.csv', header = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = FALSE, col = grey(seq(0, 1, length = 256)))

healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = 'euclidean')
str(healthyVector)
n = 365636
n * (n-1) / 2

numClusters = seq(2, 10, 1)
sumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers = x, iter.max = 1000)$withinss))
plot(numClusters, sumWithinss, type = 'b')

k = 5
set.seed(1)
kmc = kmeans(healthyVector, centers = k, iter.max = 1000)
str(kmc)
healthyClusters = kmc$cluster

kmc$centers[2]

dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col = rainbow(k))

tumor = read.csv('6-tumor.csv', header = FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

install.packages('flexclust')
library(flexclust)
kmc.kcca = as.kcca(kmc, healthyVector)
tumorClusters = predict(kmc.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))
