stocks = read.csv('3-StocksCluster.csv')
str(stocks)

# what proportion of the observations have positive returns in December?
mean(stocks$PositiveDec)

# what is the maximum correlation between any 2 return variables in the dataset?
cor(stocks)

# which month has the largest mean return across all observations in the dataset?
summary(stocks)

# split the data into a training set and testing set, putting 70% of the data in the training set
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, .7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

# use the training data frame to train a logistic regression model to predict PositiveDec using all other variables as independent variables
stocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)

# what is the overall accuracy on the training set using a threshold of .5?
predictTrain = predict(stocksModel, type = 'response')
table(stocksTrain$PositiveDec, predictTrain > .5)
(990 + 3640) / (990 + 2689 + 787 + 3640)
# 0.5711818

# what is the overall accuracy of the model on the test?
predictTest = predict(stocksModel, newdata = stocksTest, type = 'response')
table(stocksTest$PositiveDec, predictTest > .5)
(417 + 1553) / (417 + 1160 + 344 + 1553)
# 0.5670697

# what is the accuracy on the test set of a baseline model?
table(stocksTest$PositiveDec)
1897 / (1897 + 1577)
# 0.5460564

# the first step of clustering the stocks is to remove the dependent variable
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
# in cluster-then-predict method, the final goal is to predict the dependent variable, which is unknown at the time of prediction
# therefore, if we need to know the outcome value to perform the clustering, the methodology is no longer useful for prediction of an unknown outcome value

# preProcess command from the caret package normalizes variables by subtracting by the mean and dividing by the standard deviation
# in cases where we have a training and testing set, we'll want to normalize by the mean and standard deviation of the variables in the training set
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

# what is the mean of the ReturnJan variable in normTrain?
mean(normTrain$ReturnJan)

# what is the mean of the ReturnJan variable in normTest?
mean(normTest$ReturnJan)

# we see that the average return in January is higher in training set than in the testing set
# since normTest was constructed by subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan is negative in normTest

# run k-means clustering with 3 clusters on normTrain
set.seed(144)
km = kmeans(normTrain, centers = 3)

# which cluster has the largest number of observations?
table(km$cluster)

# use the flexclust package to obtain training set and testing set cluster assignments for our observations
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)

# how many test-set observations were assigned to cluster 2?
table(clusterTest)

# build data frames containing the elements in the stocksTrain data frame assigned to clusters 1, 2, and 3
stocksTrains = split(stocksTrain, km$cluster)

# which training set data frame has the highest average value of the dependent variable?
mean(stocksTrains[[1]]$PositiveDec)
mean(stocksTrains[[2]]$PositiveDec)
mean(stocksTrains[[3]]$PositiveDec)

# similarly build data frames from the stockTest data frame
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

# build logistic regression models which predict PositiveDec using all the other variables as independent variables
stocksModel1 = glm(PositiveDec ~ ., data = stocksTrains[[1]], family = binomial)
summary(stocksModel1)
stocksModel2 = glm(PositiveDec ~ ., data = stocksTrains[[2]], family = binomial)
summary(stocksModel2)
stocksModel3 = glm(PositiveDec ~ ., data = stocksTrains[[3]], family = binomial)
summary(stocksModel3)

# make test-set predictions
predictTest1 = predict(stocksModel1, newdata = stocksTest1, type = 'response')
predictTest2 = predict(stocksModel2, newdata = stocksTest2, type = 'response')
predictTest3 = predict(stocksModel3, newdata = stocksTest3, type = 'response')

# what is the overall accuracy of stocksModel1 on the test set, using a threshold of .5?
table(stocksTest1$PositiveDec, predictTest1 > .5)
(30 + 774) / (30 + 471 + 23 + 774)
0.6194145

# what is the overall accuracy of stocksModel2 on the test set?
table(stocksTest2$PositiveDec, predictTest2 > .5)
0.5504808

# what is the overall accuracy of stocksModel3 on the test set?
table(stocksTest3$PositiveDec, predictTest3 > .5)
(49 + 13) / (49 + 13 + 21 + 13)
0.6458333

# compute the overall test-set accuracy of the cluster-then-predict approach
allPredictions = c(predictTest1, predictTest2, predictTest3)
allOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

# what is the overall test-set accuracy of the cluster-then-predict approach using a threshold of .5?
table(allOutcomes, allPredictions > .5)
(467 + 1544) / (467 + 1110 + 353 + 1544)
0.5788716
