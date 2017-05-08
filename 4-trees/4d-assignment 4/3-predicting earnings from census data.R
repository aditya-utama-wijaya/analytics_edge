census = read.csv('3-census.csv')

# Split the data randomly into a training set and a testing set
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, .6)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)

# Build a logistic regression model to predict whether an individual's earnings are above $50,000
censusglm = glm(over50k ~ ., data = train, family = binomial)

# Which variables are significant, or have factors that are significant?
summary(censusglm)

# What is the accuracy of the model on the testing set?
predictTest = predict(censusglm, newdata = test, type = 'response')
table(test$over50k, predictTest >= .5)
(9051 + 1888) / (9051 + 662 + 1190 + 1888)

# What is the baseline accuracy for the testing set?
table(train$over50k)
table(test$over50k)
9713 / nrow(test)

# What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, 'auc')@y.values)

# Build a classification tree to predict 'over50k'
library(rpart)
library(rpart.plot)
censustree = rpart(over50k ~ ., data = train, method = 'class')

# How many splits does the tree have in total?
prp(censustree)

# What is the accuracy of the model on the testing set?
predictTest = predict(censustree, newdata = test, type = 'class')
table(test$over50k, predictTest)
(9243 + 1596) / (9243 + 470 + 1482 + 1596)

# Build the ROC curve and compute the AUC for the CART model on the test set
predictTest = predict(censustree, newdata = test)
ROCRpred = prediction(predictTest[, 2], test$over50k)
perf = performance(ROCRpred, 'tpr', 'fpr')
plot(perf)
as.numeric(performance(ROCRpred, 'auc')@y.values)

# Observe that compared to the logistic regression ROC curve
perfglm = performance(ROCRpred, 'tpr', 'fpr')
plot(perfglm)
# The CART ROC curve is less smooth than the logistic regression ROC curve
# The probabilities from the CART model take only a handful of values (5, 1 for each end bucket/leaf of the tree); the changes in the ROC curve correspond to setting the threshold to 1 of those values
# The breakpoints of the curve correspond to the false and true positive rates when the threshold is set to the 5 possible probability values

# Before building a random forest model, down-sample the training set
# Some computers might run out of memory when trying to train the model since random forests is much more computationally intensive than CART or logistic regression
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

# Build a random forest model to predict 'over50k'
library(randomForest)
set.seed(1)
censusrf = randomForest(over50k ~ ., data = trainSmall)

# What is the accuracy of the model on the test set?
predictTest = predict(censusrf, newdata = test)
table(test$over50k, predictTest)
(9637 + 859) / (9637 + 76 + 2219 + 859)

# One way to measure the importance of a variable is to look at the number of times, aggregated over all of the trees in the random forest model, that a certain variable is selected for a split
# Build the metric
vu = varUsed(censusrf, count = TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

# Which of the variables is the most important in terms of the number of splits?
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

# Impurity measures how homogenous each bucket or leaf of the tree is
# In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased
# Therefore, another way to measure the importance of a variable is to average the reduction in impurity, taken over all the times that variable is selected for splitting in all of the trees in the forest
# Compute this metric
varImpPlot(censusrf)

# Select the cp parameter for our CART model using k-fold cross validation, with k = 10 folds
library(caret)
set.seed(2)
fitControl = trainControl(method = 'cv', number = 10)
cartGrid = expand.grid(.cp = seq(.002, .1, .002))
train(over50k ~ ., data = train, method = 'rpart', trControl = fitControl, tuneGrid = cartGrid)

# Fit a CART model to the training data using this value of cp
model = rpart(over50k ~ ., data = train, method = 'class', cp = .002)

# What is the prediction accuracy on the test set?
predictTest = predict(model, newdata = test, type = 'class')
table(test$over50k, predictTest)
(9178 + 1838) / (9178 + 535 + 1240 + 1838)

# Plot the CART tree for this model
# How many splits are there?
prp(model)
# In some applications, an improvement in accuracy would be worth the loss in interpretability
# In others, we may prefer a less accurate model that is simpler to understand and describe over a more accurate - but more complicated - model

