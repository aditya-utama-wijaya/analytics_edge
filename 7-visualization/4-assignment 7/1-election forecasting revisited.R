library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data('state')
str(statesMap)

# tha variable group defines the different shapes or polygons on the map
# how many different groups are there?
length(table(statesMap$group))
# the variable order defines the order to connect the points within each group

# draw a map of US
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = 'white', color = 'black')

polling = read.csv('1-PollingData.csv')

# do the imputation to fill in the missing values
library(mice)
simple = polling[c('Rasmussen', 'SurveyUSA', 'PropR', 'DiffCount')]
set.seed(144)
imputed = complete(mice(simple))
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# split the data into a training set and a testing set
train = subset(polling, Year >= 2004 | Year <= 2008)
test = subset(polling, Year == 2012)

# create a logistic regression model
mod = glm(Republican ~ SurveyUSA + DiffCount, data = train, family = binomial)

# make predictions on the test set
testPrediction = predict(mod, newdata = test, type = 'response')

# create a vector of Republican/Democrat predictions
testPredictionBinary = as.numeric(testPrediction > .5)

# put the predictions and state labels in a data frame so that we can use ggplot
predictionDataFrame = data.frame(testPrediction, testPredictionBinary, test$State)

# for how many states is our binary prediction 1 (for 2012), corresponding to Republican?
table(testPredictionBinary)

# what is the average predicted probability of our model (on the test set, for 2012)?
mean(testPrediction)

# convert the test.State variable to lowercase, so that it matches the region variable in statesMap
predictionDataFrame$region = tolower(predictionDataFrame$test.State)

# merge the 2 data frames
predictionMap = merge(statesMap, predictionDataFrame, by = 'region')

# make sure the observations are in order so that the map is drawn properly
predictionMap = predictionMap[order(predictionMap$order), ]

# how many observations are there in predictionMap?
nrow(predictionMap)

# how many observations are there in statesMap?
nrow(statesMap)

# when we merge data, it only merged the observations that exist in both data sets
# so since we are merging based on the region variable, we will lose all observations that have a value of 'region' that doesn't exist in both data frames

# color the states according to binary predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + geom_polygon(color = 'black')

# replot the map with discrete outcomes
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + geom_polygon(color = 'black') + scale_fill_gradient(low = 'blue', high = 'red', guide = 'legend', breaks = c(0, 1), labels = c('Democrat', 'Republican'), name = 'Prediction 2012')

# plot the probabilities instead of the binary predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction)) + geom_polygon(color = 'black') + scale_fill_gradient(low = 'blue', high = 'red', name = 'Prediction 2012')

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction)) + geom_polygon(color = 'black', linetype = 3) + scale_fill_gradient(low = 'blue', high = 'red', guide = 'legend', breaks = c(0, 1), labels = c('Democrat', 'Republican'), name = 'Prediction 2012')

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction)) + geom_polygon(color = 'black', size = 3) + scale_fill_gradient(low = 'blue', high = 'red', guide = 'legend', breaks = c(0, 1), labels = c('Democrat', 'Republican'), name = 'Prediction 2012')

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction)) + geom_polygon(color = 'black', alpha = .3) + scale_fill_gradient(low = 'blue', high = 'red', guide = 'legend', breaks = c(0, 1), labels = c('Democrat', 'Republican'), name = 'Prediction 2012')
