letters = read.csv('2-letters_ABPR.csv')

# Create a new variable in the dataframe which takes the value 'TRUE' if the observation corresponds to the letter B, and 'FALSE' if it does not
letters$isB = as.factor(letters$letter == 'B')

# Split the data into a training and testing set, putting 50% of the data in the training set
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = .5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

# What is the accuracy of the baseline method on the test set?
table(train$isB)
table(test$isB)
1175 / nrow(test)

# Build a classification tree to predict whether a letter is a B or not
# Remove the variable 'letter' out of the model, as this is related to what we are trying to predict
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data = train, method = 'class')

# What is the accuracy of the CART model on the test set?
prediction = predict(CARTb, newdata = test, type = 'class')
table(test$isB, prediction)
(1118 + 340) / (1118 + 57 + 43 + 340)

# Build a random forest model to predict whether the letter is a B or not
library(randomForest)
set.seed(1000)
RFb = randomForest(isB ~ . - letter, data = train)

# What is the accuracy of the model on the test set?
predictions = predict(RFb, newdata = test)
table(test$isB, predictions)
(1165 + 374) / (1165 + 10 + 9 + 374)

# The variable in the data frame which we will be trying to predict is 'letter'
# Convert letter in the original data set to a factor
letters$letter = as.factor(letters$letter)

# Generate new training and testing sets of the letters data frame using letters$letter as the first input to the sample.split function
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = .5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)

# What is the baseline accuracy on the testing set?
table(train2$letter)
402 / (394 + 383 + 402 + 379)

# Build a classification tree to predict 'letter'
# isB is related to what we are trying to predict
CARTletter = rpart(letter ~ . - isB, data = train2, method = 'class')

# What is the test set accuracy of the CART model?
predictLetter = predict(CARTletter, newdata = test2, type = 'class')
table(test2$letter, predictLetter)
(348 + 318 + 363 + 340) / nrow(test2)

# Build a random forest model on the training data
set.seed(1000)
RFletter = randomForest(letter ~ . - isB, data = train2)

# What is the test set accuracy of the random forest model?
predictLetter = predict(RFletter, newdata = test2)
table(test2$letter, predictLetter)
(390 + 379 + 393 + 367) / nrow(test2)
