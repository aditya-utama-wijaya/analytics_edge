# Load the 'state' dataset into R and convert it to a data frame
data(state)
statedata = data.frame(state.x77)

# Create the linear regression models
regModel = lm(Life.Exp ~ ., data = statedata)

# What is the adjusted R-squared of the model?
summary(regModel)

# Calculate the sum of squared errors (SSE) between the predicted life expectancies using this model and the actual life expectancies
predictions = predict(regModel)
sum((statedata$Life.Exp - predictions) ^ 2)

# Build a second linear regression model using just Population, Murder, Frost, and HS.Grad as independent variables
regModel2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)

# What is the adjusted R-squared for this model?
summary(regModel2)

# Calculate the sum of squared errors again using this reduced model
predictions2 = predict(regModel2)
sum((statedata$Life.Exp - predictions2) ^ 2)
# Trying different combinations of variables in linear regression is like trying different numbers of splits in a tree - this controls the complexity of the model

# Build a CART model to predict Life.Exp using all of the other variables as independent variables
library(rpart)
library(rpart.plot)
CARTmodel = rpart(Life.Exp ~ ., data = statedata)

# Plot the tree
# Which variables appear in the tree?
prp(CARTmodel)

# Use the regression tree to predict life expectancies
predictionsCART = predict(CARTmodel)

# Calculate the sum of squared errors
sum((statedata$Life.Exp - predictionsCART) ^ 2)
# The error is higher than for the linear regression models
# One reason might be that we haven't made the tree big enough

# Set the minbucket parameter to 5
cartModel2 = rpart(Life.Exp ~ ., data = statedata, minbucket = 5)

# Which variables appear in this new tree?
prp(cartModel2)
# Since the tree now has more splits, it must be true that the default minbucket parameter was limiting the tree from splitting more before
# So the default minbucket parameter must be larger than 5

# What is the SSE of this tree?
predictionsCart2 = predict(cartModel2)
sum((statedata$Life.Exp - predictionsCart2) ^ 2)

# Create a tree that predicts Life.Exp using only Area, with the minbucket parameter to 1
cartModel3 = rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)

# What is the SSE of this tree?
predictionsCart3 = predict(cartModel3)
sum((statedata$Life.Exp - predictionsCart3) ^ 2)
# By making the minbucket parameter very small, we could build an almost perfect model using just 1 variable, that is not even the most significant variable
# However, this is not a very interpretable model, and will not generalize well

# Tune the regression tree to see if we can improve the fit of our tree while keeping it as simple as possible
# What value of cp does the train function recommend?
library(caret)
set.seed(111)
fitControl = trainControl(method = 'cv', number = 10)
cartGrid = expand.grid(.cp = seq(.01, .5, .01))
train(Life.Exp ~ ., data = statedata, method = 'rpart', trControl = fitControl, tuneGrid = cartGrid)

# Create a tree with the value of cp
cartModel4 = rpart(Life.Exp ~ ., data = statedata, cp = .12)

# Plot the tree
prp(cartModel4)

# Calculate the SSE of this tree
predictionsCart4 = predict(cartModel4)
sum((statedata$Life.Exp - predictionsCart4) ^ 2)
# The purpose of cross-validation is to pick the tree that will perform the best on a test set
# So we would expect the model we made with the best cp to perform best on a test set

# Use 'train' with the same parameters as before but just using 'Area' as an independent variable to find the best cp value
set.seed(111)
train(Life.Exp ~ Area, data = statedata, method = 'rpart', trControl = fitControl, tuneGrid = cartGrid)

# Build a tree using just Area and this value of cp
cartModel5 = rpart(Life.Exp ~ Area, data = statedata, cp = .02)

# How many splits does the tree have?
prp(cartModel5)

# Calculate the SSE of the cross-validated 'Area tree'
predictionsCart5 = predict(cartModel5)
sum((statedata$Life.Exp - predictionsCart5) ^ 2)
# The Area variable is not as useful as Murder rate - if it was, it would have been in the cross-validated tree
# Cross-validation is not designed to improve the fit on the training data, but it won't necessarily make it worse either
# Cross-validation cannot guarantee improving the SSE on unseen data, although it often helps
