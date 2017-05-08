gerber = read.csv('1-gerber.csv')

# What proportion of people in this dataset voted in this election?
table(gerber$voting)
108696/nrow(gerber)

# Which of the 4 treatment groups had the largest percentage of people who actually voted?
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# Build a logistic regression model
logModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)

# Which of the coefficients are significant in the logistic regression model?
summary(logModel)

# Using a threshold of .3, what is the accuracy of the logistic regression model?
predictLog = predict(logModel, type = 'response')
table(gerber$voting, predictLog > .3)
(134513+51966)/nrow(gerber)

# Using a threshold of .5, what is the accuracy of the logistic regression model?
table(gerber$voting, predictLog > .5)
235388/nrow(gerber)

# Compute the AUC of the model
library(ROCR)
ROCRpred = prediction(predictLog, gerber$voting)
as.numeric(performance(ROCRpred, 'auc')@y.values)

# Build a CART tree for voting using all data and the same 4 treatment variables we used before
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
prp(CARTmodel)
# There are no splits in the tree, because none of the variables make a big enough effect to be split on

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = .0)
prp(CARTmodel2)

# Make a new tree that includes the 'sex' variable
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = gerber, cp = .0)
prp(CARTmodel3)

# Create a regression tree using just the 'control' variable
CARTcontrol = rpart(voting ~ control, data = gerber, cp = .0)

# Create another tree with the 'control' and 'sex' variables
CARTsex = rpart(voting ~ control + sex, data = gerber, cp = .0)

# What is the absolute value of the difference in the predicted probability of voting between being in the control group versus being in a different group?
prp(CARTcontrol, digits = 6)
.34 - .296638

# Using the second tree, determine who is affected more by not being in the control group
prp(CARTsex, digits = 6)
.334176 - .290456
# = .04372
.345818 - .302795
# = .04302
# Men and women are affected about the same

# Create a logistic regression model using 'sex' and 'control'
logModelSex = glm(voting ~ control + sex, data = gerber, family = binomial)

# Interpret the coefficient for 'sex'
summary(logModelSex)
# Since women have a larger value in the sex variable, so negative coefficient means women are less likely to vote

# Create a dataframe containing all of the possible values of sex and control
possibilities = data.frame(sex = c(0, 0, 1, 1), control = c(0, 1, 0, 1))

# Evaluate the logistic regression
predict(logModelSex, newdata = possibilities, type = 'response')

# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?
abs(.290456 - .2908065)

# Add a new term to the logistic regression, that is the combination of the 'sex' and 'control' variables, so if this new variable is 1, that means the person is a woman and in the control group
logModel2 = glm(voting ~ sex + control + sex:control, data = gerber, family = binomial)
summary(logModel2)
# The coefficient is negative, so that means that a value of 1 in this variable decreases the chance of voting

# Evaluate the logistic regression
predict(logModel2, newdata = possibilities, type = 'response')

# Now what is the difference between the logistic regression model and the CART model for the (Woman, Control) case?
abs(.2904558 - .290456)
