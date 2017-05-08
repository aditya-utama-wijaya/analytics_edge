pisaTrain = read.csv('2-pisa2009train.csv')
pisaTest = read.csv('2-pisa2009test.csv')

# How many students are there in the training set?
nrow(pisaTrain)

# What is the average reading test score of males and of females?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Which variables are missing data in at least 1 observation in the training set?
summary(pisaTrain)

# Remove observations with any missing value
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# How many obserations are now in the sets?
nrow(pisaTrain)
nrow(pisaTest)

# Set the most common level as the reference level in the race variable
pisaTrain$raceeth = relevel(pisaTrain$raceeth, 'White')
pisaTest$raceeth = relevel(pisaTest$raceeth, 'White')

# Build a linear regression model using the training set to predict readingScore using all the remaining variables
lmScore = lm(readingScore ~ ., data = pisaTrain)

# What is the Multiple R-squared value of lmScore on the training set?
summary(lmScore)

# What is the training set RMSE of lmScore?
SSE = sum(lmScore$residuals ^ 2)
RMSE = sqrt(SSE / nrow(pisaTrain))
RMSE

# Predict the reading scores of students in pisaTest
predTest = predict(lmScore, newdata = pisaTest)

# What is the range between the maximum and minimum predicated reading score on the test set?
summary(predTest)

# What is the SSE of lmScore on the testing set?
SSE2 = sum((predTest - pisaTest$readingScore) ^ 2)
SSE2

# What is the RMSE of lmScore on the testing set?
RMSE2 = sqrt(SSE2 / nrow(pisaTest))
RMSE2

# What is the predicted test score used in the baseline model?
baseline = mean(pisaTrain$readingScore)
baseline

# What is the sum of squared errors of the baseline model on the testing set?
SST = sum((baseline - pisaTest$readingScore) ^ 2)
SST

# What is the test-set R-squared value of lmScore?
1-SSE2/SST
