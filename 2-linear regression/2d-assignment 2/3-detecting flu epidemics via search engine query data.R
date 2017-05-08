FluTrain = read.csv('3-FluTrain.csv')

# Which week corresponds to the highest percentage of ILI-related physician visits?
which.max(FluTrain$ILI)
FluTrain$Week[303]

# Which week corresponds to the highest percentage of ILI-related query fraction?
which.max(FluTrain$Queries)
FluTrain$Week[303]

hist(FluTrain$ILI)

# Plot the Queries versus the natural logarithm of ILI to prevent the small number of unusually large or small observations from having an undue influence on the SSE of predictive models
plot(FluTrain$Queries, log(FluTrain$ILI))

# What is the training set R-squared value for the regression model from the previous problem?
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

# For a single variable linear regression model, there is a direct relationship between R-squared and the correlation between the independent and dependent variables.
Correlation = cor(log(FluTrain$ILI), FluTrain$Queries)
# R-squared = Correlation ^ 2

FluTest = read.csv('3-FluTest.csv')

# Normally, we would obtain test-set predicitions from the model using the code:
PredTest1 = predict(FluTrend1, newdata = FluTest)

# However, the dependent variable in our model is log(ILI), so it would countain predictions of the log(ILI) value
# To predict the ILI value:
PredTest1 = exp(predict(FluTrend1, newdata = FluTest))

# What is our estimate for the percentage of ILI-related physician isits for the week of March 11, 2012?
Estimated1 = subset(PredTest1, FluTest$Week == '2012-03-11 - 2012-03-17')

# What is the relative error between the estimate and the observed value for the week of March 11, 2012?
(FluTest$ILI[11]-Estimated1)/FluTest$ILI[11]

# What is the RMSE between our estimates and the actual observations for the percentage of ILI-related physician visits on the test-set?
SSE = sum((PredTest1 - FluTest$ILI) ^ 2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE

# Use the 'zoo' package, which provides a number of helpful methods for time series models
install.packages('zoo')
library(zoo)

# Build a variable that contains the ILI value from 2 weeks before the current observation
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
# The parameter na.pad = TRUE means to add missing values for the first 2 weeks of our dataset, where we can't compute the data from 2 weeks earlier

# How many values are missing in the new ILILag2 variable?
summary(FluTrain$ILILag2)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable and the log of the ILILag2 variable
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)

# Which coefficients are significant at the p=0.05 level in this regression model?
# What is the R^2 value of the model?
summary(FluTrend2)

# Add an ILILag2 variable to the FluTest data frame
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 = coredata(ILILag2)

# How many missing values are there in this new variable?
summary(FluTest$ILILag2)

# Fill in the missing values for ILILag2 in FluTest
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain) - 1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]

# What is the new value of the ILILag2 variable in the first and second row of FluTest?
FluTest$ILILag2[1]
FluTest$ILILag2[2]

# Obtain test set predictions of the ILI variable from the FluTrend2 model
PredTest2 = exp(predict(FluTrend2, newdata = FluTest))

# What is the test-set RMSE of the model?
SSE = sum((PredTest2 - FluTest$ILI) ^ 2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE
