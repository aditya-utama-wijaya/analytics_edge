# Read the dataset into R!
climate = read.csv('1-climate_change.csv')

# Split the data into a training set!
train = subset(climate, Year <= 2006)

# and a testing set
test = subset(climate, Year > 2006)

# Build a linear regression model to predict the dependent variable Temp!
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)

# Enter the model R^2 (the Multiple R-squared value)!
# Which variables are significant in the model? (p-value < 0.05)?
summary(climatelm)

# Compute the correlations between all the variables in the training set!
cor(train)

# Given that the correlations are so high, focus on the N2O variable and build a model with only MEI, TSI, Aerosols, and N2O as independent variables!
climatelm2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = train)

# Enter the coefficient of N2O in the reduced model!
# Enter the model R^2!
summary(climatelm2)

# Use the step function to automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R^2
# Which variables were eliminated from the full model by the step function?
climateStep = step(climatelm)

# Enter the R^2 value of the model produced by the step function
summary(climateStep)

tempPredict = predict(climateStep, newdata = test)
SSE = sum((tempPredict - test$Temp) ^ 2)
SST = sum((mean(train$Temp) - test$Temp) ^ 2)
R2 = 1 - SSE/SST
R2
