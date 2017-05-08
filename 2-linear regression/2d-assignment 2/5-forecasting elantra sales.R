Elantra = read.csv('5-elantra.csv')
ElantraTrain = subset(Elantra, Year <= 2012)
ElantraTest = subset(Elantra, Year > 2012)

# How many observations are in the training set?
nrow(ElantraTrain)

# Build a regression model to predict monthly Elantra sales
ElantraLM = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)
summary(ElantraLM)

# Build a new linear regression model that predicts monthly Elantra sales to incorporate the seasonal effect due to the month
ElantraLM = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data = ElantraTrain)
summary(ElantraLM)

# We should convert Month to a factor variable before adding it to the model
# By modeling Month as a factor variable, the effect of each calendar month is not restricted to be linear in the numberical coding of the month
ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
ElantraLM = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + MonthFactor, data = ElantraTrain)
summary(ElantraLM)

# Another peculiar observation about the regression is that the sign of the Queries variable has changed
# Furthermore, CPI_energy has a positive coefficient, which seems counterintuitive
# Which variable is CPI_energy highly correlated with? (cor > 0.6)
cor(ElantraTrain[c('Unemployment', 'Month', 'Queries', 'CPI_energy', 'CPI_all')])
# Multicolinearity

# Simplify the model
ElantraLM = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + MonthFactor, data = ElantraTrain)
summary(ElantraLM)

# Make predictions on the test set
PredictTest = predict(ElantraLM, newdata = ElantraTest)
SSE = sum((PredictTest - ElantraTest$ElantraSales) ^ 2)
SSE

# What would the baseline method predict for all observations in the test set?
mean(ElantraTrain$ElantraSales)

# What is the test set R-squared?
SST = sum((mean(ElantraTrain$ElantraSales) - ElantraTest$ElantraSales) ^ 2)
1-SSE/SST

# What is the largest absolute error that we make in test set predicitions?
max(abs(PredictTest - ElantraTest$ElantraSales))

# In which period do we make the largest absolute error in our prediction?
which.max(abs(PredictTest - ElantraTest$ElantraSales))
ElantraTest$Month[5]
ElantraTest$Year[5]
