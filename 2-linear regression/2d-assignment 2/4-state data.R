data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
str(statedata)

# Plot all of the states' centers with latitude on the y axis and longitude on the x axis
plot(statedata$x, statedata$y)

# Determine which region has the highest average high school graduation rate of all the states in the region
sort(tapply(statedata$HS.Grad, statedata$state.region, mean))

# Which region has the highest median murder rate?
boxplot(statedata$Murder ~ statedata$state.region)

# There is an outlier in the Northeast region of the boxplot.
# Which state does this correspond to?
NortheastData = subset(statedata, state.region == 'Northeast')
table(NortheastData$Murder, NortheastData$state.abb)

# Build a model to predict life expectancy by state using the state statistics in the dataset
LinReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)

# What is the coefficient for 'Income' in the linear regression model?
summary(LinReg)

plot(statedata$Income, statedata$Life.Exp)

# Experiment with removing independent variables from the original model
# Remove the one with the largest 'p-value' first, or the one with the 't-value' closest to 0
# Remove them one at a time (backwards variable selection)
# This is important due to multicollinearity issues; removing 1 insignificant variable may make another previously insignificant variable become significant
LinReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(LinReg)

LinReg = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(LinReg)

LinReg = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LinReg)

# Which state do we predict to have the lowest and highest life expectancy?
sort(predict(LinReg))

# Which state actually has the lowest life expectancy?
which.min(statedata$Life.Exp)
statedata$state.name[40]

# Which state actually has the highest life expectancy?
which.max(statedata$Life.Exp)
statedata$state.name[11]

# For which state do we make the smallest and largest absolute error?
sort(abs(LinReg$residuals))
