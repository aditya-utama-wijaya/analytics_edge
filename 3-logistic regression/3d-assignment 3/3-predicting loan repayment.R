loans = read.csv('3-loans.csv')

# What proportion of the loans in the dataset were not paid in full?
table(loans$not.fully.paid)
1533/(1533+8045)

# Which variables has at least 1 missing observation?
summary(loans)

# Build a data frame with the observations missing at least 1 value
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))

# Removing this small number of observations would not lead to overfitting
nrow(missing)

# The observations with missing data have a similar rate of not paying in full, so removing them would not bias subsequent model
table(missing$not.fully.paid)

# However, to predit risk for loans with missing data, we need to fill in the missing values instead of removing the observations
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), 'not.fully.paid')
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
# We predicted missing variables values using the available independent ariables for each observation

# Split the dataset into a training and testing set
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)

# Predict the dependent variable using all the independent variables
mod = glm(not.fully.paid ~ ., data = train, family = binomial)

# Which independent variables are significant in our model?
summary(mod)

# Consider 2 loan applications, which are identical other than the fact that the borrower in Application A has FICO credit score 700, while the borrower in Application B has FICO credit score 710
# What is the value of Logit(A) - Logit(B)?
(-9.406e-03 * 700) - (-9.406e-03 * 710)

# What is the value of O(A)/O(B)?
# = exp(Logit(A) - Logit(B))
exp(0.09406)
# The predicted odds of loan A not being paid back in full are 1.098626 times larger than the predicted odds for loan B

# Predict the probability of the test set loans not being paid back in full
test$predicted.risk = predict(mod, newdata = test, type = 'response')

# Compute the confusion matrix using a threshold of 0.5
table(test$not.fully.paid, test$predicted.risk > 0.5)

# What is the accuracy of the logistic regression model?
(2400+3)/(2400+13+457+3)

# What is the accuracy of the baseline model?
table(test$not.fully.paid)
2413/(2413+460)

# Compute the test set AUC
library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, 'auc')@y.values)

# Build a bivariate logistic regression model that predicts the dependent variable using only the variable int.rate
bivariate = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(bivariate)
# Decreased significance between a bivariate and multivariate model is typically due to correlation

# Make test set predictions for the bivariate model
pred.bivariate = predict(bivariate, newdata = test, type = 'response')

# What is the highest predicted probability of a loan not being paid in full on the testing set?
summary(pred.bivariate)
# With a logistic regression cutoff of 0.5, no loans would be flagged

# What is the test set AUC of the bivariate model?
prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, 'auc')@y.values)

# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest?
10 * exp(.06 * 3)

# What is the profit to the investor if the investment is paid back in full?
c * exp(rt) - c

# Assume, no money was receied from the borrower
# What is the profit to the investor?
-c

# In order to evaluate the quality of an investment strategy, we need to compute the profit for each loan in the test set
# c = 1, t = 3
test$profit = exp(test$int.rate * 3) - 1

# Replace the value with -1 in the cases where the loan was not paid in full
test$profit[test$not.fully.paid == 1] = -1

# What is the maximum profit of a $10 investment in any loan in the testing set?
summary(test$profit)
10 * 0.8894769

# Build a data frame consisting of the test set loans with an interest rate of at least 15%
highInterest = subset(test, int.rate >= .15)

# What is the average profit of a $1 investment in 1 of these high-interest loans?
summary(highInterest$profit)

# What proportion of the high-interest loans were not paid back in full?
table(highInterest$not.fully.paid)
110/(110+327)

# Determine the 100th smallest predicted probability of not paying in full
cutoff = sort(highInterest$predicted.risk, decreasing = FALSE)[100]

# Build a data frame consisting of the high-interest loans with predicted risk not exceeding the cutoff
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
nrow(selectedLoans)

# What is the profit of the investor who invested $1 in each of these 100 loans?
sum(selectedLoans$profit)

# How many of 100 selected loans were not paid back in full?
table(selectedLoans$not.fully.paid)
