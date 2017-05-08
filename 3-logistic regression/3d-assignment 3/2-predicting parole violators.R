parole = read.csv('2-parole.csv')

# How many parolees are contained in the dataset?
nrow(parole)

# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)

# 'state' and 'crime' are unordered factors which have at least 3 levels
# We need to convert them to factors for our prediction problem
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state)
summary(parole$crime)

# Split into a training and testing set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Train a logistic regression model on the training set
mod = glm(violator ~ ., data = train, family = binomial)

# What variables are significant in this model?
summary(mod)

# If we have a coefficient c for a variable, then that means the:
# a. log odds (or Logit) are increased by c for a unit increase in the variable, or
# b. odds are multiplied by e^c for a unit increase in the variable

# What can we say based on the coefficient of the multiple.offenses variable?
exp(1.6119919)
# The model predicts that a parolee who commited multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses, but is otherwise identical

# Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and commited a larceny
# According to the model, what are the odds this individual is a violator?
# log(odds) =
-4.2411574 + 0.3869904 * 1 + 0.8867192 * 1 - 0.0001756 * 50 + 0.4433007 * 0 + 0.8349797 * 0 - 3.3967878 * 0 - 0.1238867 * 3 + 0.0802954 * 12 + 1.6119919 * 0 + 0.6837143 * 1 - 0.2781054 * 0 - 0.0117627 * 0
# odds =
exp(-1.700629)

# According to the model, what is the probability this individual is a violator?
1 / (1 + exp(-1 * (-1.700629)))

# Obtain the model's predicted probabilities for parolees in the testing set
prediction = predict(mod, newdata = test, type = 'response')

# What is the maximum predicted probability of a violation?
summary(prediction)

# Evaluate the model's predictions on the test using a threshold of 0.5
table(test$violator, as.numeric(prediction >= 0.5))

# What is the model's sensitivity?
12/(12+11)

# What is the model's specificity?
167/(167+12)

# What is the model's accuracy?
(12+167)/(12+11+12+167)

# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
table(test$violator)
179/(179+23)

# The job of parole board is to make sure that a prisoner is ready to be released into free society
# Therefore parole boards tend to be particularly concerned about releasing prisoners who will violate their parole
# So, best course of action:
# The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5

# The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline model has 0 false positives and 23 false negatives

# What is the AUC value for the model?
library(ROCR)
pred = prediction(prediction, test$violator)
auc = as.numeric(performance(pred, 'auc')@y.values)
auc

# The meaning of AUC in this context is the probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator

