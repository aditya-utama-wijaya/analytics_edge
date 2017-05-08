claims = read.csv('6-ClaimsData.csv')
str(claims)

table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
spl = sample.split(claims$bucket2009, SplitRatio = 0.6)
claimsTrain = subset(claims, spl == TRUE)
claimsTest = subset(claims, spl == FALSE)

# What is the average age of patients in the training set?
mean(claimsTrain$age)

# What proportion of people in the training set had at least 1 diagnosis code for diabetes?
table(claimsTrain$diabetes)/nrow(claimsTrain)

table(claimsTest$bucket2009, claimsTest$bucket2008)
(110138+10721+2774+1539+104)/nrow(claimsTest)

penaltyMatrix = matrix(c(0, 1, 2, 3, 4, 2, 0, 1, 2, 3, 4, 2, 0, 1, 2, 6, 4, 2, 0, 1, 8, 6, 4, 2, 0), byrow = TRUE, nrow = 5)
penaltyMatrix

as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix) / nrow(claimsTest)

# The baseline method would predict cost bucket 1 for everyone
# What would the accuracy of the baseline method be on the test set?
table(claimsTest$bucket2009)
122978/nrow(claimsTest)

# What would the penalty error of the baseline method be on the test set?
(0 * 122978 + 2 * 34840 + 4 * 16390 + 6 * 7937 + 8 * 1057) / nrow(claimsTest)

library(rpart)
library(rpart.plot)
claimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = claimsTrain, method = 'class', cp = .00005)
prp(claimsTree)

predictTest = predict(claimsTree, newdata = claimsTest, type = 'class')
table(claimsTest$bucket2009, predictTest)
(114141+16102+118+201+0)/nrow(claimsTest)

as.matrix(table(claimsTest$bucket2009, predictTest)) * penaltyMatrix
sum(as.matrix(table(claimsTest$bucket2009, predictTest)) * penaltyMatrix) / nrow(claimsTest)

claimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = claimsTrain, method = 'class', cp = .00005, parms = list(loss = penaltyMatrix))
predictTest = predict(claimsTree, newdata = claimsTest, type = 'class')
table(claimsTest$bucket2009, predictTest)
(94310+18942+4692+636+2)/nrow(claimsTest)
sum(as.matrix(table(claimsTest$bucket2009, predictTest)) * penaltyMatrix) / nrow(claimsTest)
