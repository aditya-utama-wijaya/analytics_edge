emails = read.csv('3-emails.csv', stringsAsFactors = FALSE)

# how many emails are in the dataset?
nrow(emails)

# how many of the emails are spam?
table(emails$spam)

# how many characters are in the longest email in the dataset?
max(nchar(emails$text))

# which row contains the shortest email in the dataset?
which.min(nchar(emails$text))

# build a new corpus variable
library(tm)
corpus = Corpus(VectorSource(emails$text))

# convert the text to lowercase
corpus = tm_map(corpus, tolower)

# remove all punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)

# remove all English stopwords from the corpus
corpus = tm_map(corpus, removeWords, stopwords('english'))

# stem the words in the corpus
corpus = tm_map(corpus, stemDocument)

# build a document term matrix from the corpus
corpus = tm_map(corpus, PlainTextDocument)
dtm = DocumentTermMatrix(corpus)

# how many terms are in dtm?
dtm

# to obtain a more reasonable number of terms, limit dtm to contain terms appearing in at least 5% of documents
spdtm = removeSparseTerms(dtm, .95)

# how many terms are in spdtm?
spdtm

# build a data fram from spdtm
emailsSparse = as.data.frame(as.matrix(spdtm))

# make the variable names valid
colnames(emailsSparse) = make.names(colnames(emailsSparse))

# what is the word stem that shows up most frequently across all the emails in the dataset?
sort(colSums(emailsSparse))

# copy over the 'spam' variable from the original data frame
emailsSparse$spam = emails$spam

# how many word stems appear at least 5000 times in the ham emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 0)))

# how many word stems appear at least 1000 times in the spam emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 1)))
# the lists of most common words are significantly different between the spam and ham emails, so the frequencies of these most common words are likely to help differentiate between spam and ham

# convert the dependent variable to a factor
emailsSparse$spam = as.factor(emailsSparse$spam)

# split the data frame
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, .7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

# using the training set, train the following 3 machine learning models
# 1) a logistic regression model
spamLog = glm(spam ~ ., data = train, family = binomial)

# 2) a CART model, using the default parameters
library(rpart)
spamCart = rpart(spam ~ ., data = train, method = 'class')

# 3) a random forest model, using the default parameters
library(randomForest)
set.seed(123)
spamRf = randomForest(spam ~ ., data = train)

# for each model, obtain the predicted spam probabilities for the training set
predTrainLog = predict(spamLog, type = 'response')
predTrainCart = predict(spamCart)[,2]
predTrainRf = predict(spamRf, type = 'prob')[,2]

# how many of the training set predicted probabilities from spamLog are less than .00001?
table(predTrainLog < .00001)

# how many of the training set predicted probabilities from spamLog are more than .99999?
table(predTrainLog > .99999)

# how many of the training set predicted probabilities from spamLog are between .00001 and .99999?
table(predTrainLog >= .00001 & predTrainLog <= .99999)

# how many variables are labeled as significant (at the p = .05 level) in the logistic regression?
library(rpart.plot)
summary(spamLog)

# how many word stems appear in the CART tree?
prp(spamCart)

# what is the training set accuracy of spamLog, using a threshold of .5 for predictions?
table(train$spam, predLog > .5)
(3034 + 836) / (3034 + 18 + 122 + 836)

# what is the training set AUC of spamLog?
library(ROCR)
predictionTrainLog = prediction(predLog, train$spam)
as.numeric(performance(predictionTrainLog, 'auc')@y.values)

# what is the training set accuracy of spamCart, using a threshold of .5 for predictions?
table(train$spam, predTrainCart > .5)
(2877 + 894) / (2877 + 175 + 64 + 894)

# what is the training set AUC of spamCart?
predictionTrainCart = prediction(predTrainCart, train$spam)
as.numeric(performance(predictionTrainCart, 'auc')@y.values)

# what is the training set accuracy of spamRf, using a threshold of .5 for predictions?
table(train$spam, predTrainRf > .5)
(3017 + 904) / (3017 + 35 + 54 + 904)

# what is the training set AUC of spamRf?
predictionTrainRf = prediction(predTrainRf, train$spam)
as.numeric(performance(predictionTrainRf, 'auc')@y.values)

# what is the testing set accuracy of spamLog, using a threshold of .5 for predictions?
predTestLog = predict(spamLog, newdata = test, type = 'response')
table(test$spam, predTestLog > .5)
(1279 + 342) / (1279 + 29 + 68 + 342)

# what is the testing set AUC of spamLog?
predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, 'auc')@y.values)

# what is the testing set accuracy of spamCart, using a threshold of .5 for predictions?
predTestCart = predict(spamCart, newdata = test)[,2]
table(test$spam, predTestCart > .5)
(1219 + 379) / (1219 + 89 + 31 + 379)

# what is the testing set AUC of spamCart?
predictionTestCart = prediction(predTestCart, test$spam)
as.numeric(performance(predictionTestCart, 'auc')@y.values)

# what is the testing set accuracy of spamRf, using a threshold of .5 for predictions?
predTestCart = predict(spamRf, newdata = test, type = 'prob')[,2]
table(test$spam, predTestCart > .5)
(1295 + 381) / (1295 + 13 + 29 + 381)

# what is the testing set AUC of spamRf?
predictionTestRf = prediction(predTestCart, test$spam)
as.numeric(performance(predictionTestRf, 'auc')@y.values)

# A false negative means the model labels a spam email as ham
# This results in a spam email being displayed in the main inbox
# A false positive means the model labels a ham email as spam
# This results in a ham email being sent to the Junk Email folder
# A false negative is largely a nuisance; the user will need to delete the unsolicited email
# However, a false positive can be very costly, since the user might completely miss an important email due to it being delivered to the spam folder

# What sort of user might assign a particularly high cost to a false negative result?
# A user who is particularly annoyed by spam email reaching their main inbox

# What sort of user might assign a particularly high cost to a false positive result?
# a user who never checks his/her Junk Email folder

# the sum of all elements in a row of the document term matrix is equal to the number of terms present in the document corresponding to the row
wordCount = rowSums(as.matrix(dtm))

# what would have occured if we had instead created wordCount using spdtm instead of dtm?
# spdtm has had sparse terms removed, which means we have removed some of the columns but non of the rows from dtm
# this means rowSums will still return a sum for each row (1 for each email), but it will not have counted the frequencis of any uncommon words in the dataset
# as a result, wordCount will only count some of the words

# plot the distribution of wordCount in the dataset
hist(wordCount)

# plot the distribution of log(wordCount) in the dataset
hist(log(wordCount))

emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)
# logWordCount is slightly smaller in spam messages than in ham messages

# train a CART tree with the default parameters
train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)
spam2Cart = rpart(spam ~ ., data = train2, method = 'class')
prp(spam2Cart)

# train a random forest with the default parameters
set.seed(123)
spam2Rf = randomForest(spam ~ ., data = train2)

# perform test-set predictions using the new CART and random forest models
pred2Cart = predict(spam2Cart, newdata = test2)[,2]
pred2Rf = predict(spam2Rf, newdata = test2, type = 'prob')[,2]

# what is the test-set accuracy of spam2Cart, using threshold .5 for predicting an email is spam?
table(test$spam, pred2Cart > .5)
(1239 + 369) / (1239 + 69 + 41 + 369)

# what is the test-set AUC of spam2Cart?
prediction2Cart = prediction(pred2Cart, test$spam)
as.numeric(performance(prediction2Cart, 'auc')@y.values)

# what is the test-set accuracy of spam2Rf, using a threshold of .5 for predicting if an email is spam?
table(test$spam, pred2Rf > .5)
(1296 + 383) / (1296 + 12 + 27 + 383)

# what is the test-set AUC of spam2Rf?
prediction2Rf = prediction(pred2Rf, test$spam)
as.numeric(performance(prediction2Rf, 'auc')@y.values)
