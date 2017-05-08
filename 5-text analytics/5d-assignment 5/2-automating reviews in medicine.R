trials = read.csv('2-clinical_trial.csv', stringsAsFactors = FALSE)

# how many characters are there in the longest abstract?
max(nchar(trials$abstract))

# how many search results provided no abstract?
table(nchar(trials$abstract) == 0)

# find the observation with the minimum number of characters in the title
# what is the text of the title of this article?
which.min(nchar(trials$title))
trials$title[1258]

# build 2 corpera
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

# convert the corpera to lowercase
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

# remove the punctuation in the corpera
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

# remove the english language stop words from the corpera
corpusTitle = tm_map(corpusTitle, removeWords, stopwords('english'))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords('english'))

# stem the words in the corpera
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# build document term matrices from the corpera
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# limit the matrices to terms with sparseness of at most 95%
dtmTitle = removeSparseTerms(dtmTitle, .95)
dtmAbstract = removeSparseTerms(dtmAbstract, .95)

# convert the matrices to data frames
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# how many terms remain in the data frames?
ncol(dtmTitle)
ncol(dtmAbstract)

# what is the most frequent word stem across all the abstracts?
which.max(colSums(dtmAbstract))

# add the letter T in front of all the title variable names
colnames(dtmTitle) = paste0('T', colnames(dtmTitle))

# add the letter A in front of all the abstract variable names
colnames(dtmAbstract) = paste0('A', colnames(dtmAbstract))

# combine the data frames into a single data frame
dtm = cbind(dtmTitle, dtmAbstract)

# add the dependent variable 'trial'
dtm$trial = trials$trial

# how many columns are in this combined data frame?
ncol(dtm)

# split the data frame into a training and testing set
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, .7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

# what is the accuracy of the baseline model on the training set?
table(train$trial)
730 / (730 + 572)

# build a CART model using all the independent variables in the training set to train the model
library(rpart)
library(rpart.plot)
trialCart = rpart(trial ~ ., data = train, method = 'class')

# what is the name of the first variable the model split on?
prp(trialCart)

# obtain the training set predictions for the model
# extract the predicted probability of a result being a trial
predTrain = predict(trialCart)[,2]

# what is the maximum predicted probability for any result?
max(predTrain)
# because the CART tree assigns the same predicted probability to each leaf node and there are a small number of leaf nodes compared to data points, the maximum predicted probability will likely be exactly the same in the testing set

# what is the training set accuracy of the CART model?
table(train$trial, predTrain >= .5)
(641 + 421) / (641 + 89 + 151 + 421)

# what is the training set sensitivity of the CART model?
421 / (421 + 151)

# what is the training set specificity of the CART model?
641 / (641 + 89)

# evaluate the CART model on the testing set
# create a vector of predicted probabilities
predTest = predict(trialCart, newdata = test)[,2]

# what is the testing set accuracy, assuming a probability threshold of .5 for predicting that a result is a clinical trial?
table(test$trial, predTest >= .5)
(270 + 155) / (270 + 43 + 90 + 155)

# what is the testing set AUC of the prediction model?
library(ROCR)
predRocr = prediction(predTest, test$trial)
as.numeric(performance(predRocr, 'auc')@y.values)

# the decision maker for this problem would use a model in the following workflow:
# 1) for all the papers retrieved, predict which papers are clinical trials using the model; this yields some initial set A of papers predicted to be trials, and some set B of papers predicted not to be trials
# 2) the decision maker manually reviews all papers in set A; this yields a more limited set of papers to be included in the study
# 3) perform the study-specific analysis, using data extracted from the limited set of papers identified in step 2
# a false negative is a paper that should have been included in Set A but was missed by the model
# this means a study that should have been included in step 3 was missed, affecting the results
# a false positive is a paper that should not have been included in set A but that was actually included
# however, because of the manual review, this extra paper will not make it into the more limited set of papers, and therefore this mistake will not affect the analysis in step 3
# the cost of a false negative is much higher than the cost of a false positive
# the decision maker should use a probability threshold less than .5 for the machine learning model
