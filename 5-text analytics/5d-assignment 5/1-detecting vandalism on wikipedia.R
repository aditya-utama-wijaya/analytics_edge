wiki = read.csv('1-wiki.csv', stringsAsFactors = FALSE)

# Convert the 'Vandal' column to a factor
wiki$Vandal = as.factor(wiki$Vandal)

# How many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)

# Create the corpus for the Added column
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))

# Remove the English-language stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords('english'))

# Stem the words
corpusAdded = tm_map(corpusAdded, stemDocument)

# Build the DocumentTermMatrix
corpusAdded = tm_map(corpusAdded, PlainTextDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)

# How many terms appear in dtmAdded?
dtmAdded

# Filter out sparse terms by keeping only terms that appear in .3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, .997)

# How many terms appear in sparseAdded?
sparseAdded

# Convert sparseAdded to a data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))

# Prepend all the words with the letter A
colnames(wordsAdded) = paste('A', colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords('english'))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, .997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste('R', colnames(wordsRemoved))

# how many words are in the wordsRemoved data frame?
ncol(wordsRemoved)

# combine the 2 data frames into a data frame
wikiWords = cbind(wordsAdded, wordsRemoved)

# add the Vandal column
wikiWords$Vandal = wiki$Vandal

# split the data
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, .7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# what is the accuracy on the test set of a baseline method that always predicts the most frequent outcome?
table(test$Vandal)
618 / (618 + 545)

# Build a CART model to predict Vandal
library(rpart)
library(rpart.plot)
wikiCart = rpart(Vandal ~ ., data = train, method = 'class')
pred = predict(wikiCart, newdata = test, type = 'class')

# what is the accuracy of the model on the test set?
# if we add the argument type = 'class' when making predictions, the output of predict will automatically use a threshold of .5
table(test$Vandal, pred)
(618 + 12) / (618 + 0 + 533 + 12)

# plot the CART tree
prp(wikiCart)

# Create a copy of the dataframe
wikiWords2 = wikiWords

# Make a new column in wikiWords2 that is 1 if 'http' was in Added
wikiWords2$http = ifelse(grepl('http', wiki$Added, fixed = TRUE), 1, 0)

# Based on this new column, how many revisions added a link?
table(wikiWords2$http)

# make new training and testing sets
train2 = subset(wikiWords2, spl == TRUE)
test2 = subset(wikiWords2, spl == FALSE)

# create a new CART model
wikiCart2 = rpart(Vandal ~ ., data = train2, method = 'class')

# what is the new accuracy of the CART model on the test set using a threshold of .5?
pred2 = predict(wikiCart2, newdata = test2, type = 'class')
table(test2$Vandal, pred2)
(609 + 57) / (609 + 9 + 488 + 57)

# sum the rows of dtmAdded and dtmRemoved and add them as new variables in the data frame wikiWords2
wikiWords2$numWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$numWordsRemoved = rowSums(as.matrix(dtmRemoved))

# what is the average number of words added?
mean(wikiWords2$numWordsAdded)

# make new training and testing sets with wikiWords2
train3 = subset(wikiWords2, spl == TRUE)
test3 = subset(wikiWords2, spl == FALSE)

# create the CART model again
wikiCart3 = rpart(Vandal ~ ., data = train3, method = 'class')

# what is the new accuaracy of the CART model on the test set?
pred = predict(wikiCart3, newdata = test3, type = 'class')
table(test3$Vandal, pred)
(514 + 248) / (514 + 104 + 297 + 248)

# make a copy of wikiWords2
wikiWords3 = wikiWords2

# add 2 original variables Minor and Loggedin to the new data frame
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

# make new training and testing sets with wikiWords3
train4 = subset(wikiWords3, spl == TRUE)
test4 = subset(wikiWords3, spl == FALSE)

# build a CART model using all the training data
wikiCart4 = rpart(Vandal ~ ., data = train4, method = 'class')

# what is the accuraracy of the model on the test set?
pred4 = predict(wikiCart4, newdata = test4, type = 'class')
table(test4$Vandal, pred4)
(595 + 241) / (595 + 23 + 304 + 241)

# how many splits are there in the tree?
prp(wikiCart4)
