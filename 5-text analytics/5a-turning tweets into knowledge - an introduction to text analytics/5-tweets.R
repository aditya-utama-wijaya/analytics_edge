# Problems that are suited for natural language processing:
# - processing medical records to extract symptoms
# - flagging customer reviews as problematic
# - automatically organizing emails based on their content

tweets = read.csv('5-tweets.csv', stringsAsFactors = FALSE)
str(tweets)

tweets$negative = as.factor(tweets$Avg <= -1)
table(tweets$negative)

install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]

corpus = tm_map(corpus, tolower)
corpus[[1]]

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

stopwords('english')[1:10]
corpus = tm_map(corpus, removeWords, c('apple', stopwords('english')))
corpus[[1]]

corpus = tm_map(corpus, stemDocument)
corpus[[1]]

corpus = tm_map(corpus, PlainTextDocument)
frequencies = DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)

sparse = removeSparseTerms(frequencies, .995)
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

tweetsSparse$negative = tweets$negative

library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$negative, .7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

# Which words appear at least 100 times?
findFreqTerms(frequencies, lowfreq = 100)

library(rpart)
library(rpart.plot)
tweetCart = rpart(negative ~ ., data = trainSparse, method = 'class')
prp(tweetCart)

predictCart = predict(tweetCart, newdata = testSparse, type = 'class')
table(testSparse$negative, predictCart)
(291 + 19) / (291 + 9 + 36 + 19)

table(testSparse$negative)
300 / (300 + 55)

library(randomForest)
set.seed(123)
tweetRf = randomForest(negative ~ ., data = trainSparse)
predictRf = predict(tweetRf, newdata = testSparse)
table(testSparse$negative, predictRf)
(289 + 23) / (289 + 11 + 32 + 23)

# Build a logistic regression model using the training set to predict 'negative' using all of the independent variables
tweetLog = glm(negative ~ ., data = trainSparse, family = binomial)

# Make predictions using the logistic regression model
predictions = predict(tweetLog, newdata = testSparse, type = 'response')
table(testSparse$negative, predictions >= .5)
# The warning messages have to do with the number of variables, and the fact that the model is overfitting to the training set
