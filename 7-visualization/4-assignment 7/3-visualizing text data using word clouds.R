tweets = read.csv('3-tweets.csv', stringsAsFactors = FALSE)

# create a corpus using the Tweet variable
library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))

# convert the corpus to lowercase
corpus = tm_map(corpus, tolower)

# remove punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)

# remove all english-language stopwords
corpus = tm_map(corpus, removeWords, stopwords('english'))

# build a document-term matrix out of the corpus
corpus = tm_map(corpus, PlainTextDocument)
frequencies = DocumentTermMatrix(corpus)

# convert the document-term matrix to a data frame
allTweets = as.data.frame(as.matrix(frequencies))

# how many unique words are there across all the documents?
frequencies

# although we typically stem words during the text preprocessing step, we did not do so here
# it will be easier to read and understand the word cloud if it includes full words instead of just the word stems

install.packages('wordcloud')
library(wordcloud)

# we will need to provide the function with a vector of words and a vector of word frequencies
# which function can we apply to allTweets to get a vector of the words in the dataset?
# => colnames
# each tweet represents a row of allTweets, and each word represents a column
# which function should we apply to allTweets to obtain the frequency of each word across all tweets?
# => colSums

# build the word cloud
wordcloud(colnames(allTweets), colSums(allTweets))

# remove the word 'apple'
corpus = tm_map(corpus, removeWords, c('apple', stopwords('english')))

# replace allTweets with the document-term matrix of this new corpus
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

# create a word cloud with the updated corpus
wordcloud(colnames(allTweets), colSums(allTweets))

# build a word cloud based only on the negative tweets
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

# min.freq and max.words are parameters that can be used to remove the least frequent words, resulting in a less cluttered word cloud

# if random.order is set to FALSE, then the most frequent (largest) words will be plotted first, resulting in them being displayed together in the center of the word cloud

# rot.per controls the proportion of words that are rotated to be vertical in the word cloud; by default, 10% of words are rotated

# when random.color is set to TRUE, the words will be colored randomly

# which color palette would be most appropriate for use in a word cloud for which we want to use color to indicate word frequency?
display.brewer.all()
# Accent and Set2 are both 'qualitative palettes', which means color changes don't imply a change in magnitude
# on the other hand, YlOrRd is a 'sequential palette', with earlier colors begin lighter and later colors being darker

# which RColorBrewer palette name would be most appropriate to use when preparing an image for a document that must be in grayscale?
# => Greys

wordcloud(colnames(allTweets), colSums(allTweets), min.freq = 4, random.order = FALSE, color = brewer.pal(9, 'Blues')[5:9])
