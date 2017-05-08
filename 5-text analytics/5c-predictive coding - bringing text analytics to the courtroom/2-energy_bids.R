emails = read.csv('2-energy_bids.csv', stringsAsFactors = FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]
table(emails$responsive)

library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, PlainTextDocument)

dtm = DocumentTermMatrix(corpus)
dtm

dtm = removeSparseTerms(dtm, .97)
dtm

labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)

library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, .7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

library(rpart)
library(rpart.plot)
emailCart = rpart(responsive ~ ., data = train, method = 'class')
prp(emailCart)

pred = predict(emailCart, newdata = test)
pred[1:10, ]
pred.prob = pred[ ,2]
table(test$responsive, pred.prob >= .5)
(199 + 17) / (199 + 16 + 25 + 17)

table(test$responsive)
215 / (215 + 42)

library(ROCR)
predRocr = prediction(pred.prob, test$responsive)
perfRocr = performance(predRocr, 'tpr', 'fpr')
plot(perfRocr, colorize = TRUE)
performance(predRocr, 'auc')@y.values
