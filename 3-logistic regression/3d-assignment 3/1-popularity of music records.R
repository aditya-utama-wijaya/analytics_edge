songs = read.csv('1-songs.csv')

# How many songs are from the year 2010?
table(songs$year)

# How many songs does the dataset include for which the artist name is 'Michael Jackson'?
MichaelJackson = subset(songs, artistname == 'Michael Jackson')
nrow(MichaelJackson)

# Which songs by Michael Jackson made it to the Top 10?
MichaelJackson[c('songtitle', 'Top10')]

# What are the values of the variable corresponding to the estimated time signature that occur in the dataset?
# Which timesignature value is the most frequent among songs in out dataset?
table(songs$timesignature)

# Out of all of the songs in the dataset, which one is the song with the highest tempo?
which.max(songs$tempo)
songs$songtitle[6206]

# Split the data into a training set and a testing set
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)

# How many songs are in the training set?
nrow(SongsTrain)

# Define a vector of variable names that we won't use in our model
nonvars = c('year', 'songtitle', 'artistname', 'songID', 'artistID')

# Remove those variables from the training and testing sets
SongsTrain = SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest = SongsTest[,!(names(SongsTest) %in% nonvars)]

SongsLog1 = glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)

# What is the correlation between the variables 'loudness' and 'energy' in the training set?
cor(SongsTrain$loudness, SongsTrain$energy)

# Create a model without the independent variable 'loudness'
SongsLog2 = glm(Top10 ~ . - loudness, data = SongsTrain, family = binomial)
summary(SongsLog2)

# Create a model without the independent variable 'energy'
SongsLog3 = glm(Top10 ~ . - energy, data = SongsTrain, family = binomial)
summary(SongsLog3)

# Make predictions on the test set using model 3
testPredict = predict(SongsLog3, newdata = SongsTest, type = 'response')

# What is the accuracy of model 3 on the test set, using a threshold of 0.45?
table(SongsTest$Top10, testPredict >= 0.45)
(309+19)/(309+5+40+19)

# What would the accuracy of the baseline model be on the test set?
table(SongsTest$Top10)
314/(314+59)

# How many songs does model 3 correctly predict as Top 10 hits in 2010?
# How many non-hit songs does model 3 predict will be Top 10 hits?
table(SongsTest$Top10, testPredict >= 0.45)

# What is the sensitivity of model 3 on the test set?
19/(19+40)

# What is the specificity of model 3 on the test set?
309/(309+5)

