baseball = read.csv('4-baseball.csv')

# How many team/year pairs are there in the whole dataset?
nrow(baseball)

# Identify the total number of years included in the dataset
length(table(baseball$Year))

# Build a data frame limited to teams that made the playoffs
baseball = subset(baseball, Playoffs == 1)

# How many team/year pairs are included in the new dataset?
nrow(baseball)

# Which has been the number of teams making the playoffs in some season?
table(table(baseball$Year))

# Store the output that counts the number of playoff teams from each year
PlayoffTable = table(baseball$Year)

# Vector of years stored as strings (type chr)
str(names(PlayoffTable))

# What function call returns the number of playoff teams in 1990 and 2001?
PlayoffTable[c('1990', '2001')]

# Look up the number of teams in the playoffs for each team/year pair in the dataset, and store it as a new ariable in the baseball data frame
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

# How many playoff team/year pairs are there in the dataset from years where 8 teams were invited to the playoffs?
table(baseball$NumCompetitors)

# Add a variable to the data frame which takes value 1 if a team won the World Series in the indicated year and a 0 otherwise
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)

# How many observations do we have in the dataset where a team did not win the World Series?
table(baseball$WorldSeries)

# Which variables is a significant predictor of the WorldSeries variable in a bivariate logistic regression model?
summary(glm(WorldSeries ~ Year, data = baseball, family = binomial))
summary(glm(WorldSeries ~ RA, data = baseball, family = binomial))
summary(glm(WorldSeries ~ RankSeason, data = baseball, family = binomial))
summary(glm(WorldSeries ~ NumCompetitors, data = baseball, family = binomial))

# Build a model using all of the variables that are significant in the bivariate models
LogModel = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = binomial)

# How many variables are significant in the combined model?
summary(LogModel)

# Often, variables that were significant in bivariate models are no longer significant in multivariate analysis due to correlation between the variables
# Which variable pairs have a high degree of correlation? (|cor| > 0.8)
cor(baseball[c('Year', 'RA', 'RankSeason', 'NumCompetitors')])

# Build all 6 of the 2-variable models
# Which model has the best AIC value?
summary(glm(WorldSeries ~ Year + RA, data = baseball, family = binomial))
summary(glm(WorldSeries ~ Year + RankSeason, data = baseball, family = binomial))
summary(glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = binomial))
summary(glm(WorldSeries ~ RA + RankSeason, data = baseball, family = binomial))
summary(glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = binomial))
summary(glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = binomial))
