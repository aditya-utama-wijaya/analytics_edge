parole = read.csv('4-parole.csv')

parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# what fraction of parole violators are female?
table(parole$violator == 1, parole$male == 0)
14 / (14 + 64)

# which crime is the most common in Kentucky?
table(parole$state == 2, parole$crime)

# create a histogram to find out the distribution of the age of parolees
library(ggplot2)
ggplot(parole, aes(x = age)) + geom_histogram()

# by default, geom_histogram divides the data into 30 bins
# change the width of the bins to 5 years
ggplot(parole, aes(x = age)) + geom_histogram(binwidth = 5)

# change the outline color of the bars
ggplot(parole, aes(x = age)) + geom_histogram(binwidth = 5, color = 'blue')

# see how the age distribution of male parolees compares to the age distribution of female parolees
ggplot(parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

# put the histograms side-by-side instead of on top of each other
ggplot(parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(. ~ male)

# produce a histogram where data points are colored by group
ggplot(parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)

# define our own color palette
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# generate the histogram again, using colorPalette
ggplot(parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values = colorPalette)

# tell ggplot not to stack the histograms and make the bars semi-transparent so we can see both colors
ggplot(parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position = 'identity', alpha = .5) + scale_fill_manual(values = colorPalette)

# create a basic histogram with time.served on the x-axis
ggplot(parole, aes(x = time.served)) + geom_histogram(binwidth = 1)
ggplot(parole, aes(x = time.served)) + geom_histogram(binwidth = .1)

# create a histogram of time.served for each value of the variable crime
ggplot(parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(crime ~ .)

# instead of faceting the histograms, overlay them
ggplot(parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1, position = 'identity', alpha = .5)
# with 4 different groups, it can be hard to tell them apart when they are overlayed
