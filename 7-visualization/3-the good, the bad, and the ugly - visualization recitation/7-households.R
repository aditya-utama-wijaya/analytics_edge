library(ggplot2)
households = read.csv('7-households.csv')
str(households)

library(reshape2)
households[, 1:2]
head(melt(households, id = 'Year'))

households[, 1:3]
melt(households, id = 'Year')[1:10, ]
ggplot(melt(households, id = 'Year'), aes(x = Year, y = value, color = variable)) + geom_line(size = 2) + geom_point(size = 5) + ylab('Percentage of Households')
