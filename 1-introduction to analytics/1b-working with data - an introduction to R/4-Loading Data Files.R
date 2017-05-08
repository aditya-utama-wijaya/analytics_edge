getwd()

WHO = read.csv('4-WHO.csv')
str(WHO)
summary(WHO)

WHO_Europe = subset(WHO, Region == 'Europe')
str(WHO_Europe)
write.csv(WHO_Europe, '4-WHO_Europe.csv')

ls()
rm(WHO_Europe)
ls()

Under15
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)

which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]

plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c('Country', 'GNI', 'FertilityRate')]

hist(WHO$CellularSubscribers)

boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = '', ylab = 'Life Expectancy', main = 'Life Expectancy of Countries by Region')

table(WHO$Region)

tapply(WHO$Over60, WHO$Region, mean)

tapply(WHO$LiteracyRate, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = TRUE)
