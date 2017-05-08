# when communicatiing information to the general public, a visualization is much easier to absorb than a table of numbers would be
# visualizations can easily be used to present data

# the geometric objects for scatterplot are points, and for histogram are bars

# 4
who = read.csv('4-WHO.csv')
str(who)

plot(who$GNI, who$FertilityRate)

install.packages('ggplot2')
library(ggplot2)
scatterplot = ggplot(who, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point(color = 'blue', size = 3, shape = 17)
scatterplot + geom_point(color = 'darkred', size = 3, shape = 8)
scatterplot + geom_point(color = 'darkred', size = 3, shape = 8) + ggtitle('Fertility Rate vs. Gross National Income')

fertilityGNIplot = scatterplot + geom_point(color = 'darkred', size = 3, shape = 8) + ggtitle('Fertility Rate vs. Gross National Income')
pdf('4-myPlot.pdf')
print(fertilityGNIplot)
dev.off()

#5
ggplot(who, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()
ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()
model = lm(Under15 ~ log(FertilityRate), data = who)
summary(model)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm')
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', level = .99)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', se = FALSE)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', se = FALSE, color = 'orange')
