mvt = read.csv('3-mvt.csv', stringsAsFactors = FALSE)
str(mvt)

mvt$Date = strptime(mvt$Date, format = '%m/%d/%y %H:%M')
mvt$weekday = weekdays(mvt$Date)
mvt$hour = mvt$Date$hour
str(mvt)

table(mvt$weekday)
weekdayCounts = as.data.frame(table(mvt$weekday))
str(weekdayCounts)

library(ggplot2)
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))

weekdayCounts$Var1 = factor(weekdayCounts$Var1, ordered = TRUE, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype = 2)
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), alpha = .3)

ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1)) + xlab('Day of the Week') + ylab('Total Motor Vehicle Thefts')

#4
table(mvt$weekday, mvt$hour)
dayHourCounts = as.data.frame(table(mvt$weekday, mvt$hour))
str(dayHourCounts)
dayHourCounts$hour = as.numeric(as.character(dayHourCounts$Var2))

ggplot(dayHourCounts, aes(x = hour, y = Freq)) + geom_line(aes(group = Var1))
ggplot(dayHourCounts, aes(x = hour, y = Freq)) + geom_line(aes(group = Var1, color = Var1), size = 2)

dayHourCounts$Var1 = factor(dayHourCounts$Var1, ordered = TRUE, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
ggplot(dayHourCounts, aes(x = hour, y = Var1)) + geom_tile(aes(fill = Freq))
ggplot(dayHourCounts, aes(x = hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total MV Thefts') + theme(axis.title.y = element_blank())
ggplot(dayHourCounts, aes(x = hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total MV Thefts', low = 'white', high = 'red') + theme(axis.title.y = element_blank())

#5
install.packages('maps')
library(maps)
install.packages('ggmap')
library(ggmap)
chicago = get_map(location = 'chicago', zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

latLonCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(latLonCounts)
latLonCounts$long = as.numeric(as.character(latLonCounts$Var1))
latLonCounts$lat = as.numeric(as.character(latLonCounts$Var2))
ggmap(chicago) + geom_point(data = latLonCounts, aes(x = long, y = lat, color = Freq, size = Freq))
ggmap(chicago) + geom_point(data = latLonCounts, aes(x = long, y = lat, color = Freq, size = Freq)) + scale_color_gradient(low = 'yellow', high = 'red')

ggmap(chicago) + geom_tile(data = latLonCounts, aes(x = long, y = lat, alpha = Freq), fill = 'red')

# the heatmap was plotting squares out in the water
# we can fix this by removing the observations from the data frame that have Freq = 0
latLonCounts2 = subset(latLonCounts, Freq > 0)
ggmap(chicago) + geom_tile(data = latLonCounts2, aes(x = long, y = lat, alpha = Freq), fill = 'red')

# how many observations did we remove?
nrow(latLonCounts) - nrow(latLonCounts2)

#6
murders = read.csv('6-murders.csv')
str(murders)

statesMap = map_data('state')
str(statesMap)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = 'white', color = 'black')

murders$region = tolower(murders$State)

murderMap = merge(statesMap, murders, by = 'region')
str(murderMap)

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = 'black') + scale_fill_gradient(low = 'black', high = 'red', guide = 'legend')

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = 'black') + scale_fill_gradient(low = 'black', high = 'red', guide = 'legend')

murderMap$murderRate = murderMap$Murders / murderMap$Population * 100000
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = murderRate)) + geom_polygon(color = 'black') + scale_fill_gradient(low = 'black', high = 'red', guide = 'legend')

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = murderRate)) + geom_polygon(color = 'black') + scale_fill_gradient(low = 'black', high = 'red', guide = 'legend', limits = c(0, 10))

# which state has the highest gun ownership rate?
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = 'black') + scale_fill_gradient(low = 'black', high = 'red', guide = 'legend')
