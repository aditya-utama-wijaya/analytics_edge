library(ggmap)
intlall = read.csv('5-intlall.csv', stringsAsFactors = FALSE)
head(intlall)

intlall[is.na(intlall)] = 0
head(intlall)

worldMap = map_data('world')
str(worldMap)

worldMap = merge(worldMap, intlall, by.x = 'region', by.y = 'Citizenship')
str(worldMap)

ggplot(worldMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = 'white', color = 'black') + coord_map('mercator')

worldMap = worldMap[order(worldMap$group, worldMap$order),]
ggplot(worldMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill= 'white', color = 'black') + coord_map('mercator')

intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = 'China'
worldMap = merge(map_data('world'), intlall, by.x = 'region', by.y = 'Citizenship')
worldMap = worldMap[order(worldMap$group, worldMap$order),]
ggplot(worldMap, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), color = 'black') + coord_map('mercator')

ggplot(worldMap, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), color = 'black') + coord_map('ortho', orientation = c(20, 30, 0))
