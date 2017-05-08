getwd()
mvt = read.csv('1-mvtWeek1.csv')
nrow(mvt)
str(mvt)

max(mvt$ID)
summary(mvt$Beat)
summary(mvt$Arrest)
table(mvt$LocationDescription)

mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, '%m/%d/%y %H:%M'))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
table(mvt$Weekday)
table(mvt$Arrest, mvt$Month)

hist(mvt$Date, breaks = 100)

boxplot(mvt$Date ~ mvt$Arrest)

table(mvt$Arrest, mvt$Year)

sort(table(mvt$LocationDescription))
TopLocations = c('STREET', 'PARKING LOT/GARAGE(NON.RESID.)', 'ALLEY', 'GAS STATION', 'DRIVEWAY - RESIDENTIAL')
Top5 = subset(mvt, LocationDescription %in% TopLocations)
str(Top5)

table(Top5$LocationDescription)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)

table(Top5$LocationDescription, Top5$Weekday)
