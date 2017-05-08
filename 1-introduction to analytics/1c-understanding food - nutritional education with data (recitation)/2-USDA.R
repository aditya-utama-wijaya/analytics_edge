getwd()
USDA = read.csv('2-USDA.csv')
str(USDA)
summary(USDA)

USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]

HighSodium = subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description

match('CAVIAR', USDA$Description)
USDA$Sodium[4154]
USDA$Sodium[match('CAVIAR', USDA$Description)]

summary(USDA$Sodium)
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm = TRUE)

plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, xlab = 'Protein', ylab = 'Fat', main = 'Protein vs. Fat', col = 'red')

hist(USDA$VitaminC, xlab = 'Vitamin C (mg)', main = 'Histogram of Vitamin C Levels')
hist(USDA$VitaminC, xlab = 'Vitamin C (mg)', main = 'Histogram of Vitamin C Levels', xlim = c(0, 100))
hist(USDA$VitaminC, xlab = 'Vitamin C (mg)', main = 'Histogram of Vitamin C Levels', xlim = c(0, 100), breaks = 100)
hist(USDA$VitaminC, xlab = 'Vitamin C (mg)', main = 'Histogram of Vitamin C Levels', xlim = c(0, 100), breaks = 2000)

boxplot(USDA$Sugar, main = 'Boxplot of Sugar Levels')
boxplot(USDA$Sugar, main = 'Boxplot of Sugar Levels', ylab = 'Sugar (g)')

USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)
USDA$Sodium[50] > mean(USDA$Sodium, na.rm = TRUE)
HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE)
str(HighSodium)
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(HighSodium)
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(USDA)
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
str(USDA)
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)

tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm = TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm = TRUE)