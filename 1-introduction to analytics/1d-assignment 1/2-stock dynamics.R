IBM = read.csv('2a-IBMStock.csv')
GE = read.csv('2b-GEStock.csv')
ProcterGamble = read.csv('2c-ProcterGambleStock.csv')
CocaCola = read.csv('2d-CocaColaStock.csv')
Boeing = read.csv('2e-BoeingStock.csv')

IBM$Date = as.Date(IBM$Date, '%m/%d/%y')
GE$Date = as.Date(GE$Date, '%m/%d/%y')
ProcterGamble$Date = as.Date(ProcterGamble$Date, '%m/%d/%y')
CocaCola$Date = as.Date(CocaCola$Date, '%m/%d/%y')
Boeing$Date = as.Date(Boeing$Date, '%m/%d/%y')

str(IBM)
str(GE)
str(ProcterGamble)
str(CocaCola)
str(Boeing)

summary(IBM$Date)
summary(IBM)

summary(GE)

summary(CocaCola)

summary(Boeing)

sd(ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type = 'l')
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type = 'l', col = 'red')
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'blue')

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type = 'l', col = 'red', ylim = c(0, 210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = 'green')
lines(GE$Date[301:432], GE$StockPrice[301:432], col = 'purple')
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = 'blue')
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = 'orange')
abline(v = as.Date(c('1997-09-01')), lwd = 2)
abline(v = as.Date(c('1997-11-01')), lwd = 2)

mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)

tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
