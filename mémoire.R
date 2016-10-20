df <- read.csv("C:/Users/GG/Desktop/Mémoire/histoCAC40_2Y.CSV")
head(df)
plot(df$highPrice, type = 'l')
lines(df$lowPrice, col=2)
