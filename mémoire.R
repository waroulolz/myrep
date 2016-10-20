library(e1071)

dataDirectory <- "C:/Users/GG/Desktop/Mémoire/myrep/"
data <- read.csv(paste(dataDirectory, 'histoCAC40_2Y.CSV', sep=""))

df <- data
df$date <- as.Date(df$date)
df = df[df$iid == 1013552,]
df = df[order(as.Date(df$date, format="%y/%m/%d")),]

plot(df$highPrice  ~df$date, type = 'l')

model <- lm(highPrice ~ date, df)
abline(model)

predictedY <- predict(model, df)
points(df$date, predictedY, col = "blue", type='l', lwd=2)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals
predictionRMSE <- rmse(error) 

model <- svm(highPrice ~ date, df)
predictedY <- predict(model, df)
points(df$date, predictedY, col = "red", type='l', lwd=2)

error <- df$highPrice - predictedY
svrPredictionRMSE <- rmse(error)

# perform a grid search
tuneResult <- tune(svm, highPrice ~ date,  data = df,
                   ranges = list(epsilon = seq(0.1, 0.3, 0.1), cost = 2^(2:4)))
print(tuneResult)
plot(tuneResult)


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, df) 

error <- df$highPrice - tunedModelY  
tunedModelRMSE <- rmse(error)

points(df$date, tunedModelY, col = "green", type='l', lwd=2)

lastDay <- df$date[length(df$date)]
forecastRange <- as.Date(seq(as.POSIXct(as.Date(lastDay)+1), as.POSIXct(as.Date(lastDay)+30), "day"))

pred <- predict(tunedModel, newdata = data.frame(date=forecastRange))
points(pred~forecastRange, col=6, type='l', lwd=2)
