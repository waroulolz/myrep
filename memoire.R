library(e1071)
source("C:/Users/GG/Desktop/Memoire/myrep/ErrorMeasures.R")

#Data extraction
dataDirectory <- "C:/Users/GG/Desktop/Memoire/myrep/"
data <- read.csv(paste(dataDirectory, 'histoCAC40_2Y.CSV', sep=""))

#Select a market iid and order by dates
df <- data
df$date <- as.Date(df$date)
df <- df[df$iid == 1013552,]
df <- df[order(as.Date(df$date, format="%y/%m/%d")),]


df$rendement <- c(0,log(df$lastPrice[2:nrow(df)] / df$lastPrice[1:nrow(df)-1]))

#Store target variable to forecast as target
df$target <- df$lastPrice

plot(target ~ date, data = df, type = 'l')

model <- svm(target ~ date, df)
predictedY <- predict(model, df)
points(df$date, predictedY, col = "red", type='l', lwd=2)


# perform a grid search
tuneResult <- tune(svm, target ~ date,  data = df,
                   ranges = list(gamma = 2^(-2:-1), cost = 2^(2:4))
                   , tunecontrol = tune.control(cross=10))
print(tuneResult)
#plot(tuneResult)


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, df) 

points(df$date, tunedModelY, col = "green", type='l', lwd=2)

lastDay <- df$date[length(df$date)]
forecastRange <- as.Date(seq(lastDay+1, lastDay+30, "day"))

pred <- predict(tunedModel, newdata = data.frame(date=forecastRange))
points(pred~forecastRange, col=6, type='l', lwd=2)


library(plotly)
plot_ly(y = df$target, x = df$date, type = 'scatter', mode = 'lines', name = 'Target') %>%
  add_trace(y = as.numeric(tunedModelY), x = df$date, mode = 'lines', name = 'Predict')






library(caret)
timeSlices <- createTimeSlices(1:nrow(df), 
                               initialWindow = 395, horizon = 30, fixedWindow = TRUE)
trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]



gammas <- 2^(-3:7)
costs  <- 2^(-3:7)
paramBench <- matrix(0, ncol = length(gammas), nrow = length(costs))
rownames(paramBench) <- costs
colnames(paramBench) <- gammas

for (g in gammas){
  for (c in costs){
    models <- vector("list", length(trainSlices))
    preds  <- vector("list", length(trainSlices))
    errors <- vector("list", length(trainSlices))
    for(i in seq_along(1:length(trainSlices))){
      #models[[i]] <- train(target ~ date, data = df[trainSlices[[i]],], method = "svmLinear")
      models[[i]] <- svm(target ~ date, gamma = g, cost = c, data = df[trainSlices[[i]],])
      preds[[i]] <- predict(models[[i]], df[testSlices[[i]], -9])
      errors[[i]] <- MASE(preds[[i]], df[testSlices[[i]],]$target)
    }
    paramBench[as.character(c), as.character(g)] <- mean(as.numeric(errors))
  }
}
paramBench

bestC <- costs[which(paramBench == min(paramBench), arr.ind = TRUE)[1]]
bestG <- gammas[which(paramBench == min(paramBench), arr.ind = TRUE)[2]]

bestC
bestG

windows <- seq(from = 10, to = 50, by = 10)
horizons <- c(1, seq(from = 5, to = 20, by = 5))
predBench <- matrix(0, ncol = length(windows), nrow = length(horizons))
rownames(predBench) <- horizons
colnames(predBench) <- windows

for (window in windows){
  for (horizon in horizons){
    timeSlices <- createTimeSlices(1:nrow(df), 
                                   initialWindow = window, horizon = horizon, fixedWindow = TRUE)
    trainSlices <- timeSlices[[1]]
    testSlices <- timeSlices[[2]]
    
    models <- vector("list", length(trainSlices))
    preds  <- vector("list", length(trainSlices))
    errors <- vector("list", length(trainSlices))
    
    for(i in seq_along(1:length(trainSlices))){
      #models[[i]] <- train(target ~ date, data = df[trainSlices[[i]],], method = "svmLinear")
      models[[i]] <- svm(target ~ date, gamma = bestG, cost = bestC, data = df[trainSlices[[i]],])
      preds[[i]] <- predict(models[[i]], df[testSlices[[i]], -9])
      errors[[i]] <- MASE(preds[[i]], df[testSlices[[i]],]$target)
    }
    predBench[as.character(horizon), as.character(window)] <- mean(as.numeric(errors))
  }
}
predBench

bestHorizon <- horizons[which(predBench == min(predBench), arr.ind = TRUE)[2]]
bestWindow <- windows[which(predBench == min(predBench), arr.ind = TRUE)[1]]








for(i in seq_along(1:length(trainSlices))){
  plot(df$target~ df$date, col = "red", type = 'l')
  true <- df[testSlices[[i]],]$target
  pred <- preds[[i]]
  points(pred ~ df[testSlices[[i]], 2], col = "blue", type = 'l') 
}



myTimeControl <- trainControl(method = "timeslice", initialWindow = 365, horizon = 30,
                              fixedWindow = TRUE)
svmPred <- train(target ~ date, data = df, method = "svmLinear",
                 preProc = c("center", "scale"), trControl = myTimeControl)
svmPred



library("quantmod")
getSymbols("AAPL",src="yahoo") 
barChart(AAPL)

AAPL <- data.frame(Date = index(AAPL), data = coredata(AAPL))
AAPL["data.AAPL.Adjusted"] <- NULL
names(AAPL) <- c("date", "openingPrice", "highPrice", "lowPrice", "lastPrice", "quantity")
AAPL$rendement <- c(0,log(1.0*AAPL$lastPrice[2:nrow(AAPL)] / AAPL$lastPrice[1:nrow(AAPL)-1]))
df$target <- df$lastPrice



