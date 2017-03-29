svmForecast <- function(df, horizon, window, gamma = 1/window, cost = 1, strategy = "recursive"){
  # By default, most libraries set gamma as 1/(number of features)

  trainDf <- head(df, -horizon)
  testDf <- tail(df, horizon)
  datestoPredict <- testDf["date"]
  
  if (strategy == "direct"){
    embedf <- cbind(tail(df$date, - window), as.data.frame(embed(df$target, window + 1)))
    names(embedf) <- c("date", paste("t_", c(0:(window)), sep=''))
    
    traindd <- head(embedf, - horizon)
    
    models <- vector("list", horizon)
    
    for (k in 1:horizon){
      dd <- head(traindd, -(horizon - k))
      if (horizon - k == 0){
        dd <- traindd
      }
      dd$t_0 <- tail(trainDf$target, - (horizon + window - k))
      
      models[[k]] <- svm(t_0 ~ ., gamma = gamma, cost = cost, data = dd)
    }
    
    predictedY <- c()
    for(i in seq_along(1:nrow(datestoPredict))){
      rolling_window <- subset(traindd[(nrow(traindd) - horizon + i),], select = -c(t_0))
      
      pred <- predict(models[[i]], rolling_window)
      predictedY <- c(predictedY, pred)
    }
  }
  
  else if (strategy == "recursive"){
    embedf <- cbind(tail(df$date, - (window)), as.data.frame(embed(df$target, window + 1)))
    names(embedf) <- c("date", paste("t_", c(0:(window)), sep=''))
    
    traindd <- head(embedf, - horizon)
    
    model <- svm(t_0 ~ ., gamma = gamma, cost = cost, data = traindd)
    
    rolling_window <- merge(as.data.frame(embed(tail(trainDf, window)$target, window)), 
                            data.frame(date = ""))
    names(rolling_window) <- c(paste("t_", c(1:(window)), sep=''), "date")
    
    predictedY <- c()
    for(i in seq_along(1:nrow(datestoPredict))){
      rolling_window$date <- datestoPredict[i, ]
      
      pred <- predict(model, rolling_window)
      predictedY <- c(predictedY, pred)
      
      rolling_window[, 1:(window - 1)] <- rolling_window[, 2:window]
      rolling_window[, window] <- pred
    }
  }
  predictedY
}