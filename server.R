library(shiny)
library(plotly)
library(e1071)
library(caret)
library("RColorBrewer")
source("C:/Users/GG/Desktop/Memoire/myrep/ErrorMeasures.R")

#Data extraction
df <- read.csv('histoCAC40_2Y.CSV')
df$date <- as.Date(df$date)
df <- df[order(as.Date(df$date, format="%y/%m/%d")),]
df$target <- df$lastPrice

volume <- read.csv(paste(dataDirectory, 'cac40_dataset_volume.CSV', sep=""), sep = "")
sigma_4 <- read.csv(paste(dataDirectory, 'cac40_6y_dataset_sigma_4.csv', sep=""), sep = "")
rv_50 <- read.csv(paste(dataDirectory, 'cac40_dataset_rv_50.csv', sep=""), sep = "")
ccreturn <- read.csv(paste(dataDirectory, 'cac40_dataset_ccreturn.csv', sep=""), sep = "")
volume$date <- as.Date(volume$Index)
sigma_4$date <- as.Date(sigma_4$Index)
rv_50$date <- as.Date(rv_50$Index)
ccreturn$date <- as.Date(ccreturn$Index)
names(volume) <- gsub("X", "", names(volume))
names(sigma_4) <- gsub("X", "", names(sigma_4))
names(rv_50) <- gsub("X", "", names(rv_50))
names(ccreturn) <- gsub("X", "", names(ccreturn))


getColors <- function(nb){
  colors <- colorRampPalette(brewer.pal(8,"Dark2"))(max(5, nb))
  # colors <- substr(colors, 1, nchar(colors)-2)
  colors
}

getString <- function(costs, gammas, MASES){
  colors <- getColors(length(costs))
  res <- ""
  for (i in 1:length(costs)){
    res <- paste(res, "<br/>SVM :  <br/>", 
                 tags$span(style=paste("color:", colors[i]), "cost = ", 2^costs[i]), 
                 "&emsp;&emsp;", 
                 tags$span(style=paste("color:", colors[i]), "gamma = ", 2^gammas[i]),
                 "<br/>", 
                 tags$span(style=paste("color:", colors[i]), "MASE = ", MASES[i]))
  }
  res
}


shinyServer(function(input, output, session) {

  window <- reactive({input$rwindow})
  horizon <- reactive({input$horizon})
  trainingcut <- reactive({input$trainingcut})
  cost <- reactive({2^input$cost})
  gamma <- reactive({2^input$gamma})
  days <- reactive({input$days})

  # Send dataset choices to the ui
  updateSelectInput(session, "market",
                    choices = names(volume)[-c(1, length(volume))],
                    selected = head(names(volume)[-c(1)], 1)
  )
  updateSelectInput(session, "dataset",
                    choices = c("volume", "sigma_4", "rv_50", "ccreturn"),
                    selected = "volume"
  )

  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(getDf()$target)
  })

  # Gets data with the chosen id
  getDf <- function(){
    if (input$dataset == "volume"){
        df <- volume[c("date", input$market)]
    } else if (input$dataset == "sigma_4"){
        df <- sigma_4[c("date", input$market)]
    } else if (input$dataset == "rv_50"){
        df <- rv_50[c("date", input$market)]
    } else if (input$dataset == "ccreturn"){
        df <- ccreturn[c("date", input$market)]
    }
    names(df) <- c("date", "target")
    head(tail(df, input$days[2]), input$days[2] - input$days[1])
  }

  getTestDf <- reactive({
    tail(getDf(), trainingcut())
  })

  getTrainDf <- reactive({
    head(getDf(), -trainingcut())
  })

  getSVM <- function(cost, gamma){
    model <- svm(target ~ date, gamma = 2^gamma, cost = 2^cost, data = getTrainDf())
    predict(model, getTestDf())
  }

  params <- reactiveValues(costs = NULL, gammas = NULL, MASES = NULL)

  observeEvent(input$addModel, {
    params$costs <- c(params$costs, input$cost)
    params$gammas <- c(params$gammas, input$gamma)
    predictedY <- getSVM(input$cost, input$gamma)
    params$MASES <- c(params$MASES, MASE(getTestDf()$target, predictedY))
  })

  observeEvent(input$resetModels, {
    params$costs <- NULL
    params$gammas <- NULL
    params$MASES <- NULL
  })

  output$modelList <- renderText({ 
    if (length(params$gammas) > 0){
      HTML(getString(params$costs, params$gammas, params$MASES))
    }
  })

  output$dataPlot <- renderPlotly({
    
    if (input$market != ""){
      df <- getDf()

      plot_ly(y = df$target, x = df$date, type = 'scatter', mode = 'lines', name = 'Target') %>%

        add_trace(x = head(getTestDf()$date, 1), y = c(min(df$target), max(df$target)),
                mode = 'lines', line = list(dash = "dash"), name = 'Training set cut') %>%

        layout(title = paste("Market : ", input$market, sep = ""))
    }
    else{
      # Returns a default empty plot while data isn't available
      plot_ly(data.frame(Date = as.Date(character())), type = 'scatter', mode = 'lines')
    }
  })

  output$predPlot <- renderPlotly({
    
    if (input$market != ""){
      df <- getDf()

      # Initial plot with legends
      p <- plot_ly(y = df$target, x = df$date, type = 'scatter', mode = 'lines', name = 'Target')
      p <- p %>% layout(title = paste("Out-of-sample forecast of market : ", input$market, sep = ""))

      # Training set vertical cut
      p <- p %>% add_trace(x = head(getTestDf()$date, 1), y = c(min(df$target), max(df$target)),
                           mode = 'lines', line = list(dash = "dash"), name = 'Training set cut')

      nbModels <- length(params$costs)
      if (nbModels > 0){
        for (i in 1:nbModels){
          colors <- getColors(nbModels)
          predictedY <- getSVM(params$costs[i], params$gammas[i])
          p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                               mode = 'lines', name = paste('SVM model : ', i, sep = ''))
        }
      }

      if (input$applySVM){
        predictedY <- svmForecast(getDf(), trainingcut(), window())
        p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                             mode = 'lines', name = 'SVM forecast')
      } 

      if (input$applyNaiveModel){
        predictedY <- naiveForecast()
        p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                             mode = 'lines', name = 'Naive model')
      }
    }
    else{
      # Returns a default empty plot while data isn't available
      p <- plot_ly(data.frame(Date=as.Date(character())), type = 'scatter', mode = 'lines')
    }
    p
  })

  output$predTable <- renderTable(rownames = TRUE, bordered = TRUE, {
    svmPredictedY <- svmForecast(getDf(), trainingcut(), window())
    naivePredictedY <- naiveForecast()
    data.frame(MSE   = c(  MSE(getTestDf()$target, svmPredictedY),   MSE(getTestDf()$target, naivePredictedY)),
               RMSE  = c( RMSE(getTestDf()$target, svmPredictedY),  RMSE(getTestDf()$target, naivePredictedY)),
               MAE   = c(  MAE(getTestDf()$target, svmPredictedY),   MAE(getTestDf()$target, naivePredictedY)),
               MAPE  = c( MAPE(getTestDf()$target, svmPredictedY),  MAPE(getTestDf()$target, naivePredictedY)),
               sMAPE = c(sMAPE(getTestDf()$target, svmPredictedY), sMAPE(getTestDf()$target, naivePredictedY)),
               MASE  = c( MASE(getTestDf()$target, svmPredictedY),  MASE(getTestDf()$target, naivePredictedY)),
               NMSE  = c( NMSE(getTestDf()$target, svmPredictedY),  NMSE(getTestDf()$target, naivePredictedY)),
               NNMSE = c(NNMSE(getTestDf()$target, svmPredictedY, naivePredictedY), 1.0),
               row.names = c("SVM Errors", "Naive Errors"))
  })

  output$predErrorPlot <- renderPlotly({
    df <- getDf()
    MSEs <- c()
    minimumTrainingSize <- 2 * window() + horizon()
    for (i in seq(minimumTrainingSize, nrow(df), horizon())){
      horizon <- min(horizon(), nrow(df) - i)
      if (horizon > 0){
        predictedY <- svmForecast(head(df, i), horizon, window())
        MSEs <- c(MSEs, MSE(df[(i-horizon+1):i, ]$target, predictedY))
      }
    }
    plot_ly(y = MSEs, type = 'scatter', mode = 'lines', name = 'Error')%>%
      layout(title = "Evolution of MSE", showlegend = TRUE)
  })

  svmForecast <- function(df, horizon, window){
    trainDf <- head(df, -horizon)
    testDf <- tail(df, horizon)
    datestoPredict <- testDf["date"]
    
    if (input$strategy == "direct"){
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
        
        models[[k]] <- svm(t_0 ~ ., data = dd)
      }
      
      predictedY <- c()
      for(i in seq_along(1:nrow(datestoPredict))){
        rolling_window <- subset(traindd[(nrow(traindd) - horizon + i),], select = -c(t_0))

        pred <- predict(models[[i]], rolling_window)
        predictedY <- c(predictedY, pred)
      }
    }

    else if (input$strategy == "recursive"){
      embedf <- cbind(tail(df$date, - (window)), as.data.frame(embed(df$target, window + 1)))
      names(embedf) <- c("date", paste("t_", c(0:(window)), sep=''))

      traindd <- head(embedf, - horizon)

      model <- svm(t_0 ~ ., data = traindd)

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

  naiveForecast <- function(){
    # Moving average forecast - Average of the last data according to a certain embedding e
    e <- 30
    rep(mean(tail(getTrainDf()$target, e)), nrow(getTestDf()))
  }
})