#Clear environment
rm(list=ls())
library(shiny)
library(plotly)
library(e1071)
library("quantmod")
library(caret)
library("RColorBrewer")
library(gbcode)
source("C:/Users/GG/Desktop/Memoire/myrep/KNNFunctions.R")
source("C:/Users/GG/Desktop/Memoire/myrep/SVM.R")
source("C:/Users/GG/Desktop/Memoire/myrep/config.R")
source("C:/Users/GG/Desktop/Memoire/myrep/ErrorMeasures.R")


extractDf <- function(data){
  data <- data.frame(Date = index(data), data = coredata(data))
  data[ncol(data)] <- NULL
  names(data) <- c("date", "openingPrice", "highPrice", "lowPrice", "lastPrice", "volume")
  data$rendement <- c(0,log(1.0*data$lastPrice[2:nrow(data)] / data$lastPrice[1:nrow(data)-1]))
  data
}

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
  
  #Data extraction
  downloadData <- function(dataName){
    data <- getSymbols(dataName, src="yahoo", to = Sys.Date(), auto.assign = FALSE) 
    data <- extractDf(data)
    data
  }
  getData <- reactive({invalidateLater(1000 *  60 * 60, session) # Refresh data each hour
                      downloadData(markets[[input$market]])})
  
  #Parameters getters
  window <- reactive({input$rwindow})
  horizon <- reactive({input$horizon})
  trainingcut <- reactive({input$trainingcut})
  cost <- reactive({2^input$cost})
  gamma <- reactive({2^input$gamma})
  days <- reactive({input$days})

  # Send dataset choices to the ui
  updateSelectInput(session, "market",
                    choices = names(markets),
                    selected = names(markets)[[1]]
  )
  updateSelectInput(session, "dataset",
                    choices = c("openingPrice", "highPrice", "lowPrice", "lastPrice", "volume"),
                    selected = "lastPrice"
  )

  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(getDf()$target)
  })

  # Gets the correct dataframe according to user's parameters
  getDf <- function(){
    df <- getData()

    if (input$dataset == "lastPrice"){
      df <- df[c("date", "lastPrice")]
    } else if (input$dataset == "volume"){
      df <- df[c("date", "volume")]
    } else if (input$dataset == "lowPrice"){
      df <- df[c("date", "lowPrice")]
    } else if (input$dataset == "highPrice"){
      df <- df[c("date", "highPrice")]
    } else if (input$dataset == "openingPrice"){
      df <- df[c("date", "openingPrice")]
    }

    names(df) <- c("date", "target")
    df[(df$date > days()[1]) & (df$date < days()[2]),]
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
        predictedY <- svmForecast(getDf(), trainingcut(), window(), gamma(), cost(), input$strategy)
        p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                             mode = 'lines', name = paste('SVM (G=', gamma(), ', C=', cost(), ')', sep = ''))
      } 

      # Add default semi-optimized svm
      predictedY <- svmForecast(getDf(), trainingcut(), window(), strategy = input$strategy)
      p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                           mode = 'lines', name = paste('SVM default (G=', round(1/window(), 3), ', C=', 1, ')', sep = ''))
      
      if (input$applyKnnModel){
        predictedY <- modelSelectionKNN(zoo(getDf()[, 2],  order.by = getDf()$date), trainingcut(), round(2*nrow(getDf())/3),
                                        window(), type = "direct", error_measure = MASE)$forecasts
        p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                             mode = 'lines', name = "KNN model")
      }
    
      if (input$applyNaiveModel){
        predictedY <- naiveForecast(trainingcut())
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
    svmPredY <- svmForecast(getDf(), trainingcut(), window(), gamma(), cost(), input$strategy)
    defaultSvmPredY <- svmForecast(getDf(), trainingcut(), window(), strategy = input$strategy)
    knnPredY <- modelSelectionKNN(zoo(getDf()[, 2],  order.by = getDf()$date), trainingcut(), round(2*nrow(getDf())/3),
                                    window(), type = "direct", error_measure = MASE)$forecasts
    naivePredY <- naiveForecast(trainingcut())
    data.frame(MSE   = c(  MSE(getTestDf()$target, svmPredY),   MSE(getTestDf()$target, defaultSvmPredY),   MSE(getTestDf()$target, knnPredY),   MSE(getTestDf()$target, naivePredY)),
               RMSE  = c( RMSE(getTestDf()$target, svmPredY),  RMSE(getTestDf()$target, defaultSvmPredY),  RMSE(getTestDf()$target, knnPredY),  RMSE(getTestDf()$target, naivePredY)),
               MAE   = c(  MAE(getTestDf()$target, svmPredY),   MAE(getTestDf()$target, defaultSvmPredY),   MAE(getTestDf()$target, knnPredY),   MAE(getTestDf()$target, naivePredY)),
               MAPE  = c( MAPE(getTestDf()$target, svmPredY),  MAPE(getTestDf()$target, defaultSvmPredY),  MAPE(getTestDf()$target, knnPredY),  MAPE(getTestDf()$target, naivePredY)),
               sMAPE = c(sMAPE(getTestDf()$target, svmPredY), sMAPE(getTestDf()$target, defaultSvmPredY), sMAPE(getTestDf()$target, knnPredY), sMAPE(getTestDf()$target, naivePredY)),
               MASE  = c( MASE(getTestDf()$target, svmPredY),  MASE(getTestDf()$target, defaultSvmPredY),  MASE(getTestDf()$target, knnPredY),  MASE(getTestDf()$target, naivePredY)),
               NMSE  = c( NMSE(getTestDf()$target, svmPredY),  NMSE(getTestDf()$target, defaultSvmPredY),  NMSE(getTestDf()$target, knnPredY),  NMSE(getTestDf()$target, naivePredY)),
               NNMSE = c(NNMSE(getTestDf()$target, svmPredY, naivePredY), NNMSE(getTestDf()$target, defaultSvmPredY, naivePredY), NNMSE(getTestDf()$target, knnPredY, naivePredY), 1.0),
               row.names = c("SVM Errors", "SVM Default Errors", "KNN Errors", "Naive Errors"))
  })

  
  output$errorTable <- renderTable(rownames = TRUE, bordered = TRUE, {
    
    df <- getDf()
    MSEs   <- c()
    RMSEs  <- c()
    MAEs   <- c()
    MAPEs  <- c()
    sMAPEs <- c()
    MASEs  <- c()
    NMSEs  <- c()
    NNMSEs <- c()
    
    minimumTrainingSize <- round(2*nrow(df)/3)
    for (i in seq(minimumTrainingSize, nrow(df) - horizon(), horizon())){
      horizon <- min(horizon(), nrow(df) - i)
      if (horizon > 0){
        svmPredY <- svmForecast(head(df, i), horizon, window(), gamma(), cost(), input$strategy)
        defaultSvmPredY <- svmForecast(head(df, i), horizon, window(), strategy = input$strategy)
        knnPredY <- modelSelectionKNN(zoo(head(df, i)[, 2],  order.by = getDf()$date), horizon, round(2*nrow(getDf())/3),
                                      window(), type = "direct", error_measure = MASE)$forecasts
        naivePredY <- naiveForecast(horizon)
        
        MSEs <- c(MSEs,      MSE(df[(i-horizon+1):i, ]$target, svmPredY),   MSE(df[(i-horizon+1):i, ]$target, defaultSvmPredY),
                             MSE(df[(i-horizon+1):i, ]$target, knnPredY),   MSE(df[(i-horizon+1):i, ]$target, naivePredY))
        RMSEs <- c(RMSEs,   RMSE(df[(i-horizon+1):i, ]$target, svmPredY),  RMSE(df[(i-horizon+1):i, ]$target, defaultSvmPredY),
                            RMSE(df[(i-horizon+1):i, ]$target, knnPredY),  RMSE(df[(i-horizon+1):i, ]$target, naivePredY))
        MAEs <- c(MAEs,      MAE(df[(i-horizon+1):i, ]$target, svmPredY),   MAE(df[(i-horizon+1):i, ]$target, defaultSvmPredY),
                             MAE(df[(i-horizon+1):i, ]$target, knnPredY),   MAE(df[(i-horizon+1):i, ]$target, naivePredY))
        MAPEs <- c(MAPEs,   MAPE(df[(i-horizon+1):i, ]$target, svmPredY),  MAPE(df[(i-horizon+1):i, ]$target, defaultSvmPredY),
                            MAPE(df[(i-horizon+1):i, ]$target, knnPredY),  MAPE(df[(i-horizon+1):i, ]$target, naivePredY))
        sMAPEs <- c(sMAPEs,sMAPE(df[(i-horizon+1):i, ]$target, svmPredY), sMAPE(df[(i-horizon+1):i, ]$target, defaultSvmPredY),
                           sMAPE(df[(i-horizon+1):i, ]$target, knnPredY), sMAPE(df[(i-horizon+1):i, ]$target, naivePredY))
        MASEs <- c(MASEs,   MASE(df[(i-horizon+1):i, ]$target, svmPredY),  MASE(df[(i-horizon+1):i, ]$target, defaultSvmPredY),
                            MASE(df[(i-horizon+1):i, ]$target, knnPredY),  MASE(df[(i-horizon+1):i, ]$target, naivePredY))
        NMSEs <- c(NMSEs,   NMSE(df[(i-horizon+1):i, ]$target, svmPredY),  NMSE(df[(i-horizon+1):i, ]$target, defaultSvmPredY),
                            NMSE(df[(i-horizon+1):i, ]$target, knnPredY),  NMSE(df[(i-horizon+1):i, ]$target, naivePredY))
        NNMSEs <- c(NNMSEs,NNMSE(df[(i-horizon+1):i, ]$target, svmPredY, naivePredY),
                           NNMSE(df[(i-horizon+1):i, ]$target, defaultSvmPredY, naivePredY),
                           NNMSE(df[(i-horizon+1):i, ]$target, knnPredY, naivePredY),
                           1.0)

      }
    }

    data.frame(MSE   = apply(matrix(  MSEs, ncol = 4, byrow = TRUE), 2, mean),
               RMSE  = apply(matrix( RMSEs, ncol = 4, byrow = TRUE), 2, mean),
               MAE   = apply(matrix(  MAEs, ncol = 4, byrow = TRUE), 2, mean),
               MAPE  = apply(matrix( MAPEs, ncol = 4, byrow = TRUE), 2, mean),
               sMAPE = apply(matrix(sMAPEs, ncol = 4, byrow = TRUE), 2, mean),
               MASE  = apply(matrix( MASEs, ncol = 4, byrow = TRUE), 2, mean),
               NMSE  = apply(matrix( NMSEs, ncol = 4, byrow = TRUE), 2, mean),
               NNMSE = apply(matrix(NNMSEs, ncol = 4, byrow = TRUE), 2, mean),
               row.names = c("SVM Errors", "SVM Default Errors", "KNN Errors", "Naive Errors"))
  })
  

  naiveForecast <- function(horizon){
    # Moving average forecast - Average of the last data according to a certain embedding e
    e <- 30
    rep(mean(tail(getTrainDf()$target, e)), horizon)
  }
})