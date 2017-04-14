#Clear environment
rm(list=ls())
library(shiny)
library(plotly)
library(e1071)
library("quantmod")
library(caret)
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

creturn <- function(data){
  cr <- c(0,log(1.0*data[2:length(data)] / data[1:length(data)-1]))
  cr[(is.infinite(cr)) | is.na(cr)] <- 0
  cr
}

volatility <- function(df){
  u <- log(df$highPrice/df$openingPrice)
  d <- log(df$lowPrice /df$openingPrice)
  c <- log(df$lastPrice/df$openingPrice)
  df$target <- 0.511 * (u - d)**2 - (2 * log(2) - 1) * c**2
  df
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
  trainingcut <- reactive({input$trainingcut})
  cost <- reactive({2^input$cost})
  gamma <- reactive({2^input$gamma})
  strategy <- reactive({input$strategy})
  windowErr <- reactive({input$rwindowErr})
  trainingcutErr <- reactive({input$trainingcut})
  costErr <- reactive({2^input$costErr})
  gammaErr <- reactive({2^input$gammaErr})
  strategyErr <- reactive({input$strategyErr})
  horizon <- reactive({input$horizon})
  days <- reactive({input$days})
  preProcess <- reactive({input$preProcess})

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

    if (preProcess() == "volatility"){
      df <- volatility(df)[c("date", "target")]
    } 
    else {
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

      if (preProcess() == "creturn"){
        df$target <- creturn(df$target)
      }
    }
    df[(df$date > days()[1]) & (df$date < days()[2]),]
  }

  getTestDf <- reactive({
    tail(getDf(), trainingcut())
  })

  getTrainDf <- reactive({
    head(getDf(), -trainingcut())
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

      # Normalise data, calculate mean and sd only on training set
      meanTarget <- mean(head(df, -trainingcut())$target) 
      sdTarget <- sd(head(df, -trainingcut())$target)
      
      df$target <- (df$target - meanTarget) / sdTarget
      
      if (input$applySVM){
        predictedY <- svmForecast(df, trainingcut(), window(), gamma(), cost(), input$strategy)
        predictedY <- (predictedY * sdTarget) + meanTarget
        p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                             mode = 'lines', name = paste('SVM (G=', gamma(), ', C=', cost(), ')', sep = ''))
      } 

      # Add default semi-optimized svm
      predictedY <- svmForecast(df, trainingcut(), window(), strategy = input$strategy)
      predictedY <- (predictedY * sdTarget) + meanTarget
      p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                           mode = 'lines', name = paste('SVM default (G=', round(1/window(), 3), ', C=', 1, ')', sep = ''))
      
      if (input$applyKnnModel){
        predictedY <- modelSelectionKNN(df$target, trainingcut(), nrow(df) - trainingcut(),
                                        window(), type = "direct", error_measure = MASE)$forecasts
        predictedY <- (predictedY * sdTarget) + meanTarget
        p <- p %>% add_trace(x = getTestDf()$date, y = predictedY, 
                             mode = 'lines', name = "KNN model")
      }
    
      if (input$applyNaiveModel){
        predictedY <- naiveForecast(df, trainingcut())
        predictedY <- (predictedY * sdTarget) + meanTarget
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
    df <- getDf()
    # Normalise data, calculate mean and sd only on training set
    meanTarget <- mean(head(df, -trainingcut())$target) 
    sdTarget <- sd(head(df, -trainingcut())$target)
    
    df$target <- (df$target - meanTarget) / sdTarget
    
    svmPredY <- svmForecast(df, trainingcut(), window(), gamma(), cost(), strategy())
    defaultSvmPredY <- svmForecast(df, trainingcut(), window(), strategy = strategy())
    knnPredY <- modelSelectionKNN(df$target, trainingcut(), nrow(df) - trainingcut(),
                                    window(), type = "direct", error_measure = MASE)$forecasts
    naivePredY <- naiveForecast(df, trainingcut())

    #Unnormalise data
    svmPredY        <- (svmPredY        * sdTarget) + meanTarget
    defaultSvmPredY <- (defaultSvmPredY * sdTarget) + meanTarget
    knnPredY        <- (knnPredY        * sdTarget) + meanTarget
    naivePredY      <- (naivePredY      * sdTarget) + meanTarget
    
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
        # Normalise data, calculate mean and sd only on training set
        meanTarget <- mean(head(head(df, i), -horizon)$target) 
        sdTarget <- sd(head(head(df, i), -horizon)$target)
        
        df$target <- (df$target - meanTarget) / sdTarget
        
        svmPredY <- svmForecast(head(df, i), horizon, windowErr(), gammaErr(), costErr(), strategyErr())
        defaultSvmPredY <- svmForecast(head(df, i), horizon, windowErr(), strategy = strategyErr())
        knnPredY <- modelSelectionKNN(head(df, i)[, 2], horizon, i - horizon,
                                      windowErr(), type = "direct", error_measure = MASE)$forecasts
        naivePredY <- naiveForecast(head(df, i), horizon)
        
        
        #Unnormalise data
        svmPredY        <- (svmPredY        * sdTarget) + meanTarget
        defaultSvmPredY <- (defaultSvmPredY * sdTarget) + meanTarget
        knnPredY        <- (knnPredY        * sdTarget) + meanTarget
        naivePredY      <- (naivePredY      * sdTarget) + meanTarget
        
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
  

  naiveForecast <- function(df, horizon){
    # Moving average forecast - Average of the last data according to a certain embedding e
    e <- 30
    rep(mean(tail(head(df, -horizon)$target, e)), horizon)
  }
})