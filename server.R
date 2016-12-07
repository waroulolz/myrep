library(shiny)
library(e1071)
library("RColorBrewer")
source("C:/Users/GG/Desktop/Memoire/myrep/ErrorMeasures.R")

#Data extraction
df <- read.csv('histoCAC40_2Y.CSV')
df$date <- as.Date(df$date)
df <- df[order(as.Date(df$date, format="%y/%m/%d")),]
df$target <- df$lastPrice


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

getSVM <- function(df, cost, gamma){
  model <- svm(target ~ date, gamma = 2^gamma, cost = 2^cost, data = df)
  predict(model, df)
}


shinyServer(function(input, output) {
  
  getDf <- function(){
    chosenIid = unique(df$iid)[input$nb]
    df <- df[df$iid == chosenIid,]
    df
  }
  
  params <- reactiveValues(costs = NULL, gammas = NULL, MASES = NULL)
  
  observeEvent(input$addModel, {
    params$costs <- c(params$costs, input$cost)
    params$gammas <- c(params$gammas, input$gamma)
    predictedY <- getSVM(getDf(), input$cost, input$gamma)
    params$MASES <- c(params$MASES, MASE(getDf()$target, predictedY))
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
  
  output$distPlot <- renderPlot({

    df <- getDf()
    
    plot(target ~ date, data = df, type = 'l',
         main = paste("Plot of the market iid : ", unique(df$iid), sep = ""))
    
    nbModels <- length(params$costs)
    if (nbModels > 0){
      for (i in 1:nbModels){
        colors <- getColors(nbModels)
        predictedY <- getSVM(df, params$costs[i], params$gammas[i])
        points(df$date, predictedY, col = colors[i], type='l', lwd=1)
      }
    }
    
    if (input$applySVM){
      model <- svm(target ~ date, gamma = 2^input$gamma, cost = 2^input$cost, data = df)
      predictedY <- predict(model, df)
      points(df$date, predictedY, col = "red", type='l', lwd=1)
    } 
    if (input$applyLinearModel){
      model <- lm(target~ date, df)
      predictedY <- predict(model, df)
      points(df$date, predictedY, col = "blue", type='l', lwd=1)
    }
  })
})