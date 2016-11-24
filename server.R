library(shiny)

shinyServer(function(input, output) {
  

  #Data extraction
  df <- read.csv('histoCAC40_2Y.CSV')
  
  
  output$distPlot <- renderPlot({
    
    df$date <- as.Date(df$date)
    chosenIid = unique(df$iid)[input$nb]
    df <- df[df$iid == chosenIid,]
    df <- df[order(as.Date(df$date, format="%y/%m/%d")),]
    
    df$target <- df$lastPrice
    plot(target ~ date, data = df, type = 'l',
         main = paste("Plot of the market iid : ", chosenIid, sep = ""))
    
    if (input$applyModel){
      
      if (input$model == "SVM"){
        model <- svm(target ~ date, df)
        predictedY <- predict(model, df)
        points(df$date, predictedY, col = "red", type='l', lwd=2)
      } 
      else if (input$model == "Linear Model"){
        model <- lm(target~ date, df)
        predictedY <- predict(model, df)
        points(df$date, predictedY, col = "red", type='l', lwd=2)
      }
    }
  })
})