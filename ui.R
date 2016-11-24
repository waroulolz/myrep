library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CAC 40 Stock market"),
  
  # Sidebar with a slider input for id of market
  sidebarPanel(
    sliderInput("nb", 
                "Which market :", 
                min = 1, max = 40, value = 1),
    
    selectInput("model", "Choose a model:", 
                choices = c("SVM", "Linear Model")),
    
    checkboxInput("applyModel", "Show model", FALSE)
    
  ),
  
  
  # Shows the plot generated
  mainPanel(
    plotOutput("distPlot")
  )
))