library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Stock market"),
  
  sidebarPanel(
    sliderInput("nb", 
                "Which market :", 
                min = 1, max = 40, value = 1),
    
    sliderInput("cost", 
                "Parameter Cost : (2^X)", 
                min = -3, max = 7, value = 0),
    
    sliderInput("gamma",
                "Parameter Gamma : (2^X)", 
                min = -3, max = 7, value = 0),

    checkboxInput("applySVM", "Show SVM", FALSE),

    checkboxInput("applyLinearModel", "Show Linear Model", FALSE),

    actionButton("addModel", "Add Model"),
    actionButton("resetModels", "Reset"),

    htmlOutput("modelList")

  ),
  
  
  # Shows the plot generated
  mainPanel(
    plotOutput("distPlot")
  )
))