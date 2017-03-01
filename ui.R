library(shiny)
library(plotly)

shinyUI(

  fluidPage(

    # Application title
    titlePanel("Stock market"),

    tabsetPanel(type = "tabs",
      # Shows the plot generated
      tabPanel("Data inspection",
        sidebarLayout(
          sidebarPanel(
            selectInput('market', 'Which market :', c()),

            selectInput('dataset', 'Which dataset :', c()),

            sliderInput("days",
                       "Cut data set : ",
                       min = 2, max = 1000, value = c(1, 365)),

            sliderInput("trainingcut",
                       "Cut training set : ",
                       min = 2, max = 1000, value = 30)
          ),

          mainPanel(
            h4(""),
            plotlyOutput("dataPlot"),
            h4("Summary"),
            verbatimTextOutput("summary")
          )
        )
      ),

      tabPanel("SVM forecasting",
        sidebarLayout(
          sidebarPanel(
            sliderInput("cost",
                        "Parameter Cost : (2^X)",
                        min = -5, max = 5, value = 0),

            sliderInput("gamma",
                        "Parameter Gamma : (2^X)",
                        min = -5, max = 5, value = 0),

            sliderInput("rwindow",
                        "Rolling window size :",
                        min = 1, max = 50, value = 30),

            sliderInput("horizon",
                        "Horizon :",
                        min = 1, max = 50, value = 1),

            radioButtons("strategy","Forecasting strategy",
                         c("Recursive" = "recursive", "Direct" = "direct"), inline = TRUE),

            checkboxInput("applySVM", "Show SVM", TRUE),

            checkboxInput("applyNaiveModel", "Show Naive Model", FALSE),

            actionButton("addModel", "Add Model"),
            actionButton("resetModels", "Reset"),

            htmlOutput("modelList")
          ),

          mainPanel(
            h4(""),
            plotlyOutput("predPlot"),
            h4("Prediction errors table"),
            tableOutput("predTable"),
            plotlyOutput("predErrorPlot")
          )
        )
      )
    )
  )
)