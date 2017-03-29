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
          sidebarPanel(width = 3,
            selectInput('market', 'Which market :', c("CAC 40")),

            selectInput('dataset', 'Which dataset :', c()),

            dateRangeInput("days",
                           "Date range:",
                           start = Sys.Date() - 365*2,
                           end = Sys.Date()),

            sliderInput("trainingcut",
                       "Days to forecast : ",
                       min = 2, max = 100, value = 14)
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
          sidebarPanel(width = 3,
            sliderInput("cost",
                        "Parameter Cost :",
                        min = -6, max = 5, value = 0, pre = "2^", ticks = FALSE),

            sliderInput("gamma",
                        "Parameter Gamma :",
                        min = -6, max = 5, value = 0, pre = "2^", ticks = FALSE),

            sliderInput("rwindow",
                        "Rolling window size :",
                        min = 2, max = 50, value = 21),

            radioButtons("strategy", "Forecasting strategy",
                         c("Recursive" = "recursive", "Direct" = "direct"), inline = TRUE),

            checkboxInput("applySVM", "Show SVM", TRUE),

            checkboxInput("applyKnnModel", "Show KNN Model", TRUE),

            checkboxInput("applyNaiveModel", "Show Naive Model", FALSE),

            actionButton("addModel", "Add Model"),
            actionButton("resetModels", "Reset"),

            htmlOutput("modelList")
          ),

          mainPanel(
            h4(""),
            plotlyOutput("predPlot"),
            h4("Prediction errors table"),
            tableOutput("predTable")
          )
        )
      ),
      
      tabPanel("Forecasting Error",
        sidebarLayout(
          sidebarPanel(width = 3,
            sliderInput("horizon",
                        "Horizon :",
                        min = 1, max = 50, value = 21)
         ),
         mainPanel(
           h4(""),
           h4("Rolling window errors table"),
           tableOutput("errorTable")
          )
        )
      )
    )
  )
)