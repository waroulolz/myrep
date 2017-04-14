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

            radioButtons("preProcess", "Data transformation :",
                         c("Raw Data" = "raw", "Compound Return" = "creturn", "Volatility" = "volatility")),
            
            dateRangeInput("days",
                           "Date range:",
                           start = Sys.Date() - 365*2,
                           end = Sys.Date())
          ),

          mainPanel(
            h4(""),
            plotlyOutput("dataPlot"),
            h4("Summary"),
            verbatimTextOutput("summary")
          )
        )
      ),

      tabPanel(" Multi-step ahead forecasting",
        sidebarLayout(
          sidebarPanel(width = 3,
            sliderInput("cost",
                        "Parameter Cost :",
                        min = -7, max = 6, value = 0, pre = "2^", ticks = FALSE),

            sliderInput("gamma",
                        "Parameter Gamma :",
                        min = -7, max = 6, value = -6, pre = "2^", ticks = FALSE),

            sliderInput("rwindow",
                        "Order of the model :",
                        min = 2, max = 50, value = 21),

            sliderInput("trainingcut",
                        "Horizon : ",
                        min = 2, max = 100, value = 14),

            radioButtons("strategy", "Forecasting strategy :",
                         c("Recursive" = "recursive", "Direct" = "direct"), inline = TRUE),

            checkboxInput("applySVM", "Show SVM", TRUE),

            checkboxInput("applyKnnModel", "Show KNN Model", TRUE),

            checkboxInput("applyNaiveModel", "Show Naive Model", FALSE)
          ),

          mainPanel(
            h4(""),
            plotlyOutput("predPlot"),
            tableOutput("predTable")
          )
        )
      ),
      
      tabPanel("Average error",
        sidebarLayout(
          sidebarPanel(width = 3,
            sliderInput("costErr",
                       "Parameter Cost :",
                       min = -7, max = 6, value = 0, pre = "2^", ticks = FALSE),

            sliderInput("gammaErr",
                       "Parameter Gamma :",
                       min = -7, max = 6, value = -6, pre = "2^", ticks = FALSE),

            sliderInput("rwindowErr",
                       "Order of the model :",
                       min = 2, max = 50, value = 21),

            sliderInput("horizon",
                        "Horizon :",
                        min = 2, max = 50, value = 14),

            radioButtons("strategyErr", "Forecasting strategy :",
                        c("Recursive" = "recursive", "Direct" = "direct"), inline = TRUE)
         ),
         mainPanel(
           h4(""),
           h4("Means of errors with rolling window"),
           tableOutput("errorTable")
          )
        )
      )
    )
  )
)