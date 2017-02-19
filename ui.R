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
                       min = 1, max = 1000, value = c(1, 365)),

            sliderInput("trainingset",
                       "Cut training set : ",
                       min = 1, max = 1000, value = 30)
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
                        min = -3, max = 7, value = 0),

            sliderInput("gamma",
                        "Parameter Gamma : (2^X)",
                        min = -3, max = 7, value = 0),

            sliderInput("rwindow",
                        "Rolling window size :",
                        min = 30, max = 365, value = 50),

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
            tableOutput("predTable"),
            plotlyOutput("predErrorPlot")
          )
        )
      )
    )
  )
)