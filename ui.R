library(shiny)
# Define user interface for app

shinyUI(
  navbarPage("spup Visualisation",
              tabPanel("Continuous",
               fluidRow(
                 column(4,
                    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
                    ),
                 column(8,
                        fluidRow(
                          column(6, plotOutput("contPlot1")
                          ),
                          column(6, plotOutput("contPlot2")
                          )
                        ),
                        fluidRow(
                          column(6, plotOutput("contPlot3")
                          ),
                          column(6, plotOutput("contPlot4")
                          )
                        )
                 )
                )
              ),
              tabPanel("Categorical",
                fluidRow(
                  column(4,
                    numericInput("n", "sample", value = 25),
                    numericInput("n2", "sample", value = 50)
                  ),
                  column(8,
                    fluidRow(
                      column(6, plotOutput("catPlot1")
                        ),
                      column(6, plotOutput("catPlot2")
                        )
                      ),
                      fluidRow(
                        column(6, plotOutput("catPlot3")
                          ),
                        column(6, plotOutput("catPlot4")
                          )
                        )
                  )
                )
    ),
    inverse = T
  )
)
