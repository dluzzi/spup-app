library(shiny)
# Define user interface for app

shinyUI(
  navbarPage("spup Visualisation",
              tabPanel("Continuous",
                    
               sidebarLayout(
                 sidebarPanel(
                    radioButtons("options", "Display Options",
                                 choices = c("Mean",
                                             "Standard Deviation"),
                                 selected = "Mean"
                                  ),
                    conditionalPanel("input.options == 'Mean'",
                    sliderInput("slidermean", "Quantile", min = 0, max = 1, value = 0)),
                    conditionalPanel("input.options == 'Standard Deviation'",
                                     sliderInput("sliderstd", "Confidence", min = 0, max = 100, value = 100))
                    ),
                 mainPanel(
                        fluidRow(
                          column(6, plotOutput("contPlot1",
                                               click = clickOpts(id = "contPlot_click"),
                                               dblclick = dblclickOpts(id = "contPlot_dblclick"),
                                               hover = hoverOpts(id = "contPlot_hover"),
                                               brush = brushOpts(id = "contPlot_brush")
                                               )
                          ),
                          column(6, plotOutput("contPlot2")
                          )
                        ),
                        fluidRow(
                          column(6, verbatimTextOutput("contPlot3")
                          ),
                          column(6, plotOutput("contPlot4")
                          )
                        )
                 )
                )
              ),
              tabPanel("Categorical",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("catoptions", "Display Options", 
                                 choices = c("Most Likely Class"),
                                 selected = "Most Likely Class")
                  ),
                  mainPanel(
                    fluidRow(
                      column(6, plotOutput("catPlot1",
                                           click = clickOpts(id = "catPlot_click"),
                                           dblclick = dblclickOpts(id = "catPlot_dblclick"),
                                           hover = hoverOpts(id = "catPlot_hover"),
                                           brush = brushOpts(id = "catPlot_brush")
                                           )
                      ),
                      column(6, plotOutput("catPlot2")
                        )
                      ),
                      fluidRow(
                        column(6, verbatimTextOutput("catPlot3")
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




