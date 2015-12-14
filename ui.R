library(shiny)
# Define user interface for app

shinyUI(
  navbarPage("spup Visualisation",
              tabPanel("Continuous",
                    
               sidebarLayout(
                 sidebarPanel(
                    radioButtons("options", "Display Options",
                                 choices = c("Standard Deviation",
                                             "Quantiles"),
                                 selected = "Standard Deviation"
                                  )
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
                    numericInput("n", "sample", value = 25),
                    numericInput("n2", "sample", value = 50)
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




