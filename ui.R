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
                    radioButtons("statistics", "More Information",
                                 choices = c("None Selected" = "",
                                             "Relative Error",
                                             "Prediction Interval"),
                                 selected = ""),
                    conditionalPanel("input.statistics == 'Relative Error'",
                                    uiOutput("relerrorslider")),
                    conditionalPanel("input.statistics == 'Prediction Interval'",
                                     uiOutput("predinterval"))
                    ),
                 mainPanel(
                        fluidRow(
                          plotOutput("contPlot1",
                            click = clickOpts(id = "contPlot_click"),
                            dblclick = dblclickOpts(id = "contPlot_dblclick")
                          )
                        ),
                        fluidRow(
                          column(6, plotOutput("contPlot2")
                          ),
                          column(6, plotOutput("contPlot3")
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
                      plotOutput("catPlot1",
                        click = clickOpts(id = "catPlot_click"),
                        dblclick = dblclickOpts(id = "catPlot_dblclick")
                        )
                      ),
                    fluidRow(
                      column(6, plotOutput("catPlot2")
                      ),
                      column(6, plotOutput("catPlot3")
                      )
                    )
                  )
                )
    ),
    inverse = T
    
  )
)




