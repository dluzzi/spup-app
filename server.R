library(shiny)
# Define server 



shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$contPlot1 <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  output$catPlot1 <- renderPlot({
    hist(rnorm(input$n))
  })
  output$catPlot2 <- renderPlot({
    hist(runif(input$n2))
  })
  output$catPlot3 <- renderPlot({
    hist(rnorm(input$n))
  })
  output$catPlot4 <- renderPlot({
    hist(runif(input$n2))
  })
  
})