library(shiny)
library(ggplot2)
# Define server 



shinyServer(function(input, output) {
  #Test data
  data <- simulations
  mean.df<- as.data.frame(simulations@Mean, xy = T)
  std.df<- as.data.frame(simulations@Standard.Deviation, xy = T)
  
  
  theme <- theme(plot.title = element_text(),
                 panel.grid = element_blank(),
                 panel.border = element_rect(colour = "black", fill = "NA"),
                 axis.ticks = element_blank(),
                 axis.text.y = element_blank(),
                 axis.text.x = element_blank(),
                 strip.background = element_blank(),
                 legend.title = element_text(size = 14, face = "bold"),
                 legend.key = element_rect(fill='white'),
                 legend.text = element_text(size = 12))
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$contPlot1 <- renderPlot({
    
    ggplot(na.omit(mean.df), aes(x=x, y=y)) +
      geom_tile(aes(fill = layer)) +
      coord_equal(xlim=c(min(mean.df$x),max(mean.df$x)),ylim = c(min(mean.df$y),max(mean.df$y))) +
      theme
  })


  output$contPlot2 <- renderPlot({
    if (!is.null(input$contPlot_click)) {
    mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
    realisations <- extract(data@Realisations, mat)
    hist(realisations) 
    }
    })
  
  output$contPlot3 <- renderPrint({
    if (!is.null(input$contPlot_click)) {
      mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
      realisations <- extract(data@Realisations, mat)
      print("Mean")
      print(mean(realisations))
      print("Standard Deviation")
      print(sd(realisations))
      }
    })
  
  output$contPlot4 <- renderPlot({
    if (!is.null(input$contPlot_click)) {
      mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
      realisations <- extract(data@Realisations, mat)
      plot(density(realisations))
    }
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