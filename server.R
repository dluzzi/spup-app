library(shiny)
library(ggplot2)
# Define server 



shinyServer(function(input, output) {
  #Test data
  data <- simulations
  mean.df<- as.data.frame(data@Mean, xy = T)
  std.df<- as.data.frame(data@Standard.Deviation, xy = T)
  
  cat <- simulations.quadtree
  mlc.df<- as.data.frame(cat@Most.Likely.Class, xy = T)
  clpb.df<- as.data.frame(cat@Class.Probabilities, xy = T)
  
  rel.error <- (std.df$layer/mean.df$layer)*100
  rel.error[is.na(rel.error)] <- -1
  
  
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
  
  predictionInterval <- reactive({
    mean <- mean.df$layer
    std <- std.df$layer
    n <- length(data@Realisations[1])
    confidence <- (1 - (input$percentage/100))/2
    confidence <- 1 - confidence
    error <- qt(confidence, df=n-1)*std/sqrt(n)
    interval <- 2*error
    interval[is.na(interval)] <- -1
    return(interval)
  })
  
  output$relerrorslider <- renderUI({
    sliderInput("relerror", "Relative Error (%)", min = 0, max = ceiling(max(rel.error)), 
                value = ceiling(max(rel.error)),
                step = 0.01)
  })
  
  output$predinterval <- renderUI({
    list(
      numericInput("percentage", "Prediction Interval (%)", min = 0, max = 99.99, value = 90),
      numericInput("predinterval", "Interval", value = 50 , min = 0, max = 10000, step = 5))
  })
  
  output$contPlot1 <- renderPlot({
    if (input$options == "Mean"){
      if (input$statistics == "Relative Error"){  
        for (i in 1:length(mean.df$layer)){
          if (rel.error[i] > input$relerror) {
            mean.df$layer[i] <- NA
          }
        }
      }
      if (input$statistics == "Prediction Interval"){
        for (i in 1:length(mean.df$layer)){
          if (predictionInterval()[i] > input$predinterval) {
            mean.df$layer[i] <- NA
          }
        }
      }

      map <- ggplot(mean.df, aes(x=x, y=y)) +
        geom_tile(aes(fill = layer)) +
        scale_fill_gradientn(colours=colorRampPalette(c("#3f524c", "#5a7b5e", "#96ac87", "#cfc59f", "#fdedd8"))(20), name = input$options) +
        coord_equal(xlim=c(min(mean.df$x),max(mean.df$x)),ylim = c(min(mean.df$y),max(mean.df$y))) +
        theme
    }
    if(input$options == "Standard Deviation"){
      if (input$statistics == "Relative Error"){  
        for (i in 1:length(mean.df$layer)){
          if (rel.error[i] > input$relerror) {
            std.df$layer[i] <- NA
          }
        }
      }
      if (input$statistics == "Prediction Interval"){
        for (i in 1:length(mean.df$layer)){
          if (predictionInterval()[i] > input$predinterval) {
            std.df$layer[i] <- NA
          }
        }
      }
      
      map <- ggplot(std.df, aes(x=x, y=y)) +
        geom_tile(aes(fill = layer)) +
        scale_fill_gradientn(colours = colorRampPalette(c("black", "white"))(25), name = input$options) +
        coord_equal(xlim=c(min(std.df$x),max(std.df$x)),ylim = c(min(std.df$y),max(std.df$y))) +
        theme
    }
    return(map)
  })


  output$contPlot2 <- renderPlot({
    if (!is.null(input$contPlot_click)) {
    mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
    realisations <- extract(data@Realisations, mat)
    hist(realisations) 
    }
    })
  
  output$contPlot3 <- renderPlot({
    if (!is.null(input$contPlot_click) & !is.null(input$contPlot_dblclick)) {
      mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
      realisations <- extract(data@Realisations, mat)
      mat2 <- matrix(c(input$contPlot_dblclick$x, input$contPlot_dblclick$y), ncol = 2)
      realisations2 <- extract(data@Realisations, mat2)
      plot(realisations, realisations2)
    }
  })
  
  output$catPlot1 <- renderPlot({
    
    ggplot(na.omit(mlc.df), aes(x=x, y=y)) +
      geom_tile(aes(fill = layer)) +
      scale_fill_gradientn(colours=colorRampPalette(c("#3f524c", "#5a7b5e", "#96ac87", "#cfc59f", "#fdedd8"))(20), name = input$catoptions) +
      coord_equal(xlim=c(min(mlc.df$x),max(mlc.df$x)),ylim = c(min(mlc.df$y),max(mlc.df$y))) +
      theme
    })
  output$catPlot3 <- renderPlot({
    if (!is.null(input$catPlot_click)) {
      mat <- matrix(c(input$catPlot_click$x, input$catPlot_click$y), ncol = 2)
      realisations <- extract(cat@Realisations, mat)
      
      i = min(realisations)
      range <- min(realisations):max(realisations)
      list <- numeric()
      for (i in range){
        name <- paste("Class", i)
        if (sum(realisations == i) > 0){
        list[name] <- sum(realisations == i)
        }
      }
      pie(list, main = "Class probabilities of realisations") 
    }
  })
  output$catPlot2 <- renderPrint({
    print(input$catPlot_click) 
  })
  
})