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
    if(input$options == "Mean"){
      dist <- rnorm(length(mean.df$layer), mean(mean.df$layer), sd(mean.df$layer))
      quantile <- quantile(dist, input$slidermean)
      for (i in 1:length(mean.df$layer)){
        if (mean.df$layer[i] < quantile) {
          mean.df$layer[i] <- NA
        }
      }
      map <- ggplot(na.omit(mean.df), aes(x=x, y=y)) +
        geom_tile(aes(fill = layer)) +
        scale_fill_gradientn(colours=colorRampPalette(c("#3f524c", "#5a7b5e", "#96ac87", "#cfc59f", "#fdedd8"))(20), name = input$options) +
        coord_equal(xlim=c(min(mean.df$x),max(mean.df$x)),ylim = c(min(mean.df$y),max(mean.df$y))) +
        theme
    }
    if(input$options == "Standard Deviation"){
      for (i in 1:length(std.df$layer)){
        if (std.df$layer[i] > input$sliderstd) {
          std.df$layer[i] <- NA
        }}
      map <- ggplot(na.omit(std.df), aes(x=x, y=y)) +
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
  
  output$contPlot3 <- renderPrint({
    if (!is.null(input$contPlot_click)) {
      mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
      realisations <- extract(data@Realisations, mat)
      print("Mean")
      print(mean(realisations))
      print("Standard Deviation")
      print(sd(realisations))
      print(input$contPlot_click)

      }
    })
  
  output$contPlot4 <- renderPlot({
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
  output$catPlot2 <- renderPlot({
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
  output$catPlot3 <- renderPrint({
    print(input$catPlot_click) 
  })
  output$catPlot4 <- renderPlot({
  })
  
})