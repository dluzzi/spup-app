interactiveCategorical <- function(data) {
  require(shiny)
  require(ggplot2)
  
  shinyApp(
    ui = fluidPage(
      titlePanel("Visualisation of Categorical Data"),
      mainPanel(
        fluidRow(
          column(6,
          plotOutput("catPlot1",
                     click = clickOpts(id = "catPlot_click"),
                     dblclick = dblclickOpts(id = "catPlot_dblclick")
          )
          ),
          column(6, plotOutput("catPlot2")
          )
        )
      )
    ),
    
    server = function(input, output) {
      
      mlc.df<- as.data.frame(data@Most.Likely.Class, xy = T)
      clpb.df<- as.data.frame(data@Class.Probabilities, xy = T)
      
      theme <- theme(plot.title = element_text(),
                     panel.grid = element_blank(),
                     panel.border = element_rect(colour = "black", fill = "NA"),
                     axis.ticks = element_blank(),
                     axis.text.y = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank(),
                     legend.title = element_text(size = 14, face = "bold"),
                     legend.key = element_rect(fill='white'),
                     legend.text = element_text(size = 12))
      
      output$catPlot1 <- renderPlot({
        
        ggplot(na.omit(mlc.df), aes(x=x, y=y)) +
          geom_tile(aes(fill = factor(layer))) +
          scale_fill_discrete(name = "Most Likely Class") +
          coord_equal(xlim=c(min(mlc.df$x),max(mlc.df$x)),ylim = c(min(mlc.df$y),max(mlc.df$y))) +
          theme
      })
      output$catPlot2 <- renderPlot({
        if (!is.null(input$catPlot_click)) {
          mat <- matrix(c(input$catPlot_click$x, input$catPlot_click$y), ncol = 2)
          realisations <- extract(data@Realisations, mat)
          
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
    }
  )
}