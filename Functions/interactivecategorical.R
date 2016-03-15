#' Interactively Visualise Continuous Spatial Data
#' 
#' Allows for visualising spatial uncertainty for continuous data in an
#' interactive interface. Options include basic map of predicted values and
#' standard deviation, as well as observing relative error and prediction
#' intervals. 
#' 
#' @param x Object of class simulations. Must contain continuous data. 
#'   
#' @return Interactive application to visualise the data.
#' @export
#' 
#' @examples
#' 
interactiveCategorical <- function(x) {
  require(shiny)
  require(ggplot2)
  require(raster)
  
  # Check if x is of correct class
  if (is(x, "Simulations") != T) {
    stop("Expected object of class Simulations")
  }
  
  # Check if x is of categorical data
  if (hasValues(x@Most.Likely.Class) != T) {
    stop("simulations object expected to contain categorical data")
  }
  
  shinyApp(
    ui = fluidPage(
      titlePanel("Visualisation of Categorical Data"),
      sidebarLayout(
        sidebarPanel(h6("This application allows the user to select a point
                        in the categorical map and view the class probabilities
                        for that specfic point.")),
      mainPanel(
        fluidRow(
          column(6,
          plotOutput("catPlot1",
                     click = clickOpts(id = "catPlot_click")
                     )
          ),
          column(6, plotOutput("catPlot2")
          )
        )
      ))
    ),
    
    server = function(input, output) {
      
      mlc.df<- as.data.frame(x@Most.Likely.Class, xy = T)
      clpb.df<- as.data.frame(x@Class.Probabilities, xy = T)
      
      theme <- theme(plot.title = element_text(),
                     panel.border = element_rect(colour = "black", fill = "NA"),
                     axis.ticks = element_blank(),
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
          realisations <- extract(x@Realisations, mat)
          
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