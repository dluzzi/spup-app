#' Interactively Visualise Categorical Spatial Data
#' 
#' Allows for visualising spatial uncertainty for categorical data in an 
#' interactive interface. Allows for more knowledge about class probabilities at
#' a certain location.
#' If data contains NA values, consider changing these to NaN values for a more
#' insightful visualisations.
#' 
#' @param x Object of class simulations. Must contain continuous data.
#'   
#' @return Interactive application to visualise the data.
#' @export
#' 
#' @examples
#' 
interactiveContinuous <- function(x){
  require(shiny)
  require(ggplot2)
  require(raster)
  
  # Check if x is of correct class
  if (is(x, "Simulations") != T) {
    stop("Expected object of class Simulations")
  }
  
  # Check if x is of continuous data
  if (hasValues(x@Mean) != T) {
    stop("simulations object expected to contain continuous data")
  }
  
  shinyApp(
    ui = fluidPage(
      titlePanel("Visualisation of Continuous Data"),
      sidebarLayout(
      sidebarPanel(
        p("This application allows the user to explore the continuous datset: 
           Single-clicking a point in the map allows the user to view the 
           realisations found at that point.
           In addition, through double-clicking a second, seperate point, a 
           scatterplot of the realisations found at the single-clicked and 
           double-clicked is displayed."),
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
                         uiOutput("predinterval")),
        p("They greyed-out areas in the map represent areas that have a relative
          error larger than the selected relative error. For the prediction interval,
          areas are greyed-out that have an interquartile range larger than the 
          threshold selected.")
        
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
    server = function(input, output) {
      mean.df<- as.data.frame(x@Mean, xy = T)
      std.df<- as.data.frame(x@Standard.Deviation, xy = T)
      rel.error <- (std.df$layer/mean.df$layer)*100
      rel.error[is.na(rel.error)] <- -1
      
      theme <- theme(plot.title = element_text(),
                     panel.border = element_rect(colour = "black", fill = "NA"),
                     axis.ticks = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank(),
                     legend.title = element_text(size = 14, face = "bold"),
                     legend.key = element_rect(fill='white'),
                     legend.text = element_text(size = 12))
      
      quantileValue <- reactive({
        threshold <- x@Quantiles$X95. - x@Quantiles$X5.
        threshold <- as.data.frame(threshold)
        threshold[is.na(threshold)] <- -1
        return(threshold)
      })
      
      output$relerrorslider <- renderUI({
        sliderInput("relerror", "Relative Error (%)", min = 0, max = ceiling(max(rel.error)), 
                    value = ceiling(max(rel.error)),
                    step = 0.01)
      })
      
      output$predinterval <- renderUI({
          numericInput("predinterval", "Threshold of interquantile range", value = 50 , min = 0, max = 10000, step = 5)
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
              if (quantileValue()[i,1] > input$predinterval) {
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
              if (quantileValue()[i,1] > input$predinterval) {
                std.df$layer[i] <- NA
              }
            }
          }
          
          map <- ggplot(std.df, aes(x=x, y=y)) +
            geom_tile(aes(fill = layer)) +
            scale_fill_gradientn(colours = colorRampPalette(c("black", "#699feb"))(20), name = input$options) +
            coord_equal(xlim=c(min(std.df$x),max(std.df$x)),ylim = c(min(std.df$y),max(std.df$y))) +
            theme
        }
        return(map)
      })
      
      
      output$contPlot2 <- renderPlot({
        if (!is.null(input$contPlot_click)) {
          mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
          Realisations <- extract(x@Realisations, mat)
          hist(Realisations, xlab = "Value of Realisations") 
        }
      })
      
      output$contPlot3 <- renderPlot({
        if (!is.null(input$contPlot_click) & !is.null(input$contPlot_dblclick)) {
          mat <- matrix(c(input$contPlot_click$x, input$contPlot_click$y), ncol = 2)
          realisations <- extract(x@Realisations, mat)
          mat2 <- matrix(c(input$contPlot_dblclick$x, input$contPlot_dblclick$y), ncol = 2)
          realisations2 <- extract(x@Realisations, mat2)
          plot(realisations, realisations2, asp = 1, main = "Scatterplot of realisations",
               xlab = "Realisations of single-click point",
               ylab = "Realisaitons of double-click point")
          abline(0,1)
        }
      })
    }
  )
}