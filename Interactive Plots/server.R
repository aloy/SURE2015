library(shiny)
library(htmlwidgets)
library(ggplot2)
library(car)

shinyServer(function(input,output){
  output$plot1 <- renderPlot({
      if(input$lm==TRUE){
        ggplot(mtcars, aes(wt, mpg)) + geom_point() + stat_smooth(method="lm") +
          theme(panel.grid.minor = element_line(colour = "grey"), 
         panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
         axis.text = element_text(colour = "black"))
      }
      else{
        ggplot(mtcars, aes(wt, mpg)) + geom_point() + 
          theme(panel.grid.minor = element_line(colour = "grey"), 
          panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
          axis.text = element_text(colour = "black"))
      }
  })
  
  output$plot2 <- renderPlot({
    residualPlot(lm(mpg~wt, data=mtcars), pch = 16, type = "rstudent", col.quad="blue")
  })
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
})