library(shiny)
library(htmlwidgets)
library(ggplot2)
library(car)
data(mtcars)

shinyServer(function(input,output){
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(mtcars))
  )
  
  keep <- reactive({
    mtcars[ vals$keeprows, , drop = FALSE]
  })
  
  exclude <- reactive({
    mtcars[!vals$keeprows, , drop = FALSE]
  })
  
  output$plot1 <- renderPlot({    
      if(input$lm==TRUE){
        ggplot(keep(), aes(wt, mpg)) + geom_point() + stat_smooth(method="lm") +
          geom_point(data = exclude(), shape = 21, fill = NA, color = "black", alpha = 0.25) +
          coord_cartesian(xlim = c(1.5, 5.5), ylim = c(5,35))+
          theme(panel.grid.minor = element_line(colour = "grey"), 
                panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
                axis.text = element_text(colour = "black"))
      }
      else{
        ggplot(keep(), aes(wt, mpg)) + geom_point() +
          geom_point(data = exclude(), shape = 21, fill = NA, color = "black", alpha = 0.25) +
          coord_cartesian(xlim = c(1.5, 5.5), ylim = c(5,35))+
          theme(panel.grid.minor = element_line(colour = "grey"), 
          panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
          axis.text = element_text(colour = "black"))
      }
  })
  
  output$plot2 <- renderPlot({
    residualPlot(lm(mpg~wt, data=keep()), pch = 16, type = "rstudent", col.quad="blue")
  })
  
  observeEvent(input$plot_click, {
    res <- nearPoints(mtcars, input$plot_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(mtcars, input$plot_brush, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
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
  
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(mtcars))
  })
  
})