library(shiny)
library(htmlwidgets)
library(ggplot2)
library(car)
library(shinyjs)
data(mtcars)

shinyServer(function(input,output){
  
  mtresid <- reactive({
    data.frame(mtcars, predict=predict(lm(mpg~wt, data=mtcars)), resid=rstudent(lm(mpg~wt, data=mtcars)))
  })
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(mtcars))
  )
  
  keep <- reactive({
    mtresid()[ vals$keeprows, , drop = FALSE]
  })
  
  exclude <- reactive({
    mtresid()[!vals$keeprows, , drop = FALSE]
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
    switch(input$plot,
    resid=ggplot(mtresid(), aes(x=predict, y=resid)) + geom_point(),
    qq=qqPlot(lm(mpg~wt, data=keep()), pch=16, asp=1)
    )
  })
  
  observeEvent(input$plot_click, {
    res <- nearPoints(mtresid(), input$plot_click, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot2_click, {
    res <- nearPoints(mtresid(), input$plot2_click, xvar=predict, yvar=resid, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot_dblclick, {
    res <- nearPoints(mtresid(), input$plot_dblclick, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot_brush, {
      res <- brushedPoints(mtresid(), input$plot_brush, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot2_hover:\n")
    str(input$plot2_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
#   output$brush_info <- renderPrint({
#     cat("input$plot_brush:\n")
#     str(input$plot_brush)
#   })

# output$brush_info <- renderTable({
#   mtresid()
# })
  
  observeEvent(input$reset, {
    vals$keeprows <- rep(TRUE, nrow(mtresid()))
  })
  
  output$lm <- renderTable({
    summary(lm(mpg~wt, data=keep()))
  })
  
})
  