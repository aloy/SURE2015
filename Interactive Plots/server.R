library(shiny)
library(htmlwidgets)
library(ggplot2)
library(car)
# library(shinyjs)
library(stats)
require(graphics)
data(mtcars)

shinyServer(function(input,output, session){
  
  shinyjs::onclick("hideCoord",
                   shinyjs::toggle(id = "coordInfo", anim = TRUE))
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(mtcars))
  )
  
  keep <- reactive({
    keep0 <- mtcars[ vals$keeprows, , drop = FALSE] 
    n <- nrow(keep0)
data.frame(keep0, predict=predict(lm(mpg~wt, data=keep0)), resid=rstudent(lm(mpg~wt, data=keep0)),
      quant=qnorm((seq(1:n)-0.5)/n))
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
    switch(input$plot,
    resid=ggplot(keep(), aes(x=predict, y=resid)) + geom_point() +
    theme(panel.grid.minor = element_line(colour = "grey"), 
            panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
            axis.text = element_text(colour = "black")),
    qq=ggplot(keep(), aes(x=quant, y=sort(resid))) + geom_point() +
      theme(panel.grid.minor = element_line(colour = "grey"), 
            panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
            axis.text = element_text(colour = "black")) + coord_fixed(ratio=1) +
      geom_abline(slope=1, intercept=0, colour="blue")
    )
  })
  
  observeEvent(input$plot_click, {
    res <- nearPoints(keep(), input$plot_click, xvar="wt", yvar="mpg", allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot2_click, {
    if(input$plot=="resid"){
    res <- nearPoints(keep(), input$plot2_click, xvar="predict", yvar="resid", allRows = TRUE)
    }else{
      res <- nearPoints(keep(), input$plot2_click, xvar="quant", yvar=sort("resid"), allRows = TRUE)
    }
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot_dblclick, {
    res <- nearPoints(keep(), input$plot_dblclick, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot_brush, {
      res <- brushedPoints(keep(), input$plot_brush, allRows = TRUE)
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
  
  observeEvent(input$reset, {
    vals$keeprows <- rep(TRUE, nrow(mtcars))
  })
  
  output$lm <- renderPrint({
    summary(lm(mpg~wt, data=keep()))
  })

output$diagPlot <- renderPlot({
  dffits <- data.frame(dffits(lm(mpg~wt, data=keep())))
  names(dffits) <- "dffits.lm."
  switch(input$diag,
         lev=plot(lm(mpg~wt, data=keep()), which = 5, pch = 16),
         cooks=plot(lm(mpg~wt, data=keep()), which = 4, pch = 16),
         dffits= plot(dffits(lm(mpg~wt, data=mtcars)), type="h")
#            ggplot(dffits, aes(x=seq(nrow(dffits)), y=dffits.lm.)) + 
#            geom_bar(stat="identity", width=0.1) + geom_text(aes(label=row.names(dffits))) + 
#     theme(panel.grid.minor = element_line(colour = "grey"),
#           panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
#           axis.text = element_text(colour = "black"))
#Works in the console but not in Shiny   
         )
})
  
})
  