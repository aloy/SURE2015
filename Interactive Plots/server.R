library(shiny)
library(htmlwidgets)
library(ggplot2)
library(car)
library(stats)
require(graphics)
data(mtcars)

shinyServer(function(input,output, session){
  
  shinyjs::onclick("hideCoord",
                   shinyjs::toggle(id = "coordInfo", anim = FALSE))
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(mtcars))
  )
  
  keep <- reactive({
    if(is.null(input$vars)==FALSE){
      m <- input$vars
    keep0 <- data.frame(mtcars[ vals$keeprows, , drop = FALSE][,c(m, "mpg")])
    colnames(keep0) <- c("m", "mpg")
    n <- nrow(keep0)
data.frame(keep0, predict=predict(lm(mpg~m, data=keep0)), resid=rstudent(lm(mpg~m, data=keep0)),
      quant=qnorm((seq(1:n)-0.5)/n))
}
    })
  
  exclude <- reactive({
    m <- input$vars
    mtcars[!vals$keeprows, , drop = FALSE][,c(m, "mpg")]
  })
  
output$scatterChoices <- renderUI({
  selectInput("vars", label="Scatterplot Variable", choices=colnames(subset(mtcars, select=-mpg)))
})

  output$plot1 <- renderPlot({    
    if(is.null(input$vars)==FALSE){
      if(input$lm==TRUE){
        ggplot(keep(), aes(m, mpg)) + geom_point() + stat_smooth(method="lm") +
          theme(panel.grid.minor = element_line(colour = "grey"), 
                panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
                axis.text = element_text(colour = "black"))
      }else{
    ggplot(keep(), aes(m, mpg)) + geom_point() + xlab(paste(input$vars)) +
      theme(panel.grid.minor = element_line(colour = "grey"), 
            panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
            axis.text = element_text(colour = "black"))
  
}
}
  })

output$excluded <- renderText({
  nrow(exclude())
})

  output$plot2 <- renderPlot({
    switch(input$plot,
    resid=ggplot(keep(), aes(x=predict, y=resid)) + geom_point() + geom_abline(slope=0, intercept=0) +
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
    res <- nearPoints(keep(), input$plot_click, xvar="m", yvar="mpg", allRows = TRUE)
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
    summary(lm(mpg~m, data=keep()))
  })

output$diagPlot <- renderPlot({
  dffits <- data.frame(dffits(lm(mpg~m, data=keep())))
  names(dffits) <- "dffits.lm."
  switch(input$diag,
         lev=plot(lm(mpg~m, data=keep()), which = 5, pch = 16),
         cooks=plot(lm(mpg~m, data=keep()), which = 4, pch = 16),
         dffits= plot(dffits(lm(mpg~m, data=keep())), type="h")
#            ggplot(dffits, aes(x=seq(nrow(dffits)), y=dffits.lm.)) + 
#            geom_bar(stat="identity", width=0.1) + geom_text(aes(label=row.names(dffits))) + 
#     theme(panel.grid.minor = element_line(colour = "grey"),
#           panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
#           axis.text = element_text(colour = "black"))
#Works in the console but not in Shiny   
         )
})

output$residChoices <- renderUI({
  checkboxGroupInput("vars2", label="Residual Variables", choices=colnames(subset(mtcars, select=-c(mpg))))
})

# output$scatter <- renderPlot({
#   data <- isolate(keep())
#   n <- which(colnames(data)==input$vars)
#   colnames(data)[n] <- "m"
#   ggplot(data, aes(x=m, y=mpg)) + geom_point() +  xlab(paste(input$vars)) +
#     theme(panel.grid.minor = element_line(colour = "grey"), 
#           panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
#           axis.text = element_text(colour = "black"))
# })

residData <-  reactive({
  vars <- input$vars2
  if(is.null(input$vars2)){
    newData0 <- data.frame(mtcars[ vals$keeprows, , drop = FALSE][,c("mpg")])
    names(newData0) <- "mpg"
  }else{
    newData0 <-  data.frame(mtcars[ vals$keeprows, , drop = FALSE][,c(vars, "mpg")])
  }
  newData0
})

output$resid <- renderPlot({
    if(is.null(input$vars2)){
      residLM <- lm(mpg~1, data=residData())
    }
    else{
      residLM <- lm(mpg~., data=residData())
    }
    residualPlot(residLM, type="rstudent", pch=16, col.quad="blue")
  })

output$av <- renderPlot({
  if(is.null(input$vars2)){
    avLM <- lm(mpg~m, data=keep())
  }
  else{
    avLM <- lm(mpg~., data=residData())
  }
  avPlots(avLM, col.lines='blue', pch=16)
})


})
  