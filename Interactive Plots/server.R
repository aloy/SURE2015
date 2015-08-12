library(shiny)
library(htmlwidgets)
library(ggplot2)
library(car)
library(stats)
require(graphics)
data(mtcars)

shinyServer(function(input,output, session){
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  
  theData <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      return(      
        read.csv(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote, 
                 row.names=input$rownames)
      )
    }
    else
      data.frame(mtcars)
  })
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(isolate(as.data.frame(theData()))))
  )
  
  keep <- reactive({
      x <- input$xvar
      y <- input$yvar
    keep0 <- data.frame(theData()[ vals$keeprows, , drop = FALSE][,c(x, y)])
    colnames(keep0) <- c("x", "y")
    n <- nrow(keep0)
data.frame(keep0, predict=predict(lm(y~x, data=keep0)), resid=rstudent(lm(y~x, data=keep0)),
      quant=qnorm((seq(1:n)-0.5)/n))
    })
  
  exclude <- reactive({
    x <- input$xvar
    y <- input$yvar
    exclude0 <- theData()[!vals$keeprows, , drop = FALSE][,c(x, y)]
    colnames(exclude0) <- c("x", "y")
    exclude0
  })
  
output$scatterx <- renderUI({
  num <- colnames(theData())[sapply(theData(),is.numeric)]
  selectInput("xvar", label="X Variable", choices=num)
})

output$scattery <- renderUI({
  num <- colnames(theData())[sapply(theData(),is.numeric)]
  selectInput("yvar", label="Y Variable", choices=num)
})

  output$plot1 <- renderPlot({    
    n <- which(colnames(theData())==input$xvar)
    n2 <- which(colnames(theData())==input$yvar)
    xmin  <- floor(min(theData()[,n]))
    xmax <- ceiling(max(theData()[,n]))
    ymin <- floor(min(theData()[,n2]))
    ymax <- ceiling(max(theData()[,n2]))
    if(xmin==min(theData()[,n])){
      xmin <- xmin- diff(range(theData()[,n])/10)
    }
    if(ymin==min(theData()[,n2])){
      ymin <- ymin-diff(range(theData()[,n2])/10)
    }
    if(xmax==max(theData()[,n])){
      xmax <- xmax+ diff(range(theData()[,n])/10)
    }
    if(ymax==max(theData()[,n2])){
      ymax <- ymax+ diff(range(theData()[,n2])/10)
    }
      if(input$lm==TRUE){
        ggplot(keep(), aes(x, y)) + geom_point() + stat_smooth(method="lm") +
          geom_point(data = exclude(), shape = 21, fill = NA, color = "black", alpha = 0.25) +
          coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
          theme(panel.grid.minor = element_line(colour = "grey"), 
                panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
                axis.text = element_text(colour = "black"))
      }else{
    ggplot(keep(), aes(x, y)) + geom_point() + xlab(paste(input$xvar)) +
      geom_point(data = exclude(), shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
      theme(panel.grid.minor = element_line(colour = "grey"), 
            panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
            axis.text = element_text(colour = "black"))
}
  })

  output$plot2 <- renderPlot({
    switch(input$plot,
    resid=ggplot(keep(), aes(x=x, y=resid)) + geom_point() + geom_abline(slope=0, intercept=0) +
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
    res <- nearPoints(keep(), input$plot_click, xvar="x", yvar="y", allRows = TRUE)
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

# output$brush_info <- renderTable({
# residData()
# })

  observeEvent(input$reset, {
    data <- isolate(theData())
    vals$keeprows <- rep(TRUE, nrow(data))
  })
  
  output$lm <- renderPrint({
    summary(lm(y~x, data=keep()))
  })

output$diagPlot <- renderPlot({
  dffits <- data.frame(dffits(lm(y~x, data=keep())))
  names(dffits) <- "dffits.lm."
  switch(input$diag,
         lev=plot(lm(y~x, data=keep()), which = 5, pch = 16),
         cooks=plot(lm(y~x, data=keep()), which = 4, pch = 16),
         dffits= plot(dffits$dffits.lm, type="h") 
#            ggplot(dffits, aes(x=seq(nrow(dffits)), y=dffits.lm.)) + 
#            geom_bar(stat="identity", width=0.1) + geom_text(aes(label=row.names(dffits))) + 
#     theme(panel.grid.minor = element_line(colour = "grey"),
#           panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
#           axis.text = element_text(colour = "black"))
#Works in the console but not in Shiny   
         )
if(input$diag=="dffits"){
  attach(dffits)
  text(row(dffits), dffits$dffits.lm, row.names(dffits), cex=0.7, pos=4)
}
})

output$residChoices <- renderUI({
  checkboxGroupInput("vars2", label="Residual Variables", choices=colnames(subset(theData())))
})

residData <-  reactive({
  y <- input$yvar
  vars <- input$vars2
  if(is.null(input$vars2)){
    newData0 <- data.frame(theData()[ vals$keeprows, , drop = FALSE][,y])
    names(newData0) <- "y"
  }else{
    newData0 <-  data.frame(theData()[ vals$keeprows, , drop = FALSE][,c(vars)],
                            y=theData()[ vals$keeprows, , drop = FALSE][,c(y)])
  }
  newData0
})

output$resid <- renderPlot({
    if(is.null(input$vars2)){
      residLM <- lm(y~1, data=residData())
    }
    else{
      residLM <- lm(y~., data=residData())
    }
    residualPlot(residLM, type="rstudent", pch=16, col.quad="blue")
  })

output$av <- renderPlot({
  if(is.null(input$vars2)){
    avLM <- lm(y~x, data=keep())
  }
  else{
    avLM <- lm(y~., data=residData())
  }
  avPlots(avLM, col.lines='blue', pch=16)
})


})
  