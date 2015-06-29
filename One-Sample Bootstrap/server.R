library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
  
  ## One-Sample Bootstrap
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggplot2)
  
  data(SleepStudy)
  
  filedata <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      return(      
        read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else
      as.data.frame(SleepStudy[,26, drop=FALSE])
  })
  
  output$contents <- renderTable({
    filedata()
  })
  
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "contents", anim = TRUE))
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  output$varChoose <- renderUI({
    df <- filedata()
    if (is.null(df)) 
      return(NULL)
    vars=names(df)
    names(vars)=vars
    selectInput("boot","Choose a quantitative variable to examine:",vars)
  })
  
  varData <- reactive({
    var<-input$boot
    filedata()[,var]
  })
  
  output$origHist <- renderPlot({
    dataPlot <- switch(input$plot, 
                       his =  qplot(data=filedata(), x=varData(), geom="histogram", binwidth=input$w, asp=1),
                       den =  qplot(data=filedata(), x=varData(), geom="density", asp=1),
                       qq = qplot(sample=varData(), asp=1),
                       hisDen = qplot(data=filedata(), x=varData(),
                              binwidth=input$w)+ aes(y=..density..) + geom_density()
                       )
    
    dataPlot
  })
  
  output$summary <- renderPrint({
    summary(as.data.frame(varData()))
  })
  
  output$sd <- renderText({
    sd(varData())
  })
  
  trialsFunction <- function(x, list) {
    switch(list,
           bootMean = do(input$num) * mean(sample(varData(), replace = TRUE)),
           bootMedian = do(input$num) * median(sample(varData(), replace = TRUE)),
           bootSd = do(input$num) * sd(sample(varData(), replace = TRUE))
    )
  }
  
  
  trials <- reactive(
    trialsFunction(filedata(), input$stat)$result
  )
  
  output$bootHist <- renderPlot({
    dataPlot <- switch(input$plot, 
                       his = qplot(trials(), geom="histogram", 
                                   binwidth=input$w2, xlab=paste("Bootstrap of", input$stat), ylab="Frequency", asp=1),
                       den = qplot(trials(), geom="density",
                                   xlab=paste(input$stat, "Hours of sleep"), ylab="Density", asp=1),
                       qq = qplot(sample=trials(), asp=1),
                       hisDen = qplot(trials(), binwidth=input$w2)+ aes(y=..density..) + geom_density()
    )
    
    dataPlot
  })
  
  output$bootSummary <- renderPrint({
    summary(as.data.frame(trials()))
  })
  
  output$bootBias <- renderText({
    mean(varData())-mean(trials())
  })
  
  output$bootSd <- renderText({
    sd(trials())
  })
  
  level <- reactive (
    input$level
  )
  
  alpha <- reactive (
    1- level()
  )
  
  SE <- reactive (
    sd(trials())
  )
  
  ciType <- function(x, double) {
    switch(double,
           perc =  quantile(trials(), probs = c(alpha()/2, 1-alpha()/2)),
           norm = c(mean(varData()) - qnorm(1-alpha()/2) *  SE(), mean(varData()) + qnorm(1-alpha()/2) * SE())
    )}
  
  output$ciPrint <- renderPrint({
    ciType(trials(), input$ci)
  })
  
  output$percLower <- renderPrint({
    quantile(trials(), probs = c(alpha()))
  })
  
  output$percUpper <- renderPrint({
    quantile(trials(), probs = c(1-alpha()))
  })
  
  output$normPrint <- renderText({
    ciType(trials(), input$ci)
  })
  
  output$normUpper <- renderText({
    c(paste(100*level(),'%'), mean(varData()) + qnorm(1-alpha()/2) * SE())
  })
  
  output$normLower <- renderText({
    c(paste(100*alpha(),'%'), mean(varData()) - qnorm(1-alpha()/2) * SE())
  })
 
})