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
                       his =  qplot(data=filedata(), x=varData(), geom="histogram", 
                              binwidth=input$w, main="Original Sample", asp=1),
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
    c(paste(100*level(),'%'), varDataFunction(filedata(), input$stat) - qnorm(input$level) * SE())
  })
  
  output$normLower <- renderText({
    c(paste(100*alpha(),'%'), varDataFunction(filedata(), input$stat) - qnorm(input$level) * SE())
  })
  
  ## Two-Sample Bootstrap
  library(ggvis)
  tv <- read.csv("../data/TV.csv")
  
  filedata2 <- reactive({
    if(input$chooseData2=="uploadYes"){
      inFile2 <- input$file2
      if (is.null(inFile2))
        return(NULL)
      return(      
        read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, quote=input$quote2)
      )
    }
    else
      as.data.frame(tv)
  })
  
  output$contents2 <- renderTable({
    filedata2()
  })
  
  shinyjs::onclick("hideData2",
                   shinyjs::toggle(id = "contents2", anim = TRUE))
  
  shinyjs::onclick("hideDataOptions2",
                   shinyjs::toggle(id = "dataOptions2", anim = TRUE))
  
  output$varChoose2 <- renderUI({
    df2 <- filedata2()
    vars2 <- colnames(df2)[sapply(df2,is.factor)]
    names(vars2)=vars2
      if(input$chooseData2=="uploadNo")
        textInput("choose1",label="Choose a categorical variable with two groups:", value="Cable")
      else
        selectInput("choose1",label="Choose a categorical variable with two groups:",vars2)
    })
    
  output$varChoose3 <- renderUI({
    df2 <- filedata2()
    vars3 <- colnames(df2)[sapply(df2,is.numeric)]
    names(vars3)=vars3
    if(input$chooseData2=="uploadNo")
      textInput("choose2",label="Choose a quantitative variable to examine between groups:", value="Time")
    else
      selectInput("choose2",label="Choose a quantitative variable to examine between groups:",vars3)
  })
  
  catVals <- reactive({ 
    filedata2()[,input$choose1]
  })
  
  quantVals <- reactive({ 
    filedata2()[,input$choose2]
  })
  
  output$origHist2 <- renderPlot({
    f<-paste(input$choose1, "~", ".")
    dataPlot <-switch(input$plot2,
                       his2 = qplot(data=filedata2(), x=quantVals(), facets=f, binwidth=input$w3, 
                                    main="Original Sample", asp=1),
                       den2 = qplot(data=filedata2(), x=quantVals(), facets=f, geom="density", asp=1),
                       qq2 = qplot(sample=quantVals(), data=filedata2(), facets=f, asp=1),
                       hisDen2 = qplot(data=filedata2(), x=quantVals(), facets=f) + aes(y=..density..)+geom_density()
    )
dataPlot
    })

  output$basicSummary <- renderPrint({
    favstats(~quantVals()|catVals())  
    })

simdata <- reactive({ 
  data.frame(catVals(), quantVals())
})

origStat <- reactive({ #Our original data frame, with all of the calculations we want (for bias)
  grouped_trials <- group_by(simdata(), catVals())
  group_means <- summarise(grouped_trials, mean=mean(quantVals()), med=median(quantVals()) * 1.0, sd = sd(quantVals()))
  summarise(group_means, mean.diff=diff(mean), med.diff=diff(med) * 1.0,
    mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2] * 1.0, sd.ratio = sd[1]/sd[2])
  
})

trials2 <- reactive({
  do(input$num) * sample(group_by(simdata(), catVals..), replace = TRUE)
})

allStat <- reactive({
  grouped_trials <- group_by(trials2(), .index, catVals..)
  group_means <- summarise(grouped_trials, mean=mean(quantVals..), med=median(quantVals..) * 1.0, sd = sd(quantVals..))
  summarise(group_means, mean.diff=diff(mean), med.diff=diff(med) * 1.0,
             mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2] * 1.0, sd.ratio = sd[1]/sd[2])
})

x1 = list()
x2 = list()

origStatSwitch <- function(x1, list){
    switch(input$stat2, 
           bootMean2= assign("x1", origStat()$mean.diff),
           bootMedian2= assign("x1", origStat()$med.diff),
           bootMeanRatio = assign("x1", origStat()$mean.ratio),
           bootMedRatio = assign("x1", origStat()$med.ratio),
           bootSdRatio= assign("x1", origStat()$sd.ratio)
    )}


plotType2 <- function(x2, type) {
  switch(type,
         his2 =  qplot(x2, binwidth=input$w4, 
                       ylab="Frequency", asp=1),
         den2 = qplot(x2, geom="density", 
                      ylab="Density", asp=1),
         qq2 = qplot(sample=x2, asp=1),
         hisDen2 = qplot(x2, binwidth=input$w4)+ aes(y=..density..) + geom_density()
  )}

dataSwitch <- function(y, list){
  switch(input$stat2, 
      bootMean2= assign("x2", allStat()$mean.diff),
      bootMedian2= assign("x2", allStat()$med.diff),
      bootMeanRatio = assign("x2", allStat()$mean.ratio),
      bootMedRatio = assign("x2", allStat()$med.ratio),
      bootSdRatio= assign("x2", allStat()$sd.ratio)
)}

  output$bootHist2 <- renderPlot({
    plotType2(dataSwitch(), input$plot2)
  })
  
  output$bootSummary2 <- renderPrint({
    summary(dataSwitch())
  })
  
  output$bootBias2 <- renderText({
    mean(origStatSwitch())-mean(dataSwitch())
  })

  
  output$bootSd2 <- renderText({
    sd(dataSwitch())
  })
  
#   level2 <- reactive(
#     input$level2
#   )
#   
#   alpha2 <- reactive(
#     1 - level2()
#   )
#   
#   SE2 <- reactive (
#     sd(allStatSwitch(allStat, input$stat2))
#   )
#   
#   output$ciPrint2 <- renderPrint({
#     quantile((allStatSwitch(allStat, input$stat2)), 
#              probs=c(alpha2()/2, 1-alpha2()/2))
#   })
#   
#   output$percLower2 <- renderPrint({
#     quantile((allStatSwitch(allStat, input$stat2)), probs = c(alpha2()))
#   })
#   
#   output$percUpper2 <- renderPrint({
#     quantile((allStatSwitch(allStat, input$stat2)), probs = c(1-alpha2()))
#   })
#   
#   output$normPrint2 <- renderText({
#     c(mean((allStatSwitch(allStat, input$stat2))) - qnorm(1-alpha2()/2) *  SE2(), 
#       (mean(allStatSwitch(allStat, input$stat2))) + qnorm(1-alpha2()/2) * SE2())
#   })
#   
#   output$normLower2 <- renderText({
#     c(paste(100*alpha(),'%'), mean(allStatSwitch(allStat, input$stat2)) 
#       - qnorm(level2()) * SE2())
#   })
#   
#   output$normUpper2 <- renderText({
#     c(paste(100*level(),'%'), mean(allStatSwitch(allStat, input$stat2))
#       + qnorm(level2()) * SE2())
#   })
})