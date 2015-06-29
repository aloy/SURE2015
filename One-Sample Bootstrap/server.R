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
  
  varDataFunction <- function(x, list) {
    switch(input$stat,
           bootMean = mean(filedata()),
           bootMedian = median(filedata()),
           bootSd = sd(filedata())
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
  
  output$varChoose2 <- renderUI({
    df2 <- filedata2()
    vars2 <- colnames(df2)[sapply(df2,is.factor)]
    names(vars2)=vars2
      if(input$chooseData=="uploadNo")
        textInput("choose1",label="Choose a categorical variable with two groups:", value="Cable")
      else
        textInput("choose1",label="Choose a categorical variable with two groups:",vars2)
    })
    
  output$varChoose3 <- renderUI({
    df2 <- filedata2()
    vars3 <- colnames(df2)[sapply(df2,is.factor)]
    names(vars3)=vars3
    if(input$chooseData=="uploadNo")
      textInput("choose2",label="Choose a quantitative variable to examine between groups:", value="Time")
    else
      textInput("choose2",label="Choose a quantitative variable to examine between groups:",vars3)
  })
  
  catVar <- reactive({ 
    filedata2()[,input$choose1]
  })
  
  quantVar <- reactive({ 
    filedata2()[,input$choose2]
  })

  grouped <- reactive({
    if(input$chooseData2=="uploadYes"){
      group_by(filedata2(), catName())
    }
    else
      group_by(filedata2(), Cable)
  })
  
  output$origHist2 <- renderPlot({
    f<-paste(input$choose1, "~", ".")
    dataPlot <-switch(input$plot2,
                       his2 = qplot(data=filedata2(), x=quantVar(), facets=f, binwidth=input$w3, 
                                    main="Original Sample", asp=1),
                       den2 = qplot(data=filedata2(), x=quantVar(), facets=f, geom="density", asp=1),
                       qq2 = qplot(sample=quantVar(), data=filedata2(), facets=f, asp=1),
                       hisDen2 = qplot(data=filedata2(), x=quantVar(), facets=f) + aes(y=..density..)+geom_density()
    )
dataPlot
    })

  output$basicSummary <- renderPrint({
    favstats(~quantVar()|catVar())  
    })
 
# q <- reactive({
#   filedata()[,input$choose2]
#   
# })
# 
# trials2 <- reactive({
#   do(input$num) * sample(group_by(filedata2(), q()), replace = TRUE)
# })
# 
# allStat <- reactive({
#   grouped_trials <- group_by(trials2(), .index, q..) 
#   aggregate(Time~Cable+.index, data= grouped_trials, mean)
# })

  allStat<- reactive({
    summarise(summarise(group_by(do(input$num2) * sample(grouped(), replace = TRUE), .index, varCat()),
              mean=mean(varQuant()), med=median(varQuant()), sd = sd(varQuant())), mean.diff=diff(mean), med.diff=diff(med),
              sd.diff=diff(sd), mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2], sd.ratio = sd[1]/sd[2])
  })
  
test<- renderPrint({
  head(allStat())
})

# #   
#   plotType2 <- function(x, type) {
#     switch(type,
#            his2 =  qplot(allStat()$mean.diff, binwidth=input$w4, 
#                          xlab="Means", ylab="Frequency", asp=1),
#            den2 = qplot(allStat()$mean.diff, geom="density", 
#                         xlab="Difference in Means", ylab="Density", asp=1),
#            qq2 = qplot(sample=allStat()$mean.diff, asp=1),
#            hisDen2 = qplot(allStat()$mean.diff, binwidth=input$w4)+ aes(y=..density..) + geom_density()
#     )}
#   
#   output$bootMeanHist2 <- renderPlot({
#     plotType2(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
#   })
#   
#   output$bootMeanSummary2 <- renderPrint({
#     summary(allStat()$mean.diff)
#   })
#   
#   output$bootMeanBias2 <- renderText({
#     mean(summarise(summarise(group_by(grouped, varQuant()), mean = mean(varQuant())), 
#                    mean.diff = diff(mean))$mean.diff)-mean(allStat()$mean.diff)
#   })
#   
#   output$bootMeanSd2 <- renderText({
#     sd(allStat()$mean.diff)
#   })
#   
#   
#   plotType3 <- function(x, type) {
#     switch(type,
#            his2 =  qplot(allStat()$med.diff, binwidth=input$w4, 
#                          xlab="Medians", ylab="Frequency", asp=1),
#            den2 = qplot(allStat()$med.diff, geom="density", 
#                         xlab="Difference in Medians", ylab="Density", asp=1),
#            qq2 = qplot(sample=allStat()$med.diff, asp=1),
#            hisDen2 = qplot(allStat()$med.diff, binwidth=input$w4)+ aes(y=..density..) + geom_density()
#     )}
#   
#   output$bootMedianHist2 <- renderPlot({
#     plotType3(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
#   })
#   
#   output$bootMedianSummary2 <- renderPrint({
#     summary(allStat()$med.diff)
#   })
#   
#   output$bootMedianBias2 <- renderText({
#     mean(summarise(summarise(group_by(grouped, varQuant()), median = median(varQuant())), 
#                    med.diff = diff(median))$med.diff)-mean(allStat()$med.diff)
#   })
#   
#   output$bootMedianSd2 <- renderText({
#     sd(allStat()$med.diff)
#   })
# 
#   plotType4 <- function(x, type) {
#     switch(type,
#            his2 =  qplot(allStat()$mean.ratio, binwidth=input$w4, 
#                          xlab="Ratios", ylab="Frequency", asp=1),
#            den2 = qplot(allStat()$mean.ratio, geom="density", 
#                         xlab="Ratio of Means", ylab="Density", asp=1),
#            qq2 = qplot(sample=allStat()$mean.ratio, asp=1),
#            hisDen2 = qplot(allStat()$mean.ratio, binwidth=input$w4)+ aes(y=..density..) + geom_density()
#     )}
#   
#   
#   output$bootMeanRatioHist <- renderPlot({
#     plotType4(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
#   })
#   
#   output$bootMeanRatioSummary <- renderPrint({
#     summary(allStat()$mean.ratio)
#   })
#   
#   output$bootMeanRatioBias <- renderText({
#     mean(summarise(summarise(group_by(grouped, varQuant()), 
#                              mean = mean(varQuant())), mean.ratio = mean[1] / mean[2])$mean.ratio)
#     -mean(allStat()$mean.ratio)
#   })
#   
#   output$bootMeanRatioSd <- renderText({
#     sd(allStat()$mean.ratio)
#   })
#   
#   
#   plotType5 <- function(x, type) {
#     switch(type,
#            his2 =  qplot(allStat()$med.ratio, binwidth=input$w4, 
#                          xlab="Ratios", ylab="Frequency", asp=1),
#            den2 = qplot(allStat()$med.ratio, geom="density", 
#                         xlab="Ratio of Means", ylab="Density", asp=1),
#            qq2 = qplot(sample=allStat()$med.ratio, asp=1),
#            hisDen2 = qplot(allStat()$med.ratio)+ aes(y=..density..) + geom_density()
#     )}
#   
#   output$bootMedRatioHist <- renderPlot({
#     plotType5(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
#   })
#   
#   output$bootMedRatioSummary <- renderPrint({
#     summary(allStat()$med.ratio)
#   })
#   
#   output$bootMedRatioBias <- renderText({
#     mean(summarise(summarise(group_by(grouped, varQuant()), 
#                              med = median(varQuant())), med.ratio = med[1] / med[2])$med.ratio)
#     -mean(allStat()$med.ratio)
#   })
#   
#   output$bootMedRatioSd <- renderText({
#     sd(allStat()$med.ratio)
#   })
#   
#   
#   plotType6 <- function(x, type) {
#     switch(type,
#            his2 =  qplot(allStat()$sd.ratio, binwidth=input$w4, 
#                          xlab="Ratios", ylab="Frequency", asp=1, rescale.axis=TRUE),
#            den2 = qplot(allStat()$sd.ratio, geom="density", 
#                         xlab="Ratio of Standard Deviation", ylab="Density", asp=1, rescale.axis=TRUE),
#            qq2 = qplot(sample=allStat()$sd.ratio, asp=1),
#            hisDen2 = qplot(allStat()$sd.ratio, binwidth=input$w4)+ aes(y=..density..) + geom_density()
#     )}
#   
#   output$bootSdRatioHist <- renderPlot({
#     plotType6(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
#   })
#   
#   output$bootSdRatioSummary <- renderPrint({
#     summary(allStat()$sd.ratio)
#   })
#   
#   output$bootSdRatioBias <- renderText({
#     mean(summarise(summarise(group_by(grouped, varQuant()), sd = sd(varQuant())), sd.ratio = sd[1] / sd[2])$sd.ratio)
#     -mean(allStat()$sd.ratio)
#   })
#   
#   output$bootSdRatioSd <- renderText({
#     sd(allStat()$sd.ratio)
#   })
#   
#   allStatSwitch <- function(x, double) {
#     switch(double,
#            "bootMean2" = as.data.frame(allStat())$mean.diff,
#            "bootMedian2" = as.data.frame(allStat())$med.diff,
#            "bootMeanRatio" = as.data.frame(allStat())$mean.ratio,
#            "bootMedRatio" = as.data.frame(allStat())$med.ratio,
#            "bootSdRatio" =as.data.frame(allStat())$sd.ratio
#     )}
#   
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