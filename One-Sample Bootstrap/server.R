library(shiny)
shinyServer(function(input,output){
  
  ## One-Sample Bootstrap
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggplot2)
  
  data(SleepStudy)
  original_data <- SleepStudy$AverageSleep
  
  output$origHist <- renderPlot({
    w <- input$w
    qplot(original_data, xlab="Hours of Sleep", ylab="Frequency", main="Original Sample", asp=1)
  })
  output$summary <- renderPrint({
      summary(original_data)
  })
  output$mean <- renderText({
    mean(original_data)
  })
  output$sd <- renderText({
    sd(original_data)
  })
  
  trialsFunction <- function(x, list) {
    switch(list,
           bootMean = do(input$num) * mean(sample(original_data, replace = TRUE)),
           bootMedian = do(input$num) * median(sample(original_data, replace = TRUE)),
           bootSd = do(input$num) * sd(sample(original_data, replace = TRUE))
    )
  }
  
  trials <- reactive(
    trialsFunction(original_data, input$stat)$result
  )
  
  plotType <- function(x, type) {
    switch(type,
           his =  qplot(trials(), geom="histogram", 
                        binwidth=input$w, xlab=paste("Bootstrap of", input$stat), ylab="Frequency", asp=1),
           den = qplot(trials(), geom="density",
                       xlab=paste(input$stat, "Hours of sleep"), ylab="Density", asp=1)
   )}
  
  output$bootHist <- renderPlot({
    plotType(original_data, input$plot)
  })
  
 output$bootSummary <- renderPrint({
    summary(trials())
  })

  output$bootBias <- renderText({
    mean(original_data)-mean(trials())
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

observed <- reactive (
mean(original_data)
  )

SE <- reactive (
sd(trials())
  )
  
  ciType <- function(x, double) {
    switch(double,
           perc =  quantile(trials(), probs = c(alpha()/2, 1-alpha()/2)),
           norm = c(observed() - qnorm(1-alpha()/2) *  SE(), observed() + qnorm(1-alpha()/2) * SE())
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
  c(paste(100*level(),'%'), observed() - qnorm(input$level) * SE())
})

output$normLower <- renderText({
  c(paste(100*alpha(),'%'), observed() - qnorm(input$level) * SE())
})

## Two-Sample Bootstrap
library(ggvis)
tv <- read.csv("../data/TV.csv")
grouped <- group_by(tv, Cable)

diffs <- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable), mean = mean(Time)), 
             mean.diff = diff(mean))
)

plotType2 <- function(x, type) {
  switch(type,
         his2 =  qplot(diffs()$mean.diff, binwidth=input$w2, 
                  xlab="Means", ylab="Frequency", asp=1),
         den2 = qplot(diffs()$mean.diff, geom="density", 
                  xlab="Difference in Means", ylab="Density", asp=1)
  )}

output$bootMeanHist2 <- renderPlot({
  plotType2(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootMeanSummary2 <- renderPrint({
  summary(diffs()$mean.diff)
})

output$bootMeanBias2 <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), mean = mean(Time)), 
  mean.diff = diff(mean))$mean.diff)-mean(diffs()$mean.diff)
})

output$bootMeanSd2 <- renderText({
  sd(diffs()$mean.diff)
})

meds <- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable),
                      median = median(Time)), med.diff = diff(median))
)


plotType3 <- function(x, type) {
  switch(type,
         his2 =  qplot(meds()$med.diff, binwidth=input$w2, 
                       xlab="Medians", ylab="Frequency", asp=1),
         den2 = qplot(meds()$med.diff, geom="density", 
                      xlab="Difference in Medians", ylab="Density", asp=1)
  )}

output$bootMedianHist2 <- renderPlot({
  plotType3(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootMedianSummary2 <- renderPrint({
  summary(meds()$med.diff)
})

output$bootMedianBias2 <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), median = median(Time)), 
                 med.diff = diff(median))$med.diff)-mean(meds()$med.diff)
})

output$bootMedianSd2 <- renderText({
  sd(meds()$med.diff)
})

ratioList <- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable), 
                      mean = mean(Time), med=median(Time), sd = sd(Time)), mean.ratio = mean[1] / mean[2], 
                      med.ratio = med[1]/med[2], sd.ratio=sd[1]/sd[2])
)

plotType4 <- function(x, type) {
  switch(type,
         his2 =  qplot(ratioList()$mean.ratio, binwidth=input$w2, 
                       xlab="Ratios", ylab="Frequency", asp=1),
         den2 = qplot(ratioList()$mean.ratio, geom="density", 
                      xlab="Ratio of Means", ylab="Density", asp=1)
  )}


output$bootMeanRatioHist <- renderPlot({
  plotType4(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootMeanRatioSummary <- renderPrint({
  summary(ratioList()$mean.ratio)
})

output$bootMeanRatioBias <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), 
       mean = mean(Time)), mean.ratio = mean[1] / mean[2])$mean.ratio)
       -mean(ratioList()$mean.ratio)
})

output$bootMeanRatioSd <- renderText({
  sd(ratioList()$mean.ratio)
})


plotType5 <- function(x, type) {
  switch(type,
         his2 =  qplot(ratioList()$med.ratio, binwidth=input$w2, 
                       xlab="Ratios", ylab="Frequency", asp=1),
         den2 = qplot(ratioList()$med.ratio, geom="density", 
                      xlab="Ratio of Means", ylab="Density", asp=1)
  )}

output$bootMedRatioHist <- renderPlot({
  plotType5(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootMedRatioSummary <- renderPrint({
  summary(ratioList()$med.ratio)
})

output$bootMedRatioBias <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), 
  med = median(Time)), med.ratio = med[1] / med[2])$med.ratio)
  -mean(ratioList()$med.ratio)
})

output$bootMedRatioSd <- renderText({
  sd(ratioList()$med.ratio)
})


plotType6 <- function(x, type) {
  switch(type,
         his2 =  qplot(ratioList()$sd.ratio, binwidth=input$w2, 
                       xlab="Ratios", ylab="Frequency", asp=1, rescale.axis=TRUE),
         den2 = qplot(ratioList()$sd.ratio, geom="density", 
                      xlab="Ratio of Standard Deviation", ylab="Density", asp=1, rescale.axis=TRUE)
  )}

output$bootSdRatioHist <- renderPlot({
  plotType6(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootSdRatioSummary <- renderPrint({
  summary(ratioList()$sd.ratio)
})

output$bootSdRatioBias <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), sd = sd(Time)), sd.ratio = sd[1] / sd[2])$sd.ratio)
  -mean(ratioList()$sd.ratio)
})

output$bootSdRatioSd <- renderText({
  sd(ratioList()$sd.ratio)
})

allStat<- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable),
            mean=mean(Time), med=median(Time), sd = sd(Time)), mean.diff=diff(mean), med.diff=diff(med),
            sd.diff=diff(sd), mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2], sd.ratio = sd[1]/sd[2])
)

allStatSwitch <- function(x, double) {
  switch(double,
         "bootMean2" = as.data.frame(allStat())$mean.diff,
         "bootMedian2" = as.data.frame(allStat())$med.diff,
         "bootMeanRatio" = as.data.frame(allStat())$mean.ratio,
         "bootMedRatio" = as.data.frame(allStat())$med.ratio,
         "bootSdRatio" =as.data.frame(allStat())$sd.ratio
  )}

level2 <- reactive(
  input$level2
)

alpha2 <- reactive(
  1 - level2()
  )

SE2 <- reactive (
  sd(allStatSwitch(allStat, input$stat2))
)

output$ciPrint2 <- renderPrint({
  quantile((allStatSwitch(allStat, input$stat2)), 
            probs=c(alpha2()/2, 1-alpha2()/2))
})

output$percLower2 <- renderPrint({
  quantile((allStatSwitch(allStat, input$stat2)), probs = c(alpha2()))
})

output$percUpper2 <- renderPrint({
  quantile((allStatSwitch(allStat, input$stat2)), probs = c(1-alpha2()))
})

output$normPrint2 <- renderText({
  c(mean((allStatSwitch(allStat, input$stat2))) - qnorm(1-alpha2()/2) *  SE2(), 
    (mean(allStatSwitch(allStat, input$stat2))) + qnorm(1-alpha2()/2) * SE2())
})

output$normLower2 <- renderText({
  c(paste(100*alpha(),'%'), mean(allStatSwitch(allStat, input$stat2)) 
    - qnorm(level2()) * SE2())
})

output$normUpper2 <- renderText({
  c(paste(100*level(),'%'), mean(allStatSwitch(allStat, input$stat2))
    + qnorm(level2()) * SE2())
})
})