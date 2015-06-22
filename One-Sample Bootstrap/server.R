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

sd2 <- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable),
                      sd = sd(Time)), sd.diff = diff(sd))
)

plotType4 <- function(x, type) {
  switch(type,
         his2 =  qplot(sd2()$sd.diff, binwidth=input$w2, 
                       xlab="Standard Deviations", ylab="Frequency", asp=1),
         den2 = qplot(sd2()$sd.diff, geom="density", 
                      xlab="Difference in Standard Deviations", ylab="Density", asp=1)
  )}

output$bootSdHist2 <- renderPlot({
  plotType4(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootSdSummary2 <- renderPrint({
  summary(sd2()$sd.diff)
})

output$bootSdBias2 <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), sd = sd(Time)), 
                 sd.diff = diff(sd))$sd.diff)-mean(sd2()$sd.diff)
})

output$bootSdSd2 <- renderText({
  sd(sd2()$sd.diff)
})

ratioList <- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable), 
                      mean = mean(Time)), ratio = mean[1] / mean[2], mean.ratio.diff=mean[1]-mean[2])
)

plotType5 <- function(x, type) {
  switch(type,
         his2 =  qplot(ratioList()$ratio, binwidth=input$w2, 
                       xlab="Ratios", ylab="Frequency", asp=1),
         den2 = qplot(ratioList()$ratio, geom="density", 
                      xlab="Difference in Ratios", ylab="Density", asp=1)
  )}


output$bootRatioHist2 <- renderPlot({
  plotType5(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootRatioSummary2 <- renderPrint({
  summary(ratioList()$ratio)
})

output$bootRatioBias2 <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), 
       mean = mean(Time)), ratio = mean[1] / mean[2], mean.ratio.diff=mean[1]-mean[2])$mean.ratio.diff)
       -mean(ratioList()$mean.ratio.diff)
})

output$bootRatioSd2 <- renderText({
  sd(ratioList()$ratio)
})

sd3 <- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable),
                      sd = sd(Time)), ratio = sd[1]/sd[2], sd.ratio.diff = sd[1]-sd[2])
)

plotType6 <- function(x, type) {
  switch(type,
         his2 =  qplot(sd3()$ratio, binwidth=input$w2, 
                       xlab="Ratios", ylab="Frequency", asp=1, rescale.axis=TRUE),
         den2 = qplot(sd3()$ratio, geom="density", 
                      xlab="Difference in Ratios", ylab="Density", asp=1)
  )}

output$bootSdRatioHist2 <- renderPlot({
  plotType6(do(input$num2) * sample(grouped, replace = TRUE), input$plot2)
})

output$bootSdRatioSummary2 <- renderPrint({
  summary(sd3()$ratio)
})

output$bootSdRatioBias2 <- renderText({
  mean(summarise(summarise(group_by(grouped, Cable), 
                           sd = sd(Time)), ratio = sd[1] / sd[2], sd.ratio.diff=sd[1]-sd[2])$sd.ratio.diff)
  -mean(sd3()$sd.ratio.diff)
})

output$bootSdRatioSd2 <- renderText({
  sd(sd3()$sd.ratio.diff)
})

allStatData<- reactive(
  summarise(summarise(group_by(do(input$num2) * sample(grouped, replace = TRUE), .index, Cable),
                      mean=mean(Time), median=median(Time), sd = sd(Time)), mean.diff=diff(mean), med.diff=diff(median),
            sd.diff=diff(sd), mean.ratio = mean[1] / mean[2], sd.ratio = sd[1]/sd[2], mean.ratio.diff = mean[1]-mean[2], 
            sd.ratio.diff = sd[1]-sd[2])
)

allStatSwitch <- function(x, double) {
  switch(double,
         "bootMean2" = as.data.frame(allStatData())$mean.diff,
         "bootMedian2" = as.data.frame(allStatData())$med.diff,
         "bootSd2" = as.data.frame(allStatData())$sd.diff,
         "bootRatioDiff" = as.data.frame(allStatData())$mean.ratio.diff,
         "bootSdRatio" =as.data.frame(allStatData())$sd.ratio.diff
  )}

level2 <- reactive(
  input$level2
)

alpha2 <- reactive(
  1 - level2()
  )

SE2 <- reactive (
  sqrt((1/input$num2)+(1/input$num2))
)

output$ciPrint2 <- renderPrint({
  quantile((allStatSwitch(allStatData, input$stat2)), 
            probs=c(alpha2()/2, 1-alpha2()/2))
})

output$percLower2 <- renderPrint({
  quantile((allStatSwitch(allStatData, input$stat2)), probs = c(alpha2()))
})

output$percUpper2 <- renderPrint({
  quantile((allStatSwitch(allStatData, input$stat2)), probs = c(1-alpha2()))
})

output$normPrint2 <- renderText({
  c(mean((allStatSwitch(allStatData, input$stat2))) - qnorm(1-alpha2()/2) *  SE2(), 
    (mean(allStatSwitch(allStatData, input$stat2))) + qnorm(1-alpha2()/2) * SE2())
})

output$normLower2 <- renderText({
  c(paste(100*alpha(),'%'), mean(allStatSwitch(allStatData, input$stat2)) 
    - qnorm(level2()) * SE2())
})

output$normUpper2 <- renderText({
  c(paste(100*level(),'%'), mean(allStatSwitch(allStatData, input$stat2))
    + qnorm(level2()) * SE2())
})
})