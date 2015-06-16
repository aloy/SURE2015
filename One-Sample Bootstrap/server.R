shinyServer(function(input,output){
  
  ## One-Sample Bootstrap
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggvis)
  
  data(SleepStudy)
  original_data <- SleepStudy$AverageSleep
  
  output$origHist <- renderPlot({
    w <- input$w
    qplot(original_data, xlab="Hours of Sleep", ylab="Frequency", main="Original Sample",)
  })
  output$summary <- renderPrint({
    summary(original_data)
  })
  output$mean <- renderPrint({
    mean(original_data)
  })
  output$sd <- renderPrint({
    sd(original_data)
  })
  
  trialsFunction <- function(x, list) {
    switch(list,
           bootMean = do(input$num) * mean(sample(original_data, replace = TRUE)),
           bootMedian = do(input$num) * median(sample(original_data, replace = TRUE)),
           bootStdDev = do(input$num) * sd(sample(original_data, replace = TRUE))
    )
  }

  plotType <- function(x, type) {
    switch(type,
           his =  qplot(trialsFunction(original_data, input$stat)$result, geom="histogram", 
                        binwidth=input$w, xlab=paste("Bootstrap of", input$stat), ylab="Frequency"),
           den = qplot(trialsFunction(original_data, input$stat)$result, geom="density",
                       xlab=paste(input$stat, "Hours of sleep"), ylab="Density")
   )}
  
  output$bootHist <- renderPlot({
    plotType(original_data, input$plot)
  })
  
 output$bootSummary <- renderPrint({
    summary(trialsFunction(original_data, input$stat)$result)
  })

  output$bootBias <- renderPrint({
    mean(original_data)-mean(trialsFunction(original_data, input$stat)$result)
  })
  
  output$bootSd <- renderPrint({
    sd(trialsFunction(original_data, input$stat)$result)
  })
  
  ciType <- function(x, double) {
    switch(double,
           perc =  quantile(trialsFunction(original_data, input$stat)$result, 
                            probs = c((1-input$level)/2, (input$level)/2)),
           norm = c(mean(original_data) - qnorm(input$level/2) * 
             sd(trialsFunction(original_data, input$stat)$result), mean(original_data) + qnorm(input$level/2) * 
               sd(trialsFunction(original_data, input$stat)$result))
    )}
  
  output$ciPrint <- renderPrint({
    ciType(trialsFunction(original_data, input$stat)$result, input$ci)
  })

  output$percLower <- renderPrint({
    quantile(trialsFunction(original_data, input$stat)$result, probs = c(1-input$level))
  })

output$percUpper <- renderPrint({
  quantile(trialsFunction(original_data, input$stat)$result, probs = input$level)
})

output$normPrint <- renderPrint({
  ciType(trialsFunction(original_data, input$stat)$result, input$ci)
})

output$normUpper <- renderPrint({
  mean(original_data) + qnorm(input$level) * sd(trialsFunction(original_data, input$stat)$result)
})

output$normLower <- renderPrint({
  mean(original_data) - qnorm(input$level) * sd(trialsFunction(original_data, input$stat)$result)
})

## Two-Sample Bootstrap
tv <- read.csv("../data/TV.csv")
grouped <- group_by(tv, Cable)

plotType2 <- function(x, type) {
  switch(type,
         his =  qplot(summarise(summarise(group_by(do(input$num) * sample(grouped, replace = TRUE), .index, Cable),
                  mean = mean(Time)), mean.diff = diff(mean))$mean.diff, binwidth=input$w, 
                  xlab="Means", ylab="Frequency"),
         den = qplot(summarise(summarise(group_by(do(input$num) * sample(grouped, replace = TRUE), .index, Cable), 
                  mean = mean(Time)), mean.diff = diff(mean))$mean.diff, geom="density", 
                  xlab="Difference in Means", ylab="Density")
  )}

output$bootHist2 <- renderPlot({
  plotType2(do(input$num) * sample(grouped, replace = TRUE), input$plot)
})

})
