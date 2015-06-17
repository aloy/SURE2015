shinyServer(function(input,output){
  
  ## One-Sample Bootstrap
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggvis)
  library(shiny)
  
  data(SleepStudy)
  original_data <- SleepStudy$AverageSleep
  
  output$origHist <- renderPlot({
    w <- input$w
    qplot(original_data, xlab="Hours of Sleep", ylab="Frequency", main="Original Sample",)
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
           bootStdDev = do(input$num) * sd(sample(original_data, replace = TRUE))
    )
  }

  
  trials <- reactive(
    trialsFunction(original_data, input$stat)$result
  )
  
  plotType <- function(x, type) {
    switch(type,
           his =  qplot(trials(), geom="histogram", 
                        binwidth=input$w, xlab=paste("Bootstrap of", input$stat), ylab="Frequency"),
           den = qplot(trials(), geom="density",
                       xlab=paste(input$stat, "Hours of sleep"), ylab="Density")
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

level <- reactive(
  input$level
)

alpha <- reactive(
  1- level()
  )

observed <- reactive(
mean(original_data)
  )

SE <- reactive (
sd(trials())
  )
  
  ciType <- function(x, double) {
    switch(double,
           perc =  quantile(trials(), probs = c(alpha()/2, 1-alpha()/2)),
           norm = c(observed() - qnorm(1 - alpha()/2) *  SE, observed() + qnorm(1 - alpha()/2) * SE())
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

output$normPrint <- renderPrint({
  ciType(trials(), input$ci)
})

output$normUpper <- renderPrint({
  observed() + qnorm(input$level) * SE()
})

output$normLower <- renderPrint({
  observed() - qnorm(input$level) * SE()
})

## Two-Sample Bootstrap
tv <- read.csv("../data/TV.csv")
grouped <- group_by(tv, Cable)

diffs <- reactive(
  summarise(summarise(group_by(do(input$num) * sample(grouped, replace = TRUE), .index, Cable),
                      mean = mean(Time)), mean.diff = diff(mean))
  )


plotType2 <- function(x, type) {
  switch(type,
         his =  qplot(diffs()$mean.diff, binwidth=input$w, 
                  xlab="Means", ylab="Frequency"),
         den = qplot(diffs()$mean.diff, geom="density", 
                  xlab="Difference in Means", ylab="Density")
  )}

output$bootHist2 <- renderPlot({
  plotType2(do(input$num) * sample(grouped, replace = TRUE), input$plot)
})

output$bootSummary2 <- renderPrint({
  summary(diffs()$mean.diff)
})

# output$bootBias2 <- renderText({
#   mean(grouped)-mean(summarise(summarise(group_by(do(input$num) * sample(grouped, replace = TRUE), .index, Cable),
#                                                mean = mean(Time)), mean.diff = diff(mean))$mean.diff)
# })

# output$bootSd2 <- renderText({
#   sd((summarise(summarise(group_by(do(input$num) * sample(grouped, replace = TRUE), .index, Cable),
#                                        mean = mean(Time)), mean.diff = diff(mean))$mean.diff))
# })

## Not sure if these are calculated correctly (using mean(grouped))

})
