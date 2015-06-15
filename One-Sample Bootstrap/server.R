shinyServer(function(input,output){
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggvis)
  
  data(SleepStudy)
  original_data <- SleepStudy$AverageSleep
  
  trialsMean <- do(1000) * mean(sample(original_data, replace = TRUE))
  trialsMedian <- do(1000) * median(sample(original_data, replace = TRUE))
  trialsSdev <- do(1000) * sd(sample(original_data, replace = TRUE))
  
  trialsFunction <- function(x, list) {
    switch(list,
           bootMean = trialsMean,
           med = trialsMedian,
           sdev = trialsSdev
    )
  }
  
  output$bootHist <- renderPlot({
    stat <- input$stat
    hist(trialsFunction(original_data, input$stat)$result, col = 'darkgray', border = 'white', 
         xlab=stat, main=paste("Bootstrap of", stat))
  })
  
  output$origHist <- renderPlot({
    hist(original_data, col = 'darkgray', border = 'white', xlab="Hours of Sleep",
         ylab="Frequency", main="Original Sample")
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
  
})