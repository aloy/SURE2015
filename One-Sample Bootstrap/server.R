shinyServer(function(input,output){
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
  
  trialsMean <- do(1000) * mean(sample(original_data, replace = TRUE))
  trialsMedian <- do(1000) * median(sample(original_data, replace = TRUE))
  trialsSdev <- do(1000) * sd(sample(original_data, replace = TRUE))
  
  trialsFunction <- function(x, list) {
    switch(list,
           bootMean = trialsMean,
           bootMedian = trialsMedian,
           bootStdDev = trialsSdev
    )
  }

  plotType <- function(x, type) {
    switch(type,
           his =  trialsFunction(original_data, input$stat) %>% 
             ggvis(~result) %>% 
             layer_histograms(binwidth = input$w) %>% 
             add_axis("x", title = paste("Bootstrap of", input$stat)),
           den = trialsFunction(original_data, input$stat) %>% 
             ggvis(~result) %>% 
             layer_densities() %>%
             add_axis("x", title = paste("Kernel Bootstrap of", input$stat))
   )}
  
  output$ggBoot <- renderPlot({
    plotType(original_data, input$plot)
  })
  
  output$bootSummary <- renderPrint({
    summary(trialsFunction(original_data, input$stat)$result)
  })

  output$bootBias <- renderPrint({
    mean(original_data) - mean(trialsFunction(original_data, input$stat)$result)
  })
  
  output$bootSd <- renderPrint({
    sd(trialsFunction(original_data, input$stat)$result)
  })
  
})