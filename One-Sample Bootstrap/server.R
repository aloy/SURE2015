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
    mean(original_data)-mean(trialsFunction(original_data, input$stat)$result)
  })
  
  output$bootSd <- renderPrint({
    sd(trialsFunction(original_data, input$stat)$result)
  })

level <- 0.95
alpha <- 1 - level  
  
  ciType <- function(x, double) {
    switch(double,
           perc =  quantile(trialsFunction(original_data, input$stat)$result, 
                            probs = c(alpha/2, 1-alpha/2)),
           norm = c(mean(original_data) - qnorm(1 - alpha/2) * 
             sd(trialsFunction(original_data, input$stat)$result), mean(original_data) + qnorm(1 - alpha/2) * 
               sd(trialsFunction(original_data, input$stat)$result))
    )}
  
  output$ciPrint <- renderPrint({
    ciType(trialsFunction(original_data, input$stat)$result, input$ci)
  })

  output$percLower <- renderPrint({
    quantile(trialsFunction(original_data, input$stat)$result, probs = c(alpha))
  })

output$percUpper <- renderPrint({
  quantile(trialsFunction(original_data, input$stat)$result, probs = c(1-alpha))
})

output$ciPrint2 <- renderPrint({
  ciType(trialsFunction(original_data, input$stat)$result, input$ci)
})
  
})