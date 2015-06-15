shinyServer(function(input,output){
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggvis)
  
  data(SleepStudy)
  original_data <- SleepStudy$AverageSleep

trials <- function(x, list) {
switch(list,
       bootMean = do(1000) * mean(sample(original_data, replace = TRUE)),
       med = do(1000) * median(sample(original_data, replace = TRUE)),
       sdev = do(1000) * sd(sample(original_data, replace = TRUE))
  )
}

output$bootHist <- renderPlot({
  result <- trials$result
  hist(result)
})
  
  output$origHist <- renderPlot({
   hist(original_data, col = 'darkgray', border = 'white', xlab="Count", 
        ylab="Hours of Sleep", main=paste("Original Sample"))
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