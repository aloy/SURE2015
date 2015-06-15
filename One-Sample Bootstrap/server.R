shinyServer(function(input,output){
  library(mosaic)
  library(Lock5Data)
  data(SleepStudy)
  original_data <- SleepStudy$AverageSleep
  
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