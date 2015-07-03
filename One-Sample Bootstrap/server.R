library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
  
  ## One-Sample Bootstrap
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggvis)
  
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
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  output$varChoose <- renderUI({
    df <- filedata()
    if (is.null(df)) 
      return(NULL)
    vars=names(df)
    names(vars)=vars
    selectInput("boot","Choose a quantitative variable to examine:",vars)
  })
  
  simdata <- reactive({
    quantName <- input$choose2
    Quantitative <- filedata()[,quantName]
    data.frame(Quantitative)  
  })

  qqdata <- reactive({
    n <- input$num
    probabilities <- (1:n)/(1+n)
    normal.quantiles <- qnorm(probabilities, mean(simdata(), na.rm = T), 
                              sd(simdata(), na.rm = T))
    qqdata0 <- data.frame(sort(normal.quantiles), sort(simdata()))
    colnames(qqdata0) <- c("normal.quantiles", "data")
    data.frame(qqdata0)
  })
  
observe(
  ggSwitch <-  switch(input$plot, 
                       his =  simdata %>%
                         ggvis(~Quantitative) %>%
                         layer_histograms(width = input_slider(0.1, 2, step=0.1, value=0.6)) %>%
                         bind_shiny("origHist", "origHist_ui"),
                       den = simdata %>% 
                          ggvis(~Quantitative) %>% 
                         layer_densities() %>%
                          add_axis("x", title = "Mean Difference") %>%
                          add_axis("y", title="Density") %>%
                          bind_shiny("origHist", "origHist_ui")
  )
)
  
  output$summary <- renderTable({
    favstats(~Quantitative|Category)  
    })
  
  output$sd <- renderText({
    sd(Quantitative)
  })
  

output$hisDenPlot <- renderPlot ({
  qplot(Quantitative, ylab = "Density", binwidth=input$w2) + aes(y=..density..) + geom_density()
})


trials <- reactive({
  if(input$stat=="bootMean"){
    trials0 <- do(input$num) * mean(sample(Quantitative, replace = TRUE))
  }
  if(input$stat=="bootMedian"){
    trials0 <-  do(input$num) * median(sample(Quantitative, replace = TRUE))
}
  if(input$stat=="bootSd"){
    trials0 <-  do(input$num) * sd(sample(Quantitative, replace = TRUE))
}
colnames(trials0) <- "result"
data.frame(trials0)
})

observe(
  ggSwitch2 <-  switch(input$plot, 
                      his =  trials %>%
                        ggvis(~result) %>%
                        layer_histograms(width = input_slider(0.005, 0.1, step=0.005, value=0.05)) %>%
                        bind_shiny("bootHist", "bootHist_ui"),
                      den = trials %>%
                        ggvis(~result) %>%
                        layer_densities() %>%
                        add_axis("x", title = "Mean Difference") %>%
                        add_axis("y", title="Density") %>%
                        bind_shiny("bootHist", "bootHist_ui")
  )
)
  
  output$bootSummary <- renderTable({
summary(trials())
})
  
  output$bootBias <- renderText({
    mean(Quantitative)-mean(trials())
  })
  
  output$bootSd <- renderText({
    sd(trials())
  })
  
  level <- reactive({
    input$level
  })
  
  alpha <- reactive({
    1- input$level
  })
  
  SE <- reactive({
    sd(trials())
  })
  
  ciType <- function(x, double) {
    switch(double,
           perc =  quantile(trials(), probs = c(alpha()/2, 1-alpha()/2)),
           norm = c(mean(Quantitative) - qnorm(1-alpha()/2) *  SE(), mean(Quantitative) + qnorm(1-alpha()/2) * SE())
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
    c(paste(100*level(),'%'), mean(Quantitative) + qnorm(1-alpha()) * SE())
  })
  
  output$normLower <- renderText({
    c(paste(100*alpha(),'%'), mean(Quantitative)- qnorm(1-alpha()) * SE())
  })
 
})