library(shiny)
library(shinyjs)
library(mosaic)
library(Lock5Data)
library(dplyr)
library(ggvis)
library(resample)
shinyServer(function(input, output, session){
  
  ## One-Sample Bootstrap
  
  data("SleepStudy")
  
  theData <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      return(      
        read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else(
    as.data.frame(SleepStudy[,26, drop=FALSE])
    )
  })
  
  observe({
    data <- theData()
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'response', choices = qvars)    
  })
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "trials", anim = TRUE))
  filteredData<-reactive({
    data<-isolate(theData())
    #if there is no input, make a dummy dataframe
    if(input$response=="response"){
      if(is.null(data)){
        data<-data.frame(response=0)
      }
    }else{
      data <- data.frame(data[,c(input$response)])
      names(data)<-c("response")
    }
    data
  })
  
  qqdata <- reactive({
    n <- nrow(filteredData())
    probabilities <- (1:n)/(1+n)
    normal.quantiles <- qnorm(probabilities, mean(filteredData()$response, na.rm = T), 
                              sd(filteredData()$response, na.rm = T))
    qqdata0 <- data.frame(sort(normal.quantiles), sort(filteredData()$response))
    colnames(qqdata0) <- c("normal.quantiles", "diffs")
    data.frame(qqdata0)
  })
  
  observe({
      if (round(diff(range(filteredData()))/100, digits=2) == 0){
        range.100 <- 0.01
    }
    else(
      range.100 <- round(diff(range(filteredData()))/100, digits=2)
    )
      switch(input$plot,
                        his =filteredData %>%
                          ggvis(~filteredData()$response) %>%
                          add_axis("x", title = input$response) %>%
                          layer_histograms(width = input_slider(range.100, 
                          range.100*20, step=range.100, value=range.100*10)) %>%
                          bind_shiny("origHist", "origHist_ui"),
                        den = filteredData %>%
                          ggvis(~filteredData()$response) %>%
                          layer_densities(fill := "dodgerblue") %>%
                          add_axis("x", title = input$choose) %>%
                          add_axis("y", title="Density") %>%
                          bind_shiny("origHist", "origHist_ui"),
                        qq = qqdata %>% 
                          ggvis(~normal.quantiles, ~diffs) %>% 
                          layer_points() %>% 
                          add_axis("x", title="Theoretical") %>%
                          add_axis("y", title="Sample") %>%
                          bind_shiny("origHist", "origHist_ui")
      )
  })
output$summary <- renderTable({
  favstats(response, data=filteredData())
  }, digits = 3)

output$hisDenPlot <- renderPlot ({
  ggplot(data=filteredData(), aes(x=response)) + geom_histogram(colour="black", fill="grey19", 
   binwidth=input$w, aes(y=..density..)) + geom_density(colour="royalblue", fill="royalblue", alpha=0.5) + theme(panel.grid.minor = element_line(colour = "grey"), 
  panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), axis.text = element_text(colour = "black"))
})

observe({
  if (round(diff(range(filteredData()))/100, digits=2) == 0){
    range.100 <- 0.01
  }
  else(
    range.100 <- round(diff(range(filteredData()))/100, digits=2)
  )
  updateSliderInput(session, 'w', min=range.100, max=range.100*20, 
                    value=range.100*10, step=range.100)
})

bootResult <- reactive({
  if(input$goButton > 0) {
    if(input$stat=="bootMean"){
      BS <- bootstrap(filteredData(), mean(response), 
                      R = input$num, statisticNames = "Mean")
    }
    if(input$stat=="bootMedian"){
      BS <- bootstrap(filteredData(), median(response), 
                      R = input$num, statisticNames = "Median")
    }
    if(input$stat=="bootSd"){
      BS <- bootstrap(filteredData(), sd(response), 
                      R = input$num, statisticNames = "Std Dev")
    }
    BS
  } 
})

trials <- reactive({
  if(input$goButton > 0) {
    result <- cbind(index = 1:bootResult()$R, result = bootResult()$replicates)
    colnames(result) <- c("index", "statistic")
    as.data.frame(result)
  } else {
    data.frame(statistic = rep(0, 10))
  }
})

output$trials <-  renderDataTable(trials(), options = list(pageLength = 10))


observe({
  
  if(input$goButton > 0){
    range2.100 <- round(diff(range(trials()$statistic))/100, digits=3)
    if (round(diff(range(trials()$statistic))/100, digits=3) == 0)
      range2.100 <- 0.001
  }
  else(
    range2.100 <- 0.001
    )
  switch(input$plot2,
   his2= trials %>%
           ggvis(~statistic) %>%
     layer_histograms(width = input_slider(range2.100, range2.100*50,
                      value=range2.100*10, step=range2.100)) %>%
           bind_shiny("bootHist", "bootHist_ui"),
 den2=
    trials %>%
           ggvis(~statistic) %>%
           layer_densities(fill := "dodgerblue") %>%
           add_axis("y", title="Density") %>%
           bind_shiny("bootHist", "bootHist_ui"),
  qq2=
    qqdata2 %>% 
           ggvis(~normal.quantiles, ~diffs) %>% 
           layer_points() %>% 
           add_axis("x", title="Theoretical") %>%
           add_axis("y", title="Sample") %>%
           bind_shiny("bootHist", "bootHist_ui")
  )
})

qqdata2 <- reactive({
  n <- input$num
  probabilities <- (1:n)/(1+n)
  normal.quantiles <- qnorm(probabilities, mean(trials()$statistic, na.rm = T), sd(trials()$statistic, na.rm = T))
  qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$statistic))
  colnames(qqdata1) <- c("normal.quantiles", "diffs")
  data.frame(qqdata1)
})

observe({
  if(input$goButton > 0){
    range2.100 <- round(diff(range(trials()$statistic))/100, digits=3)
    if (round(diff(range(trials()$statistic))/100, digits=3) == 0)
      range2.100 <- 0.001
  }
  else(
    range2.100 <- 0.001
  )
  
  updateSliderInput(session, 'w2', min=range2.100, max=range2.100*50, 
                    value=range2.100*10, step=range2.100)
})

output$hisDenPlot2 <- renderPlot ({
  ggplot(data=trials(), aes(x=statistic)) + 
    geom_histogram(colour="black", fill="grey19", binwidth=input$w2, aes(y=..density..)) + 
    geom_density(colour="royalblue", fill="royalblue", alpha=0.5) + 
    theme(panel.grid.minor = element_line(colour = "grey"), 
          panel.background = element_rect(fill = "white"), 
          axis.line = element_line(colour="black"), 
          axis.text = element_text(colour = "black"))
})

output$bootMean <- renderText({
# round(mean(trials()$statistic), digits=3)
  bootResult()$stats$Mean
})

# observed <- reactive({
#   switch(input$stat,
#          bootMean= mean(filteredData()$response),
#          bootMedian= median(filteredData()$response),
#          bootSd= sd(filteredData()$response)
#           )
# })

output$origStat <- renderText({ 
  # round(mean(trials()$statistic), digits=3)
  bootResult()$observed
})

  output$bootBias <- renderText({
#     biasStat <- switch(input$stat,
#            bootMean = observed()-mean(trials()$statistic),
#            bootMedian= observed()-median(trials()$statistic),
#            bootSd=observed()-sd(trials()$statistic)
#            )
#    signif(biasStat, digits=3)
    bootResult()$stats$Bias
  })
  
  output$bootSd <- renderText({
    # signif(sd(trials()$statistic), digits=3)    
    bootResult()$stats$SE
  })
  
  level <- reactive({
    input$level
  })
  
  alpha <- reactive({
    1-as.numeric(level())
  })
  
#   SE <- reactive({
#     sd(trials()$statistic)
#   })

  output$percPrint <- renderPrint({
    # round(quantile(trials()$statistic, probs = c(alpha()/2, 1-alpha()/2)), digits=3)
    CI.percentile(bootResult(), probs = c(alpha()/2, 1-alpha()/2))
  })
  
  output$percLower <- renderPrint({
    # c(paste(100*alpha(),'%'), round(quantile(trials()$statistic, probs = c(alpha())), digits=3))
    CI.percentile(bootResult(), probs = alpha())
  })
  
  output$percUpper <- renderPrint({
    # c(paste(100*level(),'%'), round(quantile(trials()$statistic, probs = c(1-alpha())),digits=3))
    CI.percentile(bootResult(), probs = 1-alpha())
  })

output$tPrint <- renderPrint({
#   c(round(observed() - qnorm(1-alpha()/2) *  SE(), digits=3),
#     round(observed() + qnorm(1-alpha()/2) * SE(), digits=3))
  CI.t(bootResult(), probs = c(alpha()/2, 1-alpha()/2))
})

output$tLower <- renderPrint({
  # c(paste(100*alpha(),'%'), round(observed()- qnorm(1-alpha()) * SE(), digits=3))
  CI.t(bootResult(), probs = alpha())
})

output$tUpper <- renderPrint({
  # c(paste(100*level(),'%'), round(observed() + qnorm(1-alpha()) * SE(), digits=3))
  CI.t(bootResult(), probs = 1-alpha())
})
 
})