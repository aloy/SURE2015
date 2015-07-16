library(shiny)
library(shinyjs)
shinyServer(function(input, output, session){
  
  ## One-Sample Bootstrap
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggvis)
  
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
    range <-diff(range(filteredData()))
      switch(input$plot,
                        his =filteredData %>%
                          ggvis(~filteredData()$response) %>%
                          add_axis("x", title = input$response) %>%
                          layer_histograms(width = input_slider(round(range/100, digits=2), 
                          round(range/5, digits=2), step=round(range/100, digits=2), value=round(range/10, digits=2))) %>%
                          bind_shiny("origHist", "origHist_ui"),
                        den = filteredData %>%
                          ggvis(~filteredData()$response) %>%
                          layer_densities() %>%
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
  })
  
  output$sd <- renderText({
    round(sd(filteredData()), digits=3)
  })

output$hisDenPlot <- renderPlot ({
  ggplot(data=filteredData(), aes(x=response)) + geom_histogram(colour="black", fill="grey19", 
   binwidth=input$w, aes(y=..density..)) + geom_density(colour="blue") + theme(panel.grid.minor = element_line(colour = "grey"), 
  panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), axis.text = element_text(colour = "black"))
})

observe({
  range <- diff(range(filteredData()))
  updateSliderInput(session, 'w', min=round(range/100, digits=2), max=round(range/5, digits=2), 
                    value=round(range/10, digits=2), step=round(range/100, digits=2))
})

observe({
  range2 <- diff(range(trials()$result))
  updateSliderInput(session, 'w2', min=signif(range2/100, digits=2), max=signif(range2/2, digits=2), 
                    value=signif(range2/10, digits=2), step=signif(range2/100, digits=2))
})

trials <- reactive({
  
  if(input$goButton > 0) {
    if(input$stat=="bootMean"){
    result <- do(input$num) * mean(response, data = sample(filteredData(), replace = TRUE))
    }
    if(input$stat=="bootMedian"){
    result <- do(input$num) * median(response, data = sample(filteredData(), replace = TRUE))
    }
    if(input$stat=="bootSd"){
    result <- do(input$num) * sd(response, data = sample(filteredData(), replace = TRUE))
    }
    names(result) <- "result"
    data.frame(result)

  }
else {
    data.frame(result = rep(0, 10))
  }  
})


output$trials <-  renderDataTable(trials(), options = list(pageLength = 10))

observe({
  if(input$reset > 0 ){
    trials <- data.frame(result=rep(0, 10))
    output$trials <- renderDataTable(data.frame(result = rep(0, 10)))
  }
  range3 <-diff(range(trials()))
  switch(input$plot2,
   his2= trials %>%
           ggvis(~result) %>%
           layer_histograms(width = input_slider(signif(range3/100, digits=2), 
                    signif(range3/2, digits=2), step=0.001, value=0.06)) %>%
           bind_shiny("bootHist", "bootHist_ui"),
 den2=
    trials %>%
           ggvis(~result) %>%
           layer_densities() %>%
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
  normal.quantiles <- qnorm(probabilities, mean(trials()$result, na.rm = T), sd(trials()$result, na.rm = T))
  qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$result))
  colnames(qqdata1) <- c("normal.quantiles", "diffs")
  data.frame(qqdata1)
})

output$hisDenPlot2 <- renderPlot ({
  ggplot(data=trials(), aes(x=result)) + geom_histogram(colour="black", fill="grey19", 
  binwidth=input$w2, aes(y=..density..)) + geom_density(colour="blue") + theme(panel.grid.minor = element_line(colour = "grey"), 
 panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), axis.text = element_text(colour = "black"))
})

output$bootMean <- renderText({
round(mean(trials()$result), digits=3)
})

observed <- reactive({
  switch(input$stat,
         bootMean= mean(filteredData()$response),
         bootMedian= median(filteredData()$response),
         bootSd= sd(filteredData()$response)
          )
})

  output$bootBias <- renderText({
    biasStat <- switch(input$stat,
           bootMean = observed()-mean(trials()$result),
           bootMedian= observed()-median(trials()$result),
           bootSd=observed()-sd(trials()$result)
           )
   signif(biasStat, digits=3)
  })
  
  output$bootSd <- renderText({
    signif(sd(trials()$result), digits=3)    
    })
  
  level <- reactive({
    input$level
  })
  
  alpha <- reactive({
    1-as.numeric(level())
  })
  
  SE <- reactive({
    sd(trials()$result)
  })

  output$percPrint <- renderText({
    round(quantile(trials()$result, probs = c(alpha()/2, 1-alpha()/2)), digits=3)
  })
  
  output$percLower <- renderText({
    c(paste(100*alpha(),'%'), round(quantile(trials()$result, probs = c(alpha())), digits=3))
  })
  
  output$percUpper <- renderText({
    c(paste(100*level(),'%'), round(quantile(trials()$result, probs = c(1-alpha())),digits=3))
  })

output$normPrint <- renderText({
  c(round(observed() - qnorm(1-alpha()/2) *  SE(), digits=3),
    round(observed() + qnorm(1-alpha()/2) * SE(), digits=3))
})

output$normLower <- renderText({
  c(paste(100*alpha(),'%'), round(observed()- qnorm(1-alpha()) * SE(), digits=3))
})
  output$normUpper <- renderText({
    c(paste(100*level(),'%'), round(observed() + qnorm(1-alpha()) * SE(), digits=3))
  })
 
})