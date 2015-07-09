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
      switch(input$plot,
                        his =filteredData %>%
                          ggvis(~filteredData()$response) %>%
                          add_axis("x", title = input$response) %>%
                          layer_histograms(width = input_slider(0.1, 1.6, step=0.1, value=0.6)) %>%
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
  qplot(response, data=filteredData(), xlab=input$choose, 
  ylab = "Density", binwidth=input$w) + aes(y=..density..) + geom_density()
})

trials <- reactive({
  
  if(input$goButton > 0) {
    if(input$stat=="bootMean"){
    perms <- do(input$num) * mean(~ response, data = sample(filteredData(), replace = TRUE))
    }
    if(input$stat=="bootMedian"){
    perms <- do(input$num) * median(~ response, data = sample(filteredData(), replace = TRUE))
    }
    if(input$stat=="bootSd"){
    perms <- do(input$num) * sd(~ response, data = sample(filteredData(), replace = TRUE))
    }
    names(perms) <- "perms"
    data.frame(perms)

  }
else {
    data.frame(perms = rep(0, 10))
  }  
})


output$trials <- renderDataTable(trials() %>% head)

observe({
  if(input$reset > 0 ){
    trials <- data.frame(perms=rep(0, 10))
    output$trials <- renderDataTable(data.frame(perms = rep(0, 10)))
  }
  if(input$plot2=="his2"){
    trials %>%
           ggvis(~perms) %>%
           layer_histograms(width = input_slider(0.01, 0.1, step=0.01, value=0.06)) %>%
           bind_shiny("bootHist", "bootHist_ui")}
  if(input$plot2=="den2"){
    trials %>%
           ggvis(~perms) %>%
           layer_densities() %>%
           add_axis("y", title="Density") %>%
           bind_shiny("bootHist", "bootHist_ui")}
  if(input$plot2=="qq2"){
    qqdata2 %>% 
           ggvis(~normal.quantiles, ~diffs) %>% 
           layer_points() %>% 
           add_axis("x", title="Theoretical") %>%
           add_axis("y", title="Sample") %>%
           bind_shiny("bootHist", "bootHist_ui")}
})

qqdata2 <- reactive({
  n <- input$num
  probabilities <- (1:n)/(1+n)
  normal.quantiles <- qnorm(probabilities, mean(trials()$perms, na.rm = T), sd(trials()$perms, na.rm = T))
  qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$perms))
  colnames(qqdata1) <- c("normal.quantiles", "diffs")
  data.frame(qqdata1)
})

output$hisDenPlot2 <- renderPlot ({
  qplot(perms, data=trials(), ylab = "Density", binwidth=input$w2) + aes(y=..density..) + geom_density()
})

output$bootMean <- renderPrint({
round(mean(trials()$perms), digits=3)
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
           bootMean = observed()-mean(trials()$perms),
           bootMedian= observed()-median(trials()$perms),
           bootSd=observed()-sd(trials()$perms)
           )
   signif(biasStat, digits=3)
  })
  
  output$bootSd <- renderText({
    signif(sd(trials()$perms), digits=3)
  })
  
  level <- reactive({
    input$level
  })
  
  alpha <- reactive({
    1-as.numeric(level())
  })
  
  SE <- reactive({
    sd(trials()$perms)
  })

  output$percPrint <- renderText({
    round(quantile(trials()$perms, probs = c(alpha()/2, 1-alpha()/2)), digits=3)
  })
  
  output$percLower <- renderText({
    c(paste(100*alpha(),'%'), round(quantile(trials()$perms, probs = c(alpha())), digits=3))
  })
  
  output$percUpper <- renderText({
    c(paste(100*level(),'%'), round(quantile(trials()$perms, probs = c(1-alpha())),digits=3))
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