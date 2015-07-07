library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
  
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
        d <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else(
      d <- as.data.frame(SleepStudy[,26, drop=FALSE])
    )
    d
  })
  
  observe({
    data <- theData()
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'response', choices = qvars)    
  })
  
  output$contents <- renderTable({
    theData()
  })
  
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "contents", anim = TRUE))
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))

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
  
# filedata %>% 
#                         ggvis(~filedata()$response) %>%
#                         add_axis("x", title = input$choose) %>%
#                         layer_histograms(width = input_slider(0.1, 1.6, step=0.1, value=0.6)) %>% 
#                         bind_shiny("origHist", "origHist_ui")
#                        den = simdata %>% 
#                           ggvis(~filedata()$response) %>% 
#                          layer_densities() %>%
#                           add_axis("x", title = input$choose) %>%
#                           add_axis("y", title="Density") %>%
#                           bind_shiny("origHist", "origHist_ui"),
#                       qq = qqdata %>% 
#                         ggvis(~normal.quantiles, ~diffs) %>% 
#                         layer_points() %>% 
#                         add_axis("x", title="Theoretical") %>%
#                         add_axis("y", title="Sample") %>%
#                         bind_shiny("origHist", "origHist_ui")
#   )
# )
# 
# 
observe({
  if(input$plot=="his"){
filteredData %>%
  ggvis(~filteredData()$response) %>%
  add_axis("x", title = input$response) %>%
  layer_histograms(width = input_slider(0.1, 1.6, step=0.1, value=0.6)) %>%
  bind_shiny("origHist", "origHist_ui")
}
if(input$plot=="den"){
  filteredData %>%
    ggvis(~filteredData()$response) %>%
    layer_densities() %>%
    add_axis("x", title = input$choose) %>%
    add_axis("y", title="Density") %>%
    bind_shiny("origHist", "origHist_ui")
}
if(input$plot=="qq"){
  qq = qqdata %>% 
    ggvis(~normal.quantiles, ~diffs) %>% 
    layer_points() %>% 
    add_axis("x", title="Theoretical") %>%
    add_axis("y", title="Sample") %>%
    bind_shiny("origHist", "origHist_ui")
}
})

output$summary <- renderTable({
  favstats(response, data=filteredData())
#   head(filteredData())
  })
  
  output$sd <- renderText({
    sd(filteredData())
  })

output$hisDenPlot <- renderPlot ({
  qplot(response, data=filteredData(), xlab=input$choose, 
  ylab = "Density", binwidth=input$w) + aes(y=..density..) + geom_density()
})

# trials <- reactive({
#  trials0 <- switch(input$stat,
#     bootMean= do(input$num) * mean(sample(simdata()$Quantitative, replace=TRUE)),
#     bootMedian= do(input$num) * median(sample(simdata()$Quantitative, replace = TRUE)),
#     bootSd= do(input$num) * sd(sample(simdata()$Quantitative, replace = TRUE))
#   )
# as.data.frame(trials0)
# })

# observe(
#   ggSwitch2 <-  switch(input$plot2, 
#                       his2= trials %>% 
#                         ggvis(~trials()$result) %>%
#                         add_axis("x", title = paste(input$choose, "Bootstrap")) %>%
#                         layer_histograms(width = input_slider(0.1, 1.6, step=0.1, value=0.6)) %>% 
#                         bind_shiny("bootHist", "bootHist_ui"),
#                       den2 = trials %>% 
#                         ggvis(~trials()$result) %>% 
#                         layer_densities() %>%
#                         add_axis("x", title = paste(input$choose, "Bootstrap")) %>%
#                         add_axis("y", title="Density") %>%
#                         bind_shiny("bootHist", "bootHist_ui")
# 
#   )
#   )
# 
# qqdata2 <- reactive({
#   n <- input$num
#   probabilities <- (1:n)/(1+n)
#   normal.quantiles <- qnorm(probabilities, mean(trials()$result, na.rm = T), sd(trials()$result, na.rm = T))
#   qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$result))
#   colnames(qqdata1) <- c("normal.quantiles", "diffs")
#   data.frame(qqdata1)
# })
# 
# output$test <- renderPlot({
#   qplot(x=normal.quantiles, y=diffs, data=qqdata2())
# })
# 
# output$hisDenPlot2 <- renderPlot ({
#   qplot(result, data=trials(), xlab=input$choose, 
#         ylab = "Density", binwidth=input$w2) + aes(y=..density..) + geom_density()
# })
# 
# output$bootSummary <- renderTable({
# favstats(~result, data=trials())
# })
# 
#   output$bootBias <- renderText({
#     biasStat <- switch(input$stat,
#            bootMean = mean(simdata()$Quantitative)-mean(trials()$result),
#            bootMedian= median(simdata()$Quantitative)-median(trials()$result),
#            bootSd=sd(simdata()$Quantitative)-sd(trials()$result)
#            )
#    biasStat
#   })
#   
#   output$bootSd <- renderText({
#     sd(trials()$result)
#   })
#   
#   level <- reactive({
#     input$level
#   })
#   
#   alpha <- reactive({
#     1-as.numeric(level())
#   })
#   
#   SE <- reactive({
#     sd(trials()$result)
#   })
#   
# observed <- reactive({
#   switch(input$stat,
#          bootMean= mean(simdata()$Quantitative),
#          bootMedian= median(simdata()$Quantitative),
#          bootSd= sd(simdata()$Quantitative)
#   )
# })
# 
#   output$percPrint <- renderText({
#     quantile(trials()$result, probs = c(alpha()/2, 1-alpha()/2))
#   })
#   
#   output$percLower <- renderText({
#     quantile(trials()$result, probs = c(alpha()))
#   })
#   
#   output$percUpper <- renderText({
#     quantile(trials()$result, probs = c(1-alpha()))
#   })
# 
# output$normPrint <- renderText({
#   c(observed() - qnorm(1-alpha()/2) *  SE(),
#     observed() + qnorm(1-alpha()/2) * SE())
# })
#   output$normUpper <- renderText({
#     c(paste(100*level(),'%'), observed() + qnorm(1-alpha()) * SE())
#   })
#   
#   output$normLower <- renderText({
#     c(paste(100*alpha(),'%'), observed()- qnorm(1-alpha()) * SE())
#   })
 
})