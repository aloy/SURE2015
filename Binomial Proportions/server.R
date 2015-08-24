library(shiny)
library(shinyjs)
library(ggvis)
library(dplyr)

shinyServer(function(input, output){
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
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
     read.csv("../Binomial Proportions/data/Backpack.csv")
    )
    })
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  
  output$selectVar <- renderUI({
    if(input$factor=="col"){
    cols <- colnames(theData())[sapply(theData(),is.numeric)]
    selectInput("var", label="Column of Trials", cols)
    }else{
      cols <- colnames(theData())[sapply(theData(),is.factor)]
      selectInput("var", label="Column of Trials", cols)
      }
  })
  
  filteredData <- reactive({
    filteredData0 <- data.frame(theData()[,input$var])
    names(filteredData0) <- "data"
    filteredData0
  })
  
  output$factSuccess <- renderUI({
    if(input$factor=="fact"){
      levels <- levels(theData()[,input$var])
      selectInput("success", "Level for Success", levels)
    }
  })
  
  perms <- reactive({
    if(input$goButton>0){
    data.frame(replicate(input$num, rbinom(nrow(filteredData()), 1, prob=input$p0)))  
    }
    })
  
  trials <- reactive({
    if(input$goButton>0){
    trials0 <- data.frame(perms=colMeans(perms()))
    }
    else{
      trials0 <- data.frame(perms=rep(0, 10))
    }
    trials0
  })
  
  observe({
    trials() %>%
      ggvis(~perms) %>%
      add_axis("x", title = "Proportion of Successes") %>%
      layer_histograms(width=input$w) %>%
      bind_shiny("permHist")
})

sampleMean <- reactive({
  if(input$factor=="fact"){
    y <- which(levels(filteredData()$data)==input$success)
    (summary(filteredData()$data)/sum(summary(filteredData()$data)))[y]
  }else{
    mean(filteredData()$data)
  }
})

output$mean <- renderPrint({
sampleMean()
})

output$permMean <- renderPrint({
  mean(trials()$perms)
})

output$pval <- renderPrint({
  n <- input$num
switch(input$test,
       tt = min((sum(trials()$perms>=sampleMean())+1)/(n+1),
                (sum(trials()$perms<=sampleMean())+1)/(n+1))*2,
       lt = (sum(trials()$perms <= sampleMean()) +1)/(n+1),
       ut = (sum(trials()$perms>=sampleMean())+1)/(n+1))
})

output$ci <- renderPrint({
  x <- sum(perms())
  n <- nrow(filteredData())*input$num
  p <- input$p0
  c <- input$ci
  binom.test(x=x, n=n, p=p)$conf.int[1:2]
})

})