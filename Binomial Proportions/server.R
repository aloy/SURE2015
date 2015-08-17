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
  
  shiny::observe({
    toggle(id = "warning", condition = input$success > input$trials)
  })
  
  output$selectVar <- renderUI({
    cols <- colnames(theData())[sapply(theData(),is.numeric)]
    selectInput("var", label="Column of Trials", cols)
  })
  
  filteredData <- reactive({
    filteredData0 <- data.frame(theData()[,input$var])
    names(filteredData0) <- "data"
    filteredData0
  })
  
  p0 <- reactive({
     input$p0
  })
  
  perms <- reactive({
    if(input$goButton>0){
    data.frame(replicate(1000, rbinom(nrow(filteredData()), 1, prob=mean(filteredData()$data))))  
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

output$sampleMean <- renderPrint({
  mean(filteredData()$data)
})

output$permMean <- renderPrint({
  mean(trials()$perms)
})

output$pval <- renderPrint({
  x <- sum(perms())
  n <- nrow(filteredData())*input$num
  p <- input$p0
prop.test(x=x, n=n, p=p)$p.value
})

})