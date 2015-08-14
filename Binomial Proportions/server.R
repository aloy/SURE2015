library(shiny)
library(shinyjs)

shinyServer(function(input, output){
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  theData <- reactive({
    if(input$method=="data"){
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
    }
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
  
  p <- reactive({
    if(input$method=="num"){
     input$success/input$trials
    }else{
     mean(theData()[,input$var]) 
    }
  })
  
  output$p <- renderPrint({
    p()
  })
  
  k <- reactive({
    switch(input$method,
           num=input$success,
           data=mean(filteredData()$data)*nrow(filteredData()))
  })
  
  n <- reactive({
    switch(input$method,
           num=input$trials,
           data=nrow(filteredData()))
  })
  
  output$testPrint <- renderPrint({
    switch(input$test,
           exact=dbinom(k(),n(),p()),
           lt=pbinom(k(), n(), p()),
           ut=pbinom(k(), n(), p(), lower.tail=FALSE)
           )
  })
  
})