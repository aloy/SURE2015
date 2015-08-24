library(shiny)
library(shinyjs)
library(ggvis)
library(Stat2Data)
library(dplyr)
library(mosaic)

#Two-Sample Binomial Proportions

shinyServer(function(input, output){  
  data(Hoops)
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  shiny::observe({
    toggle(id = "suggest", condition = input$chooseData=="uploadNo")
  })
  
  theData <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      return(      
        read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else{
      data.frame(Hoops)
    }
  })
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  
  output$selectPop <- renderUI({
      cols <- colnames(theData())
      selectInput("var", label="Column of Two Populations", cols)
  })
  
  output$selectDiff <- renderUI({
      cols <- colnames(theData())
      selectInput("var2", label="Column for Proportions", cols)
    })
  
  filteredData <- reactive({
    data0 <- data.frame(theData()[,input$var], theData()[,input$var2])
    names(data0) <- c("pop", "prop")
    data0
  })
  
  tab <- reactive({
    table(filteredData()$pop, filteredData()$prop)
  })
  
  output$orig <- renderTable({
    tab()
  })
  
  trials <- reactive({
    if(input$goButton>0){
      perms <- do(input$num) * diff(mean(prop ~ shuffle(pop), data = filteredData()))
      trials0 <- data.frame(perms)
      names(trials0) <- "diff"
    }else{
      trials0 <- data.frame(diff=rep(0, 10))
    }
    trials0
  })

observe({
  trials %>% ggvis(~diff) %>% layer_histograms(width=input$w) %>% bind_shiny("permsHist")
})

output$propDiff <- renderPrint({
  (tab()[3]/(tab()[1]+tab()[3])) - (tab()[4]/(tab()[2]+tab()[4]))
})

output$permDiff <- renderPrint({
  mean(trials()$diff)
})

output$pval <- renderPrint({
  n <- input$num
  switch(input$test,
  tt = min((sum(trials()$diff>=0)+1)/(n+1),
           (sum(trials()$diff<=0)+1)/(n+1))*2,
  lt = (sum(trials()$diff <= 0) +1)/(n+1),
  ut = (sum(trials()$diff>=0)+1)/(n+1)
  )
})

output$confInt <- renderPrint({
#          prop.test(tab(), conf.level=input$ci)$conf.int[1:2]
})
  
  })