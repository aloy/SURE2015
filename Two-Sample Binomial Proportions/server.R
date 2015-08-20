library(shiny)
library(shinyjs)
library(ggvis)
library(Stat2Data)
library(dplyr)

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
  
  tab <- reactive({
    table(theData()[,input$var], theData()[,input$var2])
  })
  
  output$orig <- renderTable({
    tab()
  })
  
  output$test <- renderPrint({
    switch(input$hyp,
           tt=prop.test(tab()),
           lt=prop.test(tab(), alternative="l"),
             ut=prop.test(tab(), alternative="g")
           )
  })
  
  perms1 <- reactive({
    if(input$goButton>0){
    data.frame(replicate(input$num, rbinom(nrow(theData()), 1, prob=tab()[1]/(tab()[1]+tab()[2]))))
    }else{
      data.frame(rep(1:2, 5))
    }
  })
  
  perms2 <- reactive({
    if(input$goButton>0){
    data.frame(replicate(input$num, rbinom(nrow(theData()), 1, prob=tab()[3]/(tab()[3]+tab()[4]))))
    }else{
      data.frame(rep(1:2, 5))
    }
  })
  
  trials <- reactive({
    if(input$goButton>0){
    trials1 <- data.frame(pop1 = colMeans(perms1()), pop2=colMeans(perms2()))
    trials0 <- data.frame(melt(trials1))
    }else{
      trials0 <- data.frame(value=rep(0, 10), variable=rep("a", 10))
    }
    trials0
  })

  observe({
  trials %>% group_by(variable) %>% 
    ggvis(~value) %>% layer_histograms(fill=~variable) %>% bind_shiny("permsHist")
  })

output$propDiff <- renderPrint({
  (tab()[1]/(tab()[1]+tab()[2])) - (tab()[3]/(tab()[3]+tab()[4]))
})

output$permDiff <- renderPrint({
  summarise(summarise(group_by(trials(), variable), mean=mean(value)), mean.diff=diff(mean))
})

output$confInt <- renderPrint({
  switch(input$hyp,
         tt=prop.test(tab(), conf.level=input$ci)$conf.int[1:2],
         lt=prop.test(tab(), alternative="l",conf.level=input$ci)$conf.int[1:2],
         ut=prop.test(tab(), alternative="g",conf.level=input$ci)$conf.int[1:2]
         )
})
  
  })