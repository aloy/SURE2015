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
  
  perms1 <- reactive({
    if(input$goButton>0){
    data.frame(replicate(input$num, rbinom(nrow(theData()), 1, prob=tab()[3]/(tab()[3]+tab()[1]))))
    }else{
      data.frame(rep(1:2, 5))
    }
  })
  
  perms2 <- reactive({
    if(input$goButton>0){
    data.frame(replicate(input$num, rbinom(nrow(theData()), 1, prob=tab()[4]/(tab()[2]+tab()[4]))))
    }else{
      data.frame(rep(1:2, 5))
    }
  })
  
  trials <- reactive({
    if(input$goButton>0){
    trials1 <- data.frame(pop1 = colMeans(perms1()), pop2=colMeans(perms2()))
    trials0 <- mutate(trials1, diff=(pop1-pop2))
    }else{
      trials0 <- data.frame(diff=rep(0, 10))
    }
    trials0
  })

observe({
  trials %>% ggvis(~diff) %>% layer_histograms() %>% bind_shiny("permsHist")
})

output$propDiff <- renderPrint({
  (tab()[3]/(tab()[1]+tab()[3])) - (tab()[4]/(tab()[2]+tab()[4]))
})

output$permDiff <- renderPrint({
  mean(trials()$diff)
})

output$test <- renderPrint({
  prop.test(tab())
})

## Alternative idea–-sample binomial distribution to get a distribution centered at the 
## p_1-p_2 difference, then test with the sample proportion as null hypothesis
# permsTest <- reactive ({ replicate(input$num, rbinom(nrow(theData()), 1, prob=input$p1p2)) })
# trialsTest <- reactive({ data.frame(test=colMeans(permsTest)) })
#  x <- sum(permsTest())
# n <- nrow(theData())*input$num
#  p <-  (tab()[3]/(tab()[1]+tab()[3])) - (tab()[4]/(tab()[2]+tab()[4]))
# binom.test(x=x, n=n, p=p)
## 


output$confInt <- renderPrint({
         prop.test(tab(), conf.level=input$ci)$conf.int[1:2]
})
  
  })