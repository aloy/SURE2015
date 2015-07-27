library(mosaic)
library(Lock5Data)
library(shinyjs)
library(nullabor)
data(RestaurantTips)

# Visual Inference

shinyServer(function(input, output, session) {
  theData <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      return(      
        read.csv(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else
      data.frame(RestaurantTips)
  })
  
  shiny::observe({
    toggle(id = "warning", condition = input$x==input$y)
  })
  shiny::observe({
    toggle(id = "warning2", condition = input$x==input$y)
  })
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  shinyjs::onclick("trueData",
                   shinyjs::toggle(id = "plotPos", anim = TRUE))
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  
  observe({
    data <- theData()
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'x', choices = qvars)
    updateSelectInput(session, 'y', choices = qvars)
  })
  
  filteredData <-reactive({
    data<-isolate(theData())
    if(input$x=="x" && input$y=="y"){
      if(is.null(data)){
        data<-data.frame(x = rep(0, 10), y = rep(0, 10))
      }
    }else{
      data <- data[,c(input$x,input$y)]
    }
    names(data)<-c("x","y")
    data.frame(data)
  })
  
  lineupPlot <- reactive({
    n <- input$num
    switch(input$plot,
           scatter= qplot(x, y, data=filteredData()) %+% 
             lineup(null_lm(y~x), filteredData(), n=n, pos=sample(n, 1)) + facet_wrap(~.sample)
    )
  })
  
  output$lineup <- renderPlot({
 lineupPlot()
  })
  
  output$plotPos <- renderText({
    paste("The true data is in plot ",attr(lineupPlot()$data, "pos"),".", sep="")
    })
})