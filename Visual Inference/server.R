library(Lock5Data)
library(shinyjs)
library(ggplot2)
library(car)
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
    toggle(id = "warning", condition = (input$x==input$y || input$x2==input$y2) && input$plot!="qq")
  })
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  shinyjs::onclick("trueData",
                   shinyjs::toggle(id = "plotPos", anim = TRUE))
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))

  observe({
  qvars <- colnames(theData())[sapply(theData(),is.numeric)]
  testFactor <- function(x){
    f <- colnames(x)[sapply(x,is.factor)]
    i <- colnames(x)[sapply(x, is.integer)]
    c <- colnames(x)[sapply(x, is.character)]
    return(c(f, i, c))
  }
  updateSelectInput(session,"x", choices=qvars)
  updateSelectInput(session,"y", choices=qvars)
  updateSelectInput(session,"x2", choices=testFactor(theData()))
  updateSelectInput(session,"y2", choices=qvars)
  if(input$plot=="box"||input$plot=="den"){
    updateSelectInput(session, "y", label="Response Variable")
  }
  if(input$plot=="scatterSmooth"||input$plot=="resid"||input$plot=="qq"){
    updateSelectInput(session, "x", selected=input$x)
    updateSelectInput(session, "y",selected=input$y)
  }
  })
#   
#   observe({
#     qvars <- colnames(theData())[sapply(theData(),is.numeric)]
#     if(input$plot=="scatterSmooth"||input$plot=="resid"){
#       updateSelectInput(session, "x", label="X Variable", choices=input$x)
#     }
#     if(input$plot=="box"||input$plot=="den"){
#     testFactor <- function(x){
#       f <- colnames(x)[sapply(x,is.factor)]
#       i <- colnames(x)[sapply(x, is.integer)]
#       c <- colnames(x)[sapply(x, is.character)]
#       return(c(f, i, c))
#     }
#     updateSelectInput(session,"x", label="Grouping Variable", choices=testFactor(theData()))
#     updateSelectInput(session,"y", label="Response Variable", selected=input$y)
#     }
#     if(input$plot=="qq"){
#       updateSelectInput(session,"x", label="Sample Variable", choices=qvars)
#     }
#   })

  filteredData <-reactive({
    data<-isolate(theData())
    if(input$x=="x"&&input$y=="y"&&input$x2=="x2"){
      if(is.null(data)){
        data<-data.frame(x = rep(0, 10), y = rep(0, 10))
      }
    }
    if(input$x2!="x2"&&input$plot=="box"||input$plot=="den")
    {
      data <- data[,c(input$x2,input$y)]
    }
    else{
      data <- data[,c(input$x,input$y)]
    }
    names(data)<-c("x","y")
    data.frame(data)
  })
  
  lineupPlot <- reactive({
    n <- input$num
    qq <- data.frame(x=sort(filteredData()$x), norm=sort(rnorm(filteredData()$x)))
    model <- lm(y~x, data=filteredData())
    resid.df <- data.frame(filteredData(), .resid=residuals(model),#residual plot df
                           .fitted=fitted(model))
    switch(input$plot,
           scatter= ggplot(filteredData(), aes(x, y)) %+% 
             lineup(null_permute("x"), filteredData(), n=n, pos=sample(n, 1)) 
           + geom_point() + facet_wrap(~.sample),
           scatterSmooth = ggplot(filteredData(), aes(x, y)) %+% 
            lineup(null_permute('x'), filteredData(), n=n, pos=sample(n,1)) +  geom_point() +
             geom_smooth(method="loess", se=FALSE) + facet_wrap(~ .sample),
           box=ggplot(filteredData(), aes(x, y)) %+% lineup(null_permute("x"),
            filteredData(), n=n, pos=sample(n,1))  + geom_boxplot()+ facet_wrap(~.sample),
           den=ggplot(filteredData(), aes(x=y, colour=x, group=x)) %+% lineup(null_permute("x"),
               filteredData(), n=n, pos=sample(n,1))  + geom_density(aes(fill=x), alpha=0.2) 
           + facet_wrap(~.sample),
           qq = ggplot(qq, aes(x=norm, y=x)) %+% lineup(null_permute('x'), qq, n=n, pos=sample(n,1))
              + geom_point()  + facet_wrap(~ .sample),
           resid=ggplot(resid.df, aes(x=x, y=.resid)) %+%
             lineup(null_lm(y~x, method='boot'), n=n, pos=sample(n,1), resid.df) +geom_point()
             + facet_wrap(~.sample)
    )
  })
  
  output$lineup <- renderPlot({
 lineupPlot()
  })
  
  output$plotPos <- renderText({
    paste("The true data is in plot ",attr(lineupPlot()$data, "pos"),".", sep="")
  })
})