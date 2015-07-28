library(Lock5Data)
library(shinyjs)
library(ggplot2)
library(stats)
library(car)
library(nullabor)
library(dplyr)
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
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  shinyjs::onclick("trueData",
                   shinyjs::toggle(id = "plotPos", anim = TRUE))
  
  shiny::observe({
    toggle(id = "warning", condition = input$x==input$y && input$plot!="qq")
    })
  
  shiny::observe({
    toggle(id = "warning2", condition = input$x2==input$y2)
  })
  
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
  if(input$plot!="scatter"){
    updateSelectInput(session, "x", selected=isolate(input$x))
    updateSelectInput(session, "y",selected=isolate(input$y))
  }
  })

  filteredData <-reactive({
    data<-isolate(theData())
    if(input$x=="x"&&input$y=="y"&&input$x2=="x2"&&input$y2=="y2"){
      if(is.null(data)){
        data<-data.frame(x = rep(0, 10), y = rep(0, 10))
      }
    }
    if(input$x2!="x2"&&input$y2!="y2"&&input$plot=="box"||input$plot=="den")
    {
      data <- data[,c(input$x2,input$y2)]
    }
    else{
      data <- data[,c(input$x,input$y)]
    }
    names(data)<-c("x","y")
    data.frame(data)
  })

  qqPos <- reactive({
    n <- input$num
    sample(n,1)
  })

  qqPlot <- reactive({
    r <- qqPos()
    xnorm <- rnorm(filteredData()$x, mean=mean(filteredData()$x), sd=sd(filteredData()$x))
    qqLineup <- function(y){
      y <- rnorm(filteredData()$x, mean=mean(filteredData()$x), sd=sd(filteredData()$x))
      data.frame(qqnorm(y, plot.it=FALSE))
    }
    w <- nrow(filteredData())
    samples <- data.frame(plyr::rdply(n, qqLineup(xnorm)))
    qq.df <- data.frame(.n=rep(as.numeric(r), w), x=sort(qqLineup(xnorm)$x), y=sort(qqLineup(xnorm)$y))
    startrow <- function(x){w*(x-1)} #replace one of the samples with the actual data
    nextrow <- function(x){(w*(x))+1}
    endrow <- function(x){w*x-1}
    new.df <- data.frame(rbind(samples[1:startrow(r),], qq.df, samples[nextrow(r):endrow(n),]))
    qqlineInfo <- function(x){
      yp <- quantile(x, c(0.25, 0.75))
      theory <- qnorm(p = c(0.25, 0.75))
      slope <- diff(yp)/diff(theory)
      intercept <- yp[1L] - slope * theory[1L]
      return(c(intercept = as.numeric(intercept), slope = as.numeric(slope)))
    }
    ggplot(new.df, aes(x=x, y=y)) + geom_point() + 
      geom_abline(slope=qqlineInfo(xnorm)[2], intercept=qqlineInfo(xnorm)[1]) +facet_wrap(~.n)
  })
  
  lineupPlot <- reactive({
    n <- input$num
    model <- lm(y~x, data=filteredData())
    resid.df <- data.frame(x=filteredData()$x, y=filteredData()$y,
                          .resid=residuals(model), .fitted=fitted(model))
    switch(input$plot,
           scatter= ggplot(filteredData(), aes(x, y)) %+% 
             lineup(null_permute("x"), filteredData(), n=n, pos=sample(n, 1)) 
           + geom_point() + facet_wrap(~.sample),
           scatterSmooth = ggplot(filteredData(), aes(x, y)) %+% 
            lineup(null_permute('x'), filteredData(), n=n, pos=sample(n,1)) +  geom_point() +
             geom_smooth(method="loess", se=FALSE) + facet_wrap(~ .sample),
           box=ggplot(filteredData(), aes(x=x, y=y)) %+% lineup(null_permute("x"),
            filteredData(), n=n, pos=sample(n,1))  + geom_boxplot()+ facet_wrap(~.sample),
           den=ggplot(filteredData(), aes(x=y, colour=x, group=x)) %+% lineup(null_permute("x"),
               filteredData(), n=n, pos=sample(n,1))  + geom_density(aes(fill=x), alpha=0.2) 
           + facet_wrap(~.sample),
           resid=ggplot(resid.df, aes(x=x, y=.resid)) %+%
             lineup(null_lm(y~x, method='boot'), n=n, pos=sample(n,1), resid.df) +geom_point()
             + facet_wrap(~.sample)
    )
  })
  
  output$lineup <- renderPlot({
    if(input$plot!="qq"){
    lineupPlot()
    }else{
      qqPlot()
    }
  })
  
  output$plotPos <- renderText({
    if(input$plot!="qq"){
    paste("The true data is in plot ",attr(lineupPlot()$data, "pos"),".", sep="")
    }else{
      paste("The true data is in plot ",qqPos(),".", sep="")
    }
  })
})